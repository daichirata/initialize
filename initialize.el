;;; initialize.el - Best way to load multiple initialize settings of your emacs.

;; Copyright (C) 2012 Daichi Hirata

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;; Author: Daichi Hirata <daichi.hirat@gmail.com>

;; Refs: http://coderepos.org/share/browser/lang/elisp/init-loader/init-loader.el

;;; Code:
(eval-when-compile (require 'cl))
(require 'dired)
(require 'benchmark)

(defgroup initialize nil
  "initialize"
  :group 'initialize)

(defcustom initialize-dir "~/.emacs.d/init"
  "init directory"
  :type 'directory
  :group 'initialize)

(defcustom initialize-pack-dir  "~/.emacs.d/init"
  ""
  :type 'directory
  :group 'initialize)

(defcustom initialize-initfuncs "~/.emacs.d/init/initfuncs.el"
  ""
  :type 'string
  :group 'initialize)

(defcustom initialize-pack-benchmark t
  ""
  :type 'boolean
  :group 'initialize)

(defcustom initialize-result-buffer
  "*initialize*"
  "Name of buffer for initialize"
  :type 'string
  :group 'initialize)

(defcustom initialize-slow-threshold 0.1
  ""
  :type 'integer
  :group 'initialize)

(defvar init/success  nil)
(defvar init/fail     nil)
(defvar init/slow     nil)
(defvar init/cocoa-p  (eq window-system 'ns))
(defvar init/carbon-p (eq window-system 'mac))
(defvar init/mac-p    (eq system-type 'darwin))
(defvar init/linux-p  (eq system-type 'gnu/linux))
(defvar init/cygwin-p (eq system-type 'cygwin))
(defvar init/win-nt-p (eq system-type 'windows-nt))
(defvar init/meadow-p (featurep 'meadow))
(defvar init/win-p    (or init/cygwin-p init/win-nt-p init/meadow-p))
(defvar init/version-regexp
  (concat "^" (regexp-quote (concat emacs-version ".")) "elc?$"))
(defvar init/major-version-regexp
  (concat "^" (regexp-quote (concat (int-to-string emacs-major-version) ".")) "elc?$"))
(defvar init/minor-version-regexp
  (concat "^" (regexp-quote (concat (int-to-string emacs-major-version) "."
                                    (int-to-string emacs-minor-version) ".")) "elc?$"))

(defvar init/pack-benchmark-format
";;
;; packing file: %s
;;
(let ((filename \"%s\"))
   (condition-case err
      (let ((time (benchmark-run ((lambda ()
;; body
%s
;; end body
)))))
        (init/success filename time))
     (error (init/fail filename (error-message-string err)))))\n\n\n")

;;         (message \"%%s\" time)

(define-minor-mode initialize-auto-pack-mode
  "Toggle pack on save for init file."
  :lighter " AP"
  :group 'initialize
  (if initialize-auto-pack-mode
      (add-hook 'after-save-hook #'initialize-pack-init-files nil t)
    (remove-hook 'after-save-hook #'initialize-pack-init-files t)))

(defun enable-initialize-auto-pack-mode ()
  (add-hook 'emacs-lisp-mode-hook #'--enable-initialize-auto-pack-mode))

(defun --enable-initialize-auto-pack-mode ()
  (when (init/init-dir-p)
    (initialize-auto-pack-mode)))

;;;###autoload
(defun* initialize (&key (dir nil) (file nil) (showlog t))
  "Start initialization process."
  (when dir
    (setq initialize-dir dir))
  (if file
      (init/load-file file)
    (init/load-dir dir))
  (when showlog
    (add-hook 'after-init-hook 'init/result)))

;;;###autoload
(defun initialize-load-pack-file ()
  (initialize :file (init/pack-filename)))

(defun initialize-pack-init-files ()
  (interactive)
  (when (file-exists-p (init/pack-filename))
    (dired-delete-file (init/pack-filename) 'always))
  (when (file-exists-p initialize-initfuncs)
    (init/pack-file (expand-file-name initialize-initfuncs)))
  (init/pack-dir initialize-dir))

(defun init/init-dir-p ()
  (string-match (file-truename initialize-dir)
                (file-truename default-directory)))

(defun init/pack-filename ()
  (expand-file-name (concat (file-name-as-directory initialize-pack-dir)
                            (format "init-pack-%s.el" (system-name)))))

(defun init/directory-files (dir)
  (loop for file in (directory-files dir t "[^.]")
        unless (string-match "\\.elc$" file)
        collect file))

(defun init/load-dir (dir)
  (init/only-if-match-filename dir 'init/load-file))

(defun init/pack-dir (dir)
  (init/only-if-match-filename dir 'init/pack-file))

(defun init/load-file (file)
  (let* ((base  (file-name-sans-extension file))
         (el    (concat base ".el"))
         (elc   (concat base ".elc"))
         (loadf (if (file-exists-p elc) elc el)))
    (condition-case err
      (let ((time (benchmark-run (load loadf))))
        (init/success loadf time))
      (error (init/fail loadf (error-message-string err))))))

(defun init/pack-file (filename)
  (let ((body (with-temp-buffer (insert-file-contents filename)
                                (buffer-string))))
    (append-to-file
     (if initialize-pack-benchmark
         (format init/pack-benchmark-format filename filename body)
       (concat body "\n\n"))
     nil (concat (file-name-as-directory initialize-pack-dir)
                 (format "init-pack-%s.el" (system-name))))))

(defun init/only-if-match-filename (dir func)
  (loop for file in (init/directory-files dir)
        for filename = (file-name-nondirectory file)
        if (file-directory-p file)
        do (init/only-if-match-filename file func)
        else
        if (init/match-file-p filename)
        do (funcall func file)))

(defun init/match-file-p (filename)
  ;; window system
  (if window-system
      (or (string-match "^[0-9]\\{2\\}[-_]." filename)
          (and init/cocoa-p
               (string-match "^cocoa[-_][0-9]\\{2\\}[-_]."   filename))
          (and init/carbon-p
               (string-match "^carbon[-_][0-9]\\{2\\}[-_]."  filename))
          (and init/mac-p
               (string-match "^mac[-_][0-9]\\{2\\}[-_]."     filename))
          (and init/linux-p
               (string-match "^linux[-_][0-9]\\{2\\}[-_]."   filename))
          (and init/cygwin-p
               (string-match "^cygwin[-_][0-9]\\{2\\}[-_]."  filename))
          (and init/win-nt-p
               (string-match "^win-nt[-_][0-9]\\{2\\}[-_]."  filename))
          (and init/meadow-p
               (string-match "^meadow[-_][0-9]\\{2\\}[-_]."  filename))
          (and init/win-p
               (string-match "^windows[-_][0-9]\\{2\\}[-_]." filename))
          ;; version
          (string-match init/version-regexp filename)
          ;; major-version
          (string-match init/major-version-regexp filename)
          ;; minor-version
          (string-match init/minor-version-regexp filename)
          ;; system
          (string-match (concat "^" (regexp-quote system-name)) filename))
    ;; no window system
    (string-match "^nw[-_][0-9]\\{2\\}[-_]." filename)))

(defun init/success (file time)
  (let* ((time  (format "%f" (car time)))
         (stime (truncate-string-to-width time 6 0))
         (lgh   (length stime))
         (atime (if (< lgh 6)
                    (concat stime (make-string (- 6 lgh) ?0))
                  stime))
         (msg   (format " %s.sec %s" atime file)))
    (push msg init/success)
    (when (< initialize-slow-threshold (string-to-number time))
      (push msg init/slow))))

(defun init/fail (file err)
  (let ((msg (format " %s %s" file err)))
    (push msg init/fail)))

(defun init/show (var)
  (mapconcat 'identity (reverse var) "\n"))

(defun init/result ()
  (let ((buf (get-buffer-create initialize-result-buffer)))
    (with-current-buffer buf
      (erase-buffer)
      (insert "Time:\n " (emacs-init-time) "\n\n")
      (when init/fail
        (insert "Fail:\n" (init/show init/fail) "\n\n"))
      (when init/success
        (insert "Success:\n" (init/show init/success)))
      (when init/slow
        (init/put-text-property buf))
      (goto-char (point-min)))
    (setq init/success nil init/fail nil init/slow nil)
    (switch-to-buffer buf)))

(defun init/put-text-property (buf)
  (with-current-buffer buf
    (loop for txt in init/slow
          for start = (search-backward txt nil t)
          for end   = (search-forward txt nil t)
          if (and start end)
          do (goto-char (point-min))
          (put-text-property start end 'face font-lock-keyword-face))))

(defun init/generate-config (type)
  (interactive "sType: ")
  (let* ((type (cond ((string= type "system") system-name)
                     ((string= type "version") emacs-version)
                     ((string= type "major-version") (int-to-string emacs-major-version))
                     ((string= type "minor-version") (concat (int-to-string emacs-major-version) "."
                                                             (int-to-string emacs-minor-version)))
                     (t (error (concat type "is not exist")))))
         (local-file (expand-file-name
                      (concat user-emacs-directory type ".el"))))
    (append-to-file (format ";; Write to %s specific configuration here." type) nil local-file)))

(provide 'initialize)
