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

(defcustom initialize-initfuncs "~/.emacs.d/init/initfuncs.el"
  ""
  :type 'string
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

(defcustom initialize-pack-benchmark t
  ""
  :type 'boolean
  :group 'initialize)

(defvar initialize/success  nil)
(defvar initialize/fail     nil)
(defvar initialize/slow     nil)
(defvar initialize/cocoa-p  (eq window-system 'ns))
(defvar initialize/carbon-p (eq window-system 'mac))
(defvar initialize/mac-p    (eq system-type 'darwin))
(defvar initialize/linux-p  (eq system-type 'gnu/linux))
(defvar initialize/cygwin-p (eq system-type 'cygwin))
(defvar initialize/win-nt-p (eq system-type 'windows-nt))
(defvar initialize/meadow-p (featurep 'meadow))
(defvar initialize/win-p    (or initialize/cygwin-p initialize/win-nt-p initialize/meadow-p))
(defvar initialize/version-regexp
  (concat "^" (regexp-quote (concat emacs-version ".")) "elc?$"))
(defvar initialize/major-version-regexp
  (concat "^" (regexp-quote (concat (int-to-string emacs-major-version) ".")) "elc?$"))
(defvar initialize/minor-version-regexp
  (concat "^" (regexp-quote (concat (int-to-string emacs-major-version) "."
                                    (int-to-string emacs-minor-version) ".")) "elc?$"))

(defvar initialize/pack-benchmark-format
";;
;; packing file: %s
;;
 (let ((filename \"%s\"))
   (condition-case err
      (let ((body (lambda ()
;; body

%s

;; end body
)))
        (initialize/success filename (benchmark-run (funcall body))))
     (error (initialize/fail filename (error-message-string err)))))

")

(define-minor-mode initialize-auto-pack-mode
  "Toggle pack on save for init file."
  :lighter " AP"
  :group 'initialize
  (if initialize-auto-pack-mode
      (add-hook 'after-save-hook #'initialize-pack nil t)
    (remove-hook 'after-save-hook #'initialize-pack t)))

(defun enable-initialize-auto-pack-mode ()
  (add-hook 'emacs-lisp-mode-hook #'--enable-initialize-auto-pack-mode))

(defun --enable-initialize-auto-pack-mode ()
  (when (initialize/init-dir-p)
    (initialize-auto-pack-mode)))

;;;###autoload
(defun* initialize (&key (dir nil) (file nil) (showlog t))
  "Start initialization process."
  (if (file-exists-p (initialize/packfile-name))
      (initialize/load-file (initialize/packfile-name))
    (when dir
      (setq initialize-dir dir))
    (if file
        (initialize/load-file file)
      (initialize/load-dir dir)))
  (when showlog
    (add-hook 'after-init-hook 'initialize/result)))

(defun initialize-pack ()
  (interactive)
  (when (file-exists-p (initialize/packfile-name))
    (dired-delete-file (initialize/packfile-name) 'always))
  (when (file-exists-p initialize-initfuncs)
    (initialize/pack-file (expand-file-name initialize-initfuncs)))
  (initialize/pack-dir initialize-dir))

(defun initialize/init-dir-p ()
  (string-match (file-truename initialize-dir)
                (file-truename default-directory)))

(defun initialize/packfile-name ()
  (expand-file-name (concat (file-name-as-directory initialize-dir)
                            (format "init-pack-%s.el" (system-name)))))

(defun initialize/directory-files (dir)
  (loop for file in (directory-files dir t "[^.]")
        unless (string-match "\\.elc$" file)
        collect file))

(defun initialize/load-dir (dir)
  (initialize/map dir 'initialize/load-file))

(defun initialize/pack-dir (dir)
  (initialize/map dir 'initialize/pack-file))

(defun initialize/map (dir func)
  (loop for file in (initialize/directory-files dir)
        for filename = (file-name-nondirectory file)
        if (file-directory-p file)
        do (initialize/map file func)
        else
        if (initialize/match-file-p filename)
        do (funcall func file)))

(defun initialize/load-file (file)
  (let* ((base  (file-name-sans-extension file))
         (el    (concat base ".el"))
         (elc   (concat base ".elc"))
         (loadf (if (file-exists-p elc) elc el)))
    (condition-case err
      (let ((time (benchmark-run (load loadf))))
        (initialize/success loadf time))
      (error (initialize/fail loadf (error-message-string err))))))

(defun initialize/pack-file (filename)
  (let ((body (with-temp-buffer (insert-file-contents filename)
                                (buffer-string))))
    (append-to-file
     (if initialize-pack-benchmark
         (format initialize/pack-benchmark-format filename filename body)
       (concat body "\n\n"))
     nil (concat (file-name-as-directory initialize-dir)
                 (format "init-pack-%s.el" (system-name))))))

(defun initialize/match-file-p (filename)
  ;; window system
  (if window-system
      (or (string-match "^[0-9]\\{2\\}[-_]." filename)
          (and initialize/cocoa-p
               (string-match "^cocoa[-_][0-9]\\{2\\}[-_]."   filename))
          (and initialize/carbon-p
               (string-match "^carbon[-_][0-9]\\{2\\}[-_]."  filename))
          (and initialize/mac-p
               (string-match "^mac[-_][0-9]\\{2\\}[-_]."     filename))
          (and initialize/linux-p
               (string-match "^linux[-_][0-9]\\{2\\}[-_]."   filename))
          (and initialize/cygwin-p
               (string-match "^cygwin[-_][0-9]\\{2\\}[-_]."  filename))
          (and initialize/win-nt-p
               (string-match "^win-nt[-_][0-9]\\{2\\}[-_]."  filename))
          (and initialize/meadow-p
               (string-match "^meadow[-_][0-9]\\{2\\}[-_]."  filename))
          (and initialize/win-p
               (string-match "^windows[-_][0-9]\\{2\\}[-_]." filename))
          ;; version
          (string-match initialize/version-regexp filename)
          ;; major-version
          (string-match initialize/major-version-regexp filename)
          ;; minor-version
          (string-match initialize/minor-version-regexp filename)
          ;; system
          (string-match (concat "^" (regexp-quote system-name)) filename))
    ;; no window system
    (string-match "^nw[-_][0-9]\\{2\\}[-_]." filename)))

(defun initialize/success (file time)
  (let* ((time  (format "%f" (car time)))
         (stime (truncate-string-to-width time 6 0))
         (lgh   (length stime))
         (atime (if (< lgh 6)
                    (concat stime (make-string (- 6 lgh) ?0))
                  stime))
         (msg   (format " %s.sec %s" atime file)))
    (push msg initialize/success)
    (when (< initialize-slow-threshold (string-to-number time))
      (push msg initialize/slow))))

(defun initialize/fail (file err)
  (let ((msg (format " %s %s" file err)))
    (push msg initialize/fail)))

(defun initialize/show (var)
  (mapconcat 'identity (reverse var) "\n"))

(defun initialize/result ()
  (let ((buf (get-buffer-create initialize-result-buffer)))
    (with-current-buffer buf
      (erase-buffer)
      (insert "Time:\n " (emacs-init-time) "\n\n")
      (when initialize/fail
        (insert "Fail:\n" (initialize/show initialize/fail) "\n\n"))
      (when initialize/success
        (insert "Success:\n" (initialize/show initialize/success)))
      (when initialize/slow
        (initialize/put-text-property buf))
      (goto-char (point-min)))
    (setq initialize/success nil initialize/fail nil initialize/slow nil)
    (switch-to-buffer buf)))

(defun initialize/put-text-property (buf)
  (with-current-buffer buf
    (loop for txt in initialize/slow
          for start = (search-backward txt nil t)
          for end   = (search-forward txt nil t)
          if (and start end)
          do (goto-char (point-min))
             (put-text-property start end 'face font-lock-keyword-face))))

(defun initialize-generate-config (type)
  (interactive "sType: ")
  (let ((type (cond ((string= type "system") system-name)
                    ((string= type "version") emacs-version)
                    ((string= type "major-version") (int-to-string emacs-major-version))
                    ((string= type "minor-version") (concat (int-to-string emacs-major-version) "."
                                                            (int-to-string emacs-minor-version)))
                    (t (error (concat type "is not exist"))))))
    (append-to-file
     (format ";; Write to %s specific configuration here." type) nil
     (expand-file-name
      (concat user-emacs-directory type ".el")))))

(provide 'initialize)
