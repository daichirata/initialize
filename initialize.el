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

;;; Commentary:
;;
;;   TODO

;;; Useage
;;
;;   TODO

;;; Code:
(eval-when-compile (require 'cl))
(require 'benchmark)

(defgroup initialize nil
  "initialize"
  :group 'initialize)

(defcustom initialize-dir "~/.emacs.d/init"
  "init directory"
  :type 'directory
  :group 'initialize)

(defcustom initialize-result-buffer
  "*initialize*"
  "Name of buffer for initialize"
  :type 'string
  :group 'initialize)

(defcustom initialize-slow-indication 0.1
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

(defun* initialize (&key (dir initialize-dir) (show t))
  (init/run dir)
  (when show
    (add-hook 'after-init-hook 'init/result)))

(defun init/run (dir)
  (let ((load-dir (init/directory-files dir)))
    (loop for x in load-dir
          for filename = (file-name-nondirectory x)
          if (file-directory-p x) do (init/run x)
          else do
          (when (init/resolve filename) (init/load x)))))

(defun init/directory-files (dir)
  (let ((files (directory-files dir t "[^.]")))
    (loop for f in files
          unless (string-match "\\.elc$" f)
          collect f)))

(defun init/load (file)
  (let* ((base  (file-name-sans-extension file))
         (el    (concat base ".el"))
         (elc   (concat base ".elc"))
         (loadf (if (file-exists-p elc) elc el)))
    (condition-case err
        (let ((time (benchmark-run (load loadf))))
          (init/success loadf time))
      (error (init/fail loadf (error-message-string err))))))

(defun init/resolve (filename)
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
          (string-match (concat "^" (regexp-quote emacs-version)) filename)
          ;; major-version
          (string-match (concat "^" (regexp-quote (int-to-string emacs-major-version))) filename)
          ;; system
          (string-match (concat "^" (regexp-quote system-name)) filename))
    ;; no window system
    (string-match "^nw[-_][0-9]\\{2\\}[-_]." filename)))

(defun init/success (file time)
  (let* ((time (car time))
         (stime (truncate-string-to-width
                 (number-to-string time) 6 0))
         (lgh (length stime))
         (atime (if (< lgh 6)
                    (concat stime (make-string (- 6 lgh) ?0))
                  stime))
         (msg (format " %s.sec %s" atime file)))
    (push msg init/success)
    (when (< initialize-slow-indication time)
      (push msg init/slow))))

(defun init/fail (file err)
  (let ((msg (format " %s %s" file err)))
    (push msg init/fail)))

(defun init/show (var)
  (mapconcat 'identity (reverse var) "\n"))

(defun init/result ()
  (interactive)
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
    (loop for txt in init/slow do
          (goto-char (point-min))
          (let ((end   (search-forward txt nil t))
                (start (search-backward txt nil t)))
            (when (and start end)
              (put-text-property
               start end 'face font-lock-keyword-face))))))

(defun init/generate-config (type)
  (interactive "sType: ")
  (let* ((type (cond ((string= type "system") system-name)
                     ((string= type "version") emacs-version)
                     ((string= type "major-version") (int-to-string emacs-major-version))
                     (t (error (concat type "is not exist")))))
         (local-file (expand-file-name
                      (concat user-emacs-directory type ".el"))))
    (append-to-file (format ";; Write to %s specific configuration here." type) nil local-file)))

(provide 'initialize)
