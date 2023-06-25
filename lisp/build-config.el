;;; build-config.el --- Join all my config files into one init.el -*- lexical-binding: t; -*-

;; Copyright (C) 2023 semenInRussia

;; Author: semenInRussia <hrams205@gmail.com>
;; Version: 0.0.1

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Join all my config files into one init.el.  I use this script to make one
;; big init.el file which has more fast startup time.

;; `build-config' command joins all modules files from the .emacs.d/lisp into one
;; dist/my-modules.el and compile it.  When i start Emacs init.el files just
;; require my-modules.el, my-autoloads.el and custom.el which have already
;; byte-compiled (or even better NATIVE-compiled).  One big file instead of a lot
;; of small is better in load time, because every `require` statement also take
;; a bit of time.

;;; Code:

;; NOTE that here i don't use external libraries like `dash' or `s', because I sometimes
;; need to build configuration when old configuration was broken and `dash'/`s' haven't been loaded
(require 'cl-lib)

(defvar my-modules-el-file "~/.emacs.d/dist/my-modules.el")
(defvar my-config-modules-prefix "~/.emacs.d/lisp/")

(defvar my-modules-order
  (list
   "package-management/my-straight.el"
   "package-management/my-leaf.el"
   "package-management"
   "my-libs.el"
   "my-lib.el"
   "editing"
   "languages/lisps/my-lisp.el"
   "languages/my-autoformat.el"
   "languages"
   "env"
   "ui")
  "Names of the directories and files that define an order to load.")

(defvar my-modules-files-ignore-regexps
  '("/local-projects/" "/test/" "/features/" ".*-step\\.el" "/site-lisp/")
  "List of the regexps that indicates that a file to load shouldn't be loaded.")

(defun my-build-config ()
  "Build my config."
  (interactive)
  (my-join-modules-into-modules.el)
  (byte-compile-file my-modules-el-file))

(defun my-file-igored-as-module-p (filename)
  "Return non-nil if a module at FILENAME can't be a configuration module."
  (cl-some
   (lambda (regexp) (string-match-p regexp filename))
   my-modules-files-ignore-regexps))

(defmacro my-remove-from (var elt)
  "Remove an ELT from the list at VAR.
The same to
\(setq var (remove elt var))"
  `(setq ,var (remove ,elt ,var)))

(defun my-all-modules-files ()
  "Return list of all modules filenames using `my-modules-order'."
  (interactive "P")
  (let ((order (mapcar
                (lambda (it) (concat my-config-modules-prefix it))
                my-modules-order))
        order-item
        (files (cl-remove-if
                #'my-file-igored-as-module-p
                (directory-files-recursively "~/.emacs.d/lisp" ".el$" nil)))
        f
        sorted)
    (while order
      (setq order-item (car order))
      (setq order (cdr order))
      (cond
       ((file-directory-p order-item)
        (setq sorted
              (append sorted (cl-remove-if
                              (lambda (f) (member f sorted))
                              (directory-files-recursively order-item ".el$")))))
       (t
        (setq sorted (append sorted (list order-item))))))
    (while files
      (setq f (car files))
      (setq files (cdr files))
      (unless (member f sorted)
        (setq sorted (append sorted (list f)))))
    sorted))

(defvar my-config-modules-prefix "~/.emacs.d/lisp/")

(defun my-join-modules-into-modules.el ()
  "Join all configuration modules into one my-modules.el file."
  (my-join-modules my-modules-el-file))

(defun my-join-modules (dest)
  "Join all configuration modules into one file with DEST filename."
  (with-temp-buffer
    (mapc #'insert-file-contents-literally (nreverse (my-all-modules-files)))
    (goto-char (point-max))
    (replace-regexp-in-region "^(provide 'my-[a-zA-Z-]*?)" "\n"
                              (point-min)
                              (point-max))
    (replace-regexp-in-region "^(require 'my-[a-zA-Z-]*?)" "\n"
                              (point-min)
                              (point-max))
    (insert "\n(provide 'my-modules)")
    (delete-file dest)
    (write-region (point-min) (point-max) dest)))

(provide 'build-config)
;;; build-config.el ends here
