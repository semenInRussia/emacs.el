;;; init.el --- Initialize emacs lisp code for my emacs -*- lexical-binding: t; -*-

;; Copyright (C) 2023

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

;; Initialize Elisp code for my Emacs.

;;; Code:

(require 'cl-lib)

(add-to-list 'load-path
             (locate-user-emacs-file "lisp/local-projects"))

(setq custom-file (expand-file-name "custom.el" user-emacs-directory)
      inhibit-startup-screen t)

(when (file-exists-p custom-file)
  (load custom-file))

(add-to-list 'load-path "~/.emacs.d/lisp")

(load "~/.emacs.d/lisp/local-projects/my-autoload.el")

(defvar my-modules-order
  (list
   "packagement/my-straight.el"
   "packagement/my-leaf.el"
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

(message "Good luck mam!")

(defvar my-modules-files-ignore-regexps
  '("/local-projects/" "/test/" "/features/" ".*-step\\.el" "/site-lisp/")
  "List of the regexps that indicates that a file to load shouldn't be loaded.")

(defun my-file-igored-as-module-p (filename)
  "Return non-nil if a module at FILENAME can't be a configuration module."
  (cl-some
   (lambda (regexp) (string-match-p regexp filename))
   my-modules-files-ignore-regexps))

(defvar my-modules-files
  (cl-remove-if
   #'my-file-igored-as-module-p
   (directory-files-recursively "~/.emacs.d/lisp" ".el$" nil)))

(let ((dirs (directory-files-recursively "~/.emacs.d/lisp" ".*" t)))
  (while dirs
    (when (file-directory-p (car dirs))
      (add-to-list 'load-path (car dirs)))
    (setq dirs (cdr dirs))))

(defvar my-load-modules-all (length my-modules-files))

(defun my-require-or-debug-file (filename)
  "Require the module in FILENAME, if catch errors, debug it."
  (my-require-or-debug (intern (file-name-base filename))))

(defun my-require-or-debug (module)
  "Require MODULE, if has any errors, then debug that."
  (unless (featurep module)
    (let ((start-time (current-time)))
      (or (ignore-errors (require module nil t))
          (lwarn "startup" :error "I can't load the module: %s" module))
      (message "`%s' module took %ssec"
               module
               (float-time (time-since start-time))))))

(defvar my-config-modules-prefix "~/.emacs.d/lisp/")

(defmacro my-extend (var lst)
  "Extend a variable called VAR with type list with LST.
The same to
\(setq var (append var lst))"
  `(setq ,var (append ,var ,lst)))

(defmacro my-remove-from (var elt)
  "Remove an ELT from the list at VAR.
The same to
\(setq var (remove elt var))"
  `(setq ,var (remove ,elt ,var)))

(defmacro my-mapc (lst elt &rest body)
  "Evaluate a BODY with variable called ELT setted to element of a LST."
  `(cl-do
       ((lst (cdr ,lst) (cdr lst))
        (,elt
         (car ,lst)
         (car lst)))
       ((null ,elt))
     ,@body))

(defun my-load-all-config-modules ()
  "Load all configuration modules."
  (cl-do*
      ((_ nil)
       (sketch-order (cdr my-modules-order) (cdr sketch-order))
       (it
        (concat my-config-modules-prefix (car sketch-order))
        (and
         sketch-order
         (concat my-config-modules-prefix (car sketch-order)))))
      ((null it))
    (if (file-directory-p it)
        (my-mapc my-modules-files file
                 (and
                  (string-prefix-p it file)
                  (not (string-equal it file))
                  (my-remove-from my-modules-files it)
                  (my-require-or-debug-file file)))
      (my-require-or-debug-file it)
      (my-remove-from my-modules-files it)))
  (my-mapc my-modules-files file (my-require-or-debug-file file)))

(my-load-all-config-modules)
(my-load-all-config-modules)

(defgroup my nil "Group for all my config files." :group 'tools)

(provide 'init)
;;; init.el ends here
