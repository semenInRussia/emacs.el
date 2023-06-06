;;; my-elisp-embrace.el --- Customization of `embrace' specially for emacs-lisp -*- lexical-binding: t; -*-

;; Copyright (C) 2023 semenInRussia

;; Author: semenInRussia <hrams205@gmail.com>
;; Version: 0.0.1
;; Package-Requires: ((dash "2.18.0") (s "1.12.0"))

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

;; Customization of `embrace' specially for emacs-lisp.

;;; Code:
(require 'my-lib)

(require 'dash)
(require 's)
(require 'embrace)

;;;###autoload
(defun my-embrace-emacs-lisp-mode-hook ()
  "Add some parens for the Emacs-Lisp embrace."
  (setq-local embrace-show-help-p nil)
  (embrace-add-pair-regexp
   ?f
   "(\\(\\w\\|\\)* "
   ")"
   'my-embrace-emacs-lisp-with-function-call
   (embrace-build-help "(name " ")"))
  (embrace-add-pair-regexp
   ?d
   "(defun \\(\\w\\|-\\)+ (.*)
  \\( *\".*?\"\\)?
  \\((interactive \".*?\")\\)?"
   ")"
   'my-embrace-emacs-lisp-with-defun
   (embrace-build-help "(defun " ")"))
  (embrace-add-pair-regexp
   ?c
   "(defcustom \\(\\w\\|-\\)*[\n ]"
   "\".*?\"[\n ]*\\(:\\(type\\|group\\) .*?)\\)*"
   'my-embrace-emacs-lisp-with-defcustom
   (embrace-build-help "(defcustom " ")")))

(defun my-embrace-emacs-lisp-with-function-call ()
  "Return open and close pairs for the Elisp function call."
  (cons
   (s-concat "(" (read-string "Name of the function: ") " ")
   ")"))

(defun my-embrace-emacs-lisp-with-defun ()
  "Return open and close pairs for the Elisp defun paren."
  (cons
   (s-concat
    "(defun "
    (read-string "Name of the function, please: ")
    " "
    "("
    (read-string "Args, please: ")
    ")"
    "\n"
    "\""
    (read-string "Docstring, please: ")
    "\""
    "\n"
    (--if-let
        (my-read-string-or-nil "Ineractive preamble please: ")
        (s-concat "\"" it "\"")
      ""))
   ")"))

(defun my-embrace-emacs-lisp-with-defcustom ()
  "Return open and close pairs for the Elisp defcustom paren."
  (cons
   (s-concat
    "(defcustom "
    (read-string "Name of the variable, please: ")
    "\n")
   (s-concat
    "\n"
    "\""
    (read-string "Docstring, please: ")
    "\""
    "\n"
    (--if-let
        (my-read-string-or-nil "Group: ")
        (s-concat ":group '" it "\n")
      "")
    (--if-let
        (my-read-string-or-nil "Type: ")
        (s-concat ":type '" it)
      "")
    ")")))

(provide 'my-elisp-embrace)
;;; my-elisp-embrace.el ends here
