;;; my-elisp-class-fields.el --- A package to help developers add a field to `defclass' instructions -*- lexical-binding: t; -*-

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

;; A package to help developers add a field to `defclass' instructions.

;;; Code:

(require 'just)
(require 'smartparens)
(require 'yasnippet)

;;;###autoload
(defun my-elisp-new-field-of-class ()
  "Insert new field of Lisp class.
Only when in class defnition."
  (interactive)
  (when (my-elisp-in-defclass-p)
    (my-goto-fields-defclass-defnition)
    (unless (just-line-is-whitespaces-p) (newline-and-indent))
    (yas-expand-snippet
     (format
      "(${1:name} :initarg :$1 :accessor %s-$1)"
      (my-elisp-defclass-name)))))

(defun my-elisp-in-defclass-p (&optional pt)
  "Move to PT and return name of function/macros in which stay this sexp."
  (setq pt (or pt (point)))
  (save-excursion
    (goto-char pt)
    (when (my-goto-defclass-beg)
      (-when-let
          (sexp (sp-get-enclosing-sexp))
        (sp-get sexp (< :beg pt :end))))))

(defun my-goto-fields-defclass-defnition ()
  "Go to fields of `defclass' defnition."
  (interactive)
  (my-goto-defclass-beg)
  (sp-get
      (sp-get-enclosing-sexp)
    (let ((sexp (read (buffer-substring-no-properties :beg :end))))
      (if (length> sexp 3)
          (forward-sexp 4)
        (goto-char :end-in)
        (newline-and-indent)
        (insert "()"))
      (forward-char -1))))

(defun my-goto-defclass-beg ()
  "Goto backward defclass."
  (search-backward-regexp "(\\W*defclass" nil t)
  (skip-chars-forward "("))

(defun my-elisp-defclass-name ()
  "Return name of `defclass' defnition."
  (interactive)
  (save-excursion
    (my-goto-defclass-beg)
    (forward-sexp 1)
    (forward-char 1)
    (sexp-at-point)))

(provide 'my-elisp-class-fields)
;;; my-elisp-class-fields.el ends here
