;;; my-elisp-class-fields.el --- A package to help developers add a field to `defclass' instructions -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2025 semenInRussia

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
