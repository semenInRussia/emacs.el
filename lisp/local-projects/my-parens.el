;;; my-parens.el --- Library to manipulate with parenthesis -*- lexical-binding: t -*-
;;; Commentary:
;; Library to manipulate with parenthesis.

;;; Code:
(require 'dash)
(require 'just)

(declare-function sp-get-sexp "smarparens")
(declare-function sp-get "smarparens")
(declare-function sp-get-enclosing-sexp "smarparens")

;;;###autoload
(defun my-goto-lisp-sexp-begin (start-name)
  "Go to backward beginning of Lisp sexp which start with START-NAME."
  (--when-let
      (search-backward-regexp
       (rx "(" (zero-or-more whitespace) (regexp start-name))
       nil t)
    (forward-char)
    it))

;;;###autoload
(defun my-goto-lisp-sexp-end (start-name)
  "Go to end of the backward Lisp sexp which start with START-NAME.

End of Lisp sexp is point before the last closed parenthesis"
  (my-goto-lisp-sexp-begin start-name)
  (sp-get (sp-get-enclosing-sexp) (goto-char :end-in)))

;;;###autoload
(defun my-mark-lisp-sexp-inner (start-name)
  "Mark the inner of the Lisp sexp which start with function START-NAME."
  (my-goto-lisp-sexp-begin start-name)
  (forward-char -1)
  (sp-get (sp-get-sexp) (just-mark-region :beg-in :end-in)))

;;;###autoload
(defun my-in-lisp-sexp-p (start-name &optional pt)
  "Get t, When cursor at PT placed in Lisp sexp which start with START-NAME."
  (save-excursion
    (if pt (goto-char pt) (setq pt (point)))
    (and (my-goto-lisp-sexp-begin start-name)
         (sp-get (sp-get-enclosing-sexp) (< :beg pt :end)))))

(provide 'my-parens)
;;; my-parens.el ends here
