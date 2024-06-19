;;; my-racket-funcs.el --- Some functions for racket language -*- lexical-binding: t -*-

;;; Code:
(require 'my-parens)
(require 'smartparens)

(remove-hook 'racket-mode-hook 'racket-mode) ;fix a bug

(defcustom my-racket-meta-return-functions nil
  "List of functions for M-ret in racket.

Each function should return t, if it should be called and should stop next
calls of functions."
  :type '(repeat function)
  :group 'my)

;;;###autoload
(defun my-racket-meta-return ()
  "Try use one of M-ret functions for racket.
Depends on `my-racket-meta-return-functions'."
  (interactive)
  (unless (-find #'funcall my-racket-meta-return-functions)
    (message "Sorry, function not found!")))

(defun my-racket-meta-return-let ()
  "Add a binding to the let expression of the Racket.
One of `my-racket-meta-return-functions'"
  (when (my-in-lisp-sexp-p "let")
    (my-goto-lisp-sexp-begin "let")
    (search-forward "(." nil t)
    (sp-get
        (sp-get-sexp)
      (goto-char :end-in)
      (newline-and-indent)
      (insert "[]")
      (forward-char -1)
      t)))

(add-to-list 'my-racket-meta-return-functions #'my-racket-meta-return-let)

(defun my-racket-meta-return-test-case ()
  "Add a test case to current test module in racket.
One of `my-racket-meta-return-functions'"
  (when (my-in-lisp-sexp-p "module\+\\W*test")
    (my-goto-lisp-sexp-begin "module\+\\W*test")
    (forward-char -1)
    (sp-get (sp-get-sexp) (goto-char :end-in))
    (newline-and-indent)
    (insert "(check-equal? )")
    (forward-char -1)
    t))

(add-to-list 'my-racket-meta-return-functions
             #'my-racket-meta-return-test-case)

(defcustom my-racket-meta-return-cond-clauses-expression-names
  '("cond" "match" "define/match")
  "List of the racket expressions names in which should work `M-ret'."
  :type '(repeat string)
  :group 'my)

(defun my-racket-meta-return-cond-clauses ()
  "Add new clause to racket expression which has syntax like on `cond'.

One of `my-racket-meta-return-functions'.

List of racket expressions in which this function should work:

- `cond'
- `match'
- `define/match'"
  (interactive)
  (--when-let
      (-find
       #'my-in-lisp-sexp-p
       my-racket-meta-return-cond-clauses-expression-names)
    (my-goto-lisp-sexp-begin it)
    (forward-char -1)
    (forward-sexp)
    (forward-char -1)
    (newline-and-indent)
    (insert "[]")
    (forward-char -1)
    t))

(add-to-list 'my-racket-meta-return-functions
             'my-racket-meta-return-cond-clauses)

(defun my-racket-meta-return-contracted ()
  "Add new argument form to the expression of the Racket `contracted'."
  (interactive)
  (when (my-in-lisp-sexp-p "contracted")
    (my-goto-lisp-sexp-end "contracted")
    (newline)
    (insert "[]")
    (my-mark-lisp-sexp-inner "contracted")
    (align-regexp
     (region-beginning)
     (region-end)
     "\\[[^ ]+ *\\( \\)[^ ]")
    (beginning-of-line-text)
    (forward-char 1)
    t))

(add-to-list 'my-racket-meta-return-functions
             #'my-racket-meta-return-contracted)

(provide 'my-racket-funcs)
;;; my-racket-funcs.el ends here
