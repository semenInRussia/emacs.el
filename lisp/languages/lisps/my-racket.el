;;; my-racket.el --- My Configuration For The Lanugage `racket'

;; Copyright (C) 2022 Semen Khramtsov

;; Author: Semen Khramtsov <hrams205@gmail.com>
;; Version: 0.1
;; URL: https://github.com/semenInRussia/emacs.el

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; My Configuration for the Lanugage `racket'

;;; Code:

(require 'my-leaf)
(require 'my-lib)
(require 'dash)
(require 'smartparens)

(declare-function my-autoformat-bind-for-major-mode "my-autoformat")


(leaf racket-mode
  :ensure (racket-mode :repo "greghendershott/racket-mode" :host github)
  :defvar (my-racket-meta-return-functions
           racket-xp-mode-hook
           my-racket-meta-return-cond-clauses-expression-names)
  :defun ((my-racket-meta-return-contracted .
                                            (my-racket
                                             my-racket-meta-return-test-case
                                             my-racket-meta-return-let)))
  :bind (:racket-mode-map
         ("M-RET" . 'my-racket-meta-return))
  :hook ((racket-mode-hook . racket-xp-mode)
         ;; `flycheck' is enough slow plus `racket-xp-mode' highlight
         ;; errors too, so i disable `flycheck' for Racket
         (racket-mode-hook . turn-off-flycheck)
         ;; enable structured editing for the `racket-mode'
         (racket-mode-hook . paxedit-mode))
  :custom (racket-xp-mode-hook . nil)
  :config                                      ;nofmt
  (remove-hook 'racket-mode-hook 'racket-mode) ;fix a bug

  (defcustom my-racket-meta-return-functions nil
    "List of functions for M-ret in racket.

Each function should return t, if it should be called and should stop next
calls of functions."
    :type '(repeat function)
    :group 'my)

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
               #'my-racket-meta-return-contracted))

(leaf scribble-mode
  :ensure (scribble-mode :repo "emacs-pe/scribble-mode" :host github)
  :config
  (add-hook
   'scribble-mode-hook
   (defun my-autoformat-scribble ()
     "Define `my-autoformat' things for `scribble-mode'."
     (require 'my-autoformat)
     (my-autoformat-bind-for-major-mode 'scribble-mode
                                        'my-autoformat-sentence-capitalization))))

(provide 'my-racket)
;;; my-racket.el ends here
