;;; my-lisp.el --- my-lisp

;; Copyright (C) 2022 Semen Khramtsov

;;; Commentary:

;;; Code:

(require 'my-leaf)
(require 'dash)
(require 'my-lib)

(declare-function meow-insert "meow-commands.el")


(defun my-lisp-sexp-whole-line-p ()
  "Return t, when the Lisp sexp at the point being at whole of line."
  (interactive "P")
  (-let
      (((beg . end)
        (paxedit-sexp-region)))
    (and
     (= beg (save-excursion (beginning-of-line-text) (point)))
     (= end (pos-eol)))))

(defun my-paxedit-transpose-forward ()
  (interactive)
  (call-interactively #'paxedit-transpose-forward)
  (repeat-at-last-keystroke))

(defun my-paxedit-transpose-backward ()
  (interactive)
  (call-interactively #'paxedit-transpose-backward)
  (repeat-at-last-keystroke))

(leaf paxedit
  :ensure (paxedit :repo "promethial/paxedit" :host github)
  :defun (paxedit-delete
          paxedit-sexp-region
          paxedit-transpose-backward
          paxedit-transpose-forward)
  :bind ((:paxedit-mode-map
          ("C-c C-t" . 'my-paxedit-transpose-forward)
          ("C-c C-u C-t" . 'my-paxedit-transpose-backward)
          ("C-c C-w" . 'paxedit-kill)
          ("C-c C-;" . 'my-paxedit-comment)
          ("C-c C-d" . 'paxedit-symbol-kill)
          ("C-c C-q" . 'paxedit-compress)
          ("C-c C-k" . 'paxedit-delete-whitespace)
          ("C-c C-y" . 'my-paxedit-duplicate)))
  :hook ((emacs-lisp-mode-hook . paxedit-mode)
         (racket-mode-hook . paxedit-mode))
  :config                               ;nofmt
  (defun my-paxedit-comment ()
    "Comment the Lisp expression at the cursor."
    (interactive)
    (-let
        (((beg . end)
          (paxedit-sexp-region)))
      (comment-region beg end)))

  (defun my-paxedit-change ()
    "Kill the Lisp expression at the cursor and activate insert mode."
    (interactive)
    (paxedit-delete)
    (meow-insert))

  (defun my-paxedit-duplicate ()
    "Make copy of the Lisp expression at the cursor."
    (interactive)
    (let* ((reg (paxedit-sexp-region))
           (beg (car reg))
           (end (cdr reg))
           (sexp (buffer-substring beg end))
           (sep (if (my-lisp-sexp-whole-line-p) "\n" " ")))
      (goto-char end)
      (insert sep sexp))))

(leaf lisp-mode
  :custom (lisp-body-indent . 2))

(provide 'my-lisp)
;;; my-lisp.el ends here
