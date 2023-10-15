;;; my-latex-embrace.el --- Config of `embrace' for `LaTeX' -*- lexical-binding: t; -*-

;; Copyright (C) 2023 semenInRussia

;; Author: semenInRussia <hrams205@gmail.com>
;; Version: 0.0.1
;; Package-Requires: ((dash "2.18.0") (s "1.12.0"))

;;; Commentary:

;; Config of `embrace' for LaTeX.

;;; Code:

(require 'dash)
(require 's)

(require 'embrace)

(defvar cdlatex-math-modify-alist-default)
(defvar cdlatex-math-modify-alist)

(declare-function latex-complete-envnames "auctex.el")

(declare-function embrace-build-help "embrace.el")
(declare-function embrace-add-pair-regexp "embrace.el")



;;;###autoload
(defun my-embrace-LaTeX-mode-hook ()
  "My additional `embrace-LaTeX-mode-hook'."
  (interactive)
  (setq-local embrace-show-help-p nil)
  (cdlatex-mode 1)
  (--each
      (-concat cdlatex-math-modify-alist-default
               cdlatex-math-modify-alist)
    (my-embrace-add-paren-of-cdlatex-math it))
  (my-embrace-add-paren-latex-command ?a "answer")
  (embrace-add-pair-regexp ?\\
                           (rx "\\"
                               (1+ wordchar)
                               (* space)
                               (? "[" (*? any) "]" (* space))
                               "{")
                           "}"
                           'my-embrace-with-latex-command
                           (embrace-build-help "\\name{" "}"))
  (embrace-add-pair-regexp ?d
                           "\\\\left."
                           "\\\\right."
                           'my-embrace-with-latex-left-right
                           (embrace-build-help
                            "\\left(" "\\right)"))
  (embrace-add-pair-regexp
   ?e
   "\\\\begin{\\(.*?\\)}\\(\\[.*?\\]\\)*"
   "\\\\end{\\(.*?\\)}"
   'my-embrace-with-latex-env
   (embrace-build-help "\\begin{name}" "\\end{name}")
   t))

(defun my-embrace-add-paren-of-cdlatex-math (element)
  "Add an ELEMENT of the `cdlatex-math-modify-alist' to the `embrace' parens."
  (let* ((key (-first-item element))
         (cmd
          (s-chop-prefix
           "\\"
           (or (-third-item element) (-second-item element))))
         (type (-fourth-item element)))
    (if type
        (my-embrace-add-paren-latex-command key cmd)
      (my-embrace-add-paren-latex-style-command key cmd))))

(defun my-embrace-add-paren-latex-command (key name)
  "Add paren at KEY for the LaTeX command with NAME in `embrace'."
  (embrace-add-pair-regexp
   key
   (my-latex-command-left-paren-regexp name)
   "}"
   (-const (cons (my-latex-command-left-paren name) "}"))
   (embrace-build-help (my-latex-command-left-paren name) "}")))

(defun my-latex-command-left-paren (name)
  "Return paren right of the LaTeX command named NAME."
  (s-concat "\\" name "{"))

(defun my-latex-command-left-paren-regexp (name)
  (rx "\\"
      (literal name)
      (* space)
      (? "[" (*? any) "]" (* space))
      "{"))

(defun my-embrace-add-paren-latex-style-command (key name)
  "Add paren at KEY for the style LaTeX command with NAME in `embrace'."
  (embrace-add-pair-regexp key
                           (my-latex-style-command-left-paren-regexp name)
                           "}"
                           (-const
                            (cons
                             (my-latex-style-command-left-paren name)
                             "}"))
                           (embrace-build-help
                            (my-latex-style-command-left-paren name)
                            "}")))

(defun my-latex-style-command-left-paren (name)
  "Return paren right of the LaTeX command named NAME."
  (s-concat "{\\" name " "))

(defun my-latex-style-command-left-paren-regexp (name)
  (rx "{" (* space) "\\" (literal name) (* space)))

(defun my-embrace-with-latex-command ()
  "Return pair from the left and right pair for a LaTeX command."
  (let ((name (read-string "Name of a LaTeX command, please: ")))
    (cons (s-concat "\\" name "{") "}")))

(defun my-embrace-with-latex-left-right ()
  "Return pair from the left and right pair for the LaTeX command \\left."
  (cons
   (s-concat "\\left" (read-char "Left paren, please: "))
   (s-concat "\\right" (read-char "Right paren, please: "))))

(defun my-embrace-with-latex-env ()
  "Return pair from the left and right pair for the LaTeX command \\left."
  (let ((env
         (read-string "Name of the environment, please: "
                      (latex-complete-envnames))))
    (cons
     (s-concat "\\begin{" env "}")
     (s-concat "\\end{" env "}"))))

(provide 'my-latex-embrace)
;;; my-latex-embrace.el ends here
