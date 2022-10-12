;;; my-latex-math-spaces.el --- Auto insertion of the spaces in the LaTeX math environment -*- lexical-binding: t; -*-

;; Copyright (C) 2022 semenInRussia

;; Author: semenInRussia <hrams205@gmail.com>
;; Version: 0.1
;; Homepage: https://github.com/semeninrussia/emacs.el

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

;; Auto insertion of the spaces in the LaTeX math environment.

;;; Code:

(require 'just)
(require 's)

(defgroup my-latex-math-spaces nil
  "Auto insertion of the spaces in the LaTeX math environment."
  :group 'my)

(defcustom my-latex-math-spaces-binary-ops
  nil
  "List of the regexps which indicates an binary operator of the LaTeX math.

Binary operator is a LaTeX command which no take arguments and needs to 2
one left number/expression and one rigth number/expression.  So, \\frac isn't
binary operator, because it takes arguments"
  :type '(repeat regexp))

(defcustom my-latex-math-spaces-parens
  '("(" "\\\\left." "|")
  "List of the regexps which indicates an paren of the LaTeX math.

In that list openning and closing parens should be added separately."
  :type '(repeat string))

(defvar my-latex-math-spaces-do-hook nil
  "Hooks which will be run when called `my-latex-math-spaces-do'.")

(define-minor-mode my-latex-math-spaces-mode
  "Minor mode which automatically insert spaces in the LaTeX math."
  :init-value t
  (if (not my-latex-math-spaces-mode)
      (remove-hook 'post-self-insert-hook 'my-latex-math-spaces-do)
    (add-hook 'post-self-insert-hook 'my-latex-math-spaces-do)))

(defun my-latex-math-spaces-do ()
  "Do insertion of the spaces for the LaTeX math syntax, if needed."
  (interactive)
  (when (and (eq major-mode 'latex-mode) (texmathp))
    (run-hooks 'my-latex-math-spaces-do-hook)))

(defun my-latex-math-spaces-for-binary-ops ()
  "Do insertion of the spaces for the last LaTeX binary operation, if needed."
  (interactive)
  (let ((start (point)))
    (when (my-latex-math-spaces-goto-binary-op-start)
      (goto-char (+ start (just-spaces-to-1)
                    ;; traveled distance
                    ))
      (just-spaces-to-1))))

(add-hook 'my-latex-math-spaces-do-hook 'my-latex-math-spaces-for-binary-ops)

(defun my-latex-math-spaces-goto-binary-op-start ()
  "Go to backward binary operation start when its looking back."
  (-any 'my-latex-math-spaces-skip-backward my-latex-math-spaces-binary-ops))

(defun my-latex-math-spaces-for-parens ()
  "Do insertion of the spaces for the last LaTeX parens commands, if needed."
  (-when-let*
      ((init-pos (point))
       (paren (my-latex-math-spaces-goto-parens-start))
       (point-update (just-spaces-to-1)))
    (goto-char (+ init-pos point-update))
    (when (> (length paren) 1) (just-spaces-to-1))))

(add-hook 'my-latex-math-spaces-do-hook 'my-latex-math-spaces-for-binary-ops)

(defun my-latex-math-spaces-goto-parens-start ()
  "Go to the start of the LaTeX paren command, if it placed backward of point.

Return a matched paren or nil if paren isn't found."
  (-any 'my-latex-math-spaces-skip-backward my-latex-math-spaces-parens))

(defun my-latex-math-spaces-for-backslash ()
  "Do insertion of the spaces for the backward from point char \ , if needed."
  (save-excursion
    (skip-chars-backward " ")
    (when (= (char-before) ?\\)
      (forward-char -1)
      (just-spaces-to-1))))

(add-hook 'my-latex-math-spaces-do-hook 'my-latex-math-spaces-for-backslash)

(defun my-latex-math-spaces-skip-backward (regexp &optional ignore-spaces)
  "Skip REGEXP looking back, if regexps match return point, else return nil.

If IGNORE-SPACES is non-nil, then ignore backward-spaces"
  (when ignore-spaces (setq regexp (s-concat regexp " *")))
  (when (search-backward-regexp (s-concat regexp "\\=") nil t)
    (goto-char (match-beginning 0))
    (match-string 0)))

(defun my-latex-math-spaces-skip-forward (regexp)
  "Skip REGEXP looking back, if regexps match return point, else return nil."
  (search-forward-regexp (s-concat "\\=" regexp) nil t))

(defun my-latex-declare-bin-op (&rest ops)
  "Define LaTeX binary OPS for auto insertion of the spaces in math."
  (--each ops (add-to-list 'my-latex-math-spaces-binary-ops it)))

(defun my-latex-declare-parens (&rest parens)
  "Define LaTeX PARENS for auto insertion of the spaces in math."
  (--each parens (add-to-list 'my-latex-math-spaces-parens it)))

(my-latex-declare-bin-op
 "\\+"
 "-"
 "\\\\cdot"
 "\\\\times"
 "="
 "\\\\neq"
 "&="
 "\\\\mapsto"
 "\\\\pm"
 "\\\\mp"
 "\\\\to"
 "\\\\ll"
 "\\\\leq"
 "\\\\diamond"
 "\\\\impliedby"
 "\\\\implies"
 "\\\\geq"
 "\\\\gg"
 "\\\\in"
 "\\\\models"
 "\\\\mid"
 "\\\\approx"
 "\\\\sim")

(provide 'my-latex-math-spaces)
;;; my-latex-math-spaces.el ends here
