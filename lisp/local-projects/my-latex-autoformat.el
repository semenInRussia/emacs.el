;;; my-latex-autoformat.el --- autoformat for LaTeX -*- lexical-binding: t; -*-

;; Copyright (C) 2023 $6

;; Author: $6 <hrams@DESKTOP-CQH054L>
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

;; autoformat for LaTeX.

;;; Code:

(require 'my-autoformat)
(require 'my-lib)

(require 'latex)
(require 'dash)

(declare-function cdlatex-wrap-environment "cdlatex.el")


;;;###autoload(autoload 'latex-mode "my-latex-autoformat")
(my-autoformat-bind-for-major-mode
 'latex-mode
 'autoformat-latex-capitalize-special-commands
 'autoformat-latex-expand-to-list-item
 'my-autoformat-sentence-capitalization)

(defvar autoformat-latex-capitalize-latex-commands
  '("author"
    "title"
    "date"
    "part"
    "subsection"
    "subsubsection"
    "section"
    "part"
    "chapter")
  "List of regexps which Emacs will automatically capitalize.")

(defvar autoformat-latex-capitalize-regexps
  (--map
   (s-concat "\\\\" it "\\W*{.")
   autoformat-latex-capitalize-latex-commands)
  "List of regexps which Emacs will automatically capitalize.")

(add-to-list 'autoformat-latex-capitalize-regexps "\\\\item\\W+.")

(defun autoformat-latex-capitalize-special-commands ()
  "Capitalize last symbol, when its match on special regexp."
  (interactive)
  (when (-any #'looking-back autoformat-latex-capitalize-regexps)
    (undo-boundary)
    (capitalize-word -1)))

(defun autoformat-latex-expand-to-list-item ()
  "Try expand fragments sush as 1. or - to LaTeX list items."
  (cond
   ((autoformat-latex-expand-to-enumerate-list-item-p)
    (autoformat-latex-expand-to-enumerate-list-item))
   ((autoformat-latex-expand-to-itemized-list-item-p)
    (autoformat-latex-expand-to-itemized-list-item))))

(defcustom autoformat-latex-enumerate-list-items-triggers
  '("[0-9]*\\. ")
  "List of regepxs which should be expanded to LaTeX enumerate list item.

Will be expanded only on matching in empty line and not in math"
  :type '(repeat string)
  :group 'my)

(defcustom autoformat-latex-itemized-list-items-triggers
  '("- " "\\* ")
  "List of regepxs which should be expanded to LaTeX itemized list item.

Will be expanded only on matching in empty line and not in math"
  :type '(repeat string)
  :group 'my)

(defun autoformat-latex-expand-to-enumerate-list-item-p ()
  "Get t, when autoformat should expand text to the enumerate LaTeX list."
  (my-one-of-regexps-looking-back-on-bol
   autoformat-latex-enumerate-list-items-triggers))

(defun autoformat-latex-expand-to-itemized-list-item-p ()
  "Get t, when autoformat should expand text to the itemized LaTeX list."
  (my-one-of-regexps-looking-back-on-bol
   autoformat-latex-itemized-list-items-triggers))

(defun my-one-of-regexps-looking-back-on-bol (regexps)
  "Get t, when one of REGEXPS matchs with text from current point to bol."
  (->> regexps (--map (concat "^ *" it)) (-some 'looking-back)))

(defun autoformat-latex-expand-to-enumerate-list-item ()
  "Expand, for example, 1. to the LaTeX enumerate list item."
  (delete-region (pos-bol) (pos-eol))
  (if (string-equal (LaTeX-current-environment) "enumerate")
      (LaTeX-insert-item)
    (LaTeX-env-item "enumerate")))

(defun autoformat-latex-expand-to-itemized-list-item ()
  "Expand, for example, 1. to the LaTeX itemized list item."
  (delete-region (pos-bol) (pos-eol))
  (if (string-equal (LaTeX-current-environment) "itemize")
      (LaTeX-insert-item)
    (LaTeX-env-item "itemize")))

(defun my-latex-env-beg-and-end ()
  "Return as cons beginning and end of current LaTeX environment."
  (save-excursion
    (LaTeX-find-matching-begin)
    (end-of-line)
    (forward-char)
    (push-mark nil nil t)
    (LaTeX-find-matching-end)
    (beginning-of-line)
    (forward-char -1)
    (cons (region-beginning) (region-end))))

(defun my-latex-env-beg ()
  "Return point at beginning of current LaTeX environment."
  (car (my-latex-env-beg-and-end)))

(defun my-latex-env-end ()
  "Return point at end of current LaTeX environment."
  (cdr (my-latex-env-beg-and-end)))

(defun my-latex-wrap-environment (beg end environment)
  "Wrap the region from BEG to END into ENVIRONMENT.

  If the environment is not given, ask for it using completion."
  (just-mark-region beg end)
  (cdlatex-wrap-environment environment)
  (indent-region (region-beginning) (region-end)))

(defun my-latex-kill-section ()
  "Kill a LaTeX section."
  (interactive)
  (LaTeX-mark-section)
  (kill-region (region-beginning) (region-end)))

(provide 'my-latex-autoformat)
;;; my-latex-autoformat.el ends here
