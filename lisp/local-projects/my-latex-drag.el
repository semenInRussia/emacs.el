;;; my-latex-drag.el --- Function to drag items in LaTeX -*- lexical-binding: t; -*-

;; Copyright (C) 2023 $6

;; Author: $6 <hrams205@gmail.com>
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

;; Function to drag items in LaTeX.

;;; Code:
(require 'just)
(require 'my-drag)

(defvar my-latex-list-item-string "\\item"
  "String which indicates list item.")

(defun my-latex-drag-right-list-item ()
  "Drag the list item at the point to the right LaTeX list item."
  (interactive)
  (my-latex-mark-list-item)
  (let ((1-list-item (just-text-in-region)))
    (delete-region (region-beginning) (region-end))
    (my-latex-end-of-list-item)
    (insert 1-list-item)
    (my-latex-goto-backward-list-item)))

(defun my-latex-drag-left-list-item ()
  "Drag the list item at the point to the left LaTeX list item."
  (interactive)
  (my-latex-mark-list-item)
  (let ((1-list-item (just-text-in-region)))
    (delete-region (region-beginning) (region-end))
    (my-latex-goto-backward-list-item)
    (insert 1-list-item)
    (my-latex-goto-backward-list-item)))

(defun my-latex-mark-list-item ()
  "Mark as region from the start of the current list item to end of that."
  (interactive)
  (just-mark-region-between-movements 'my-latex-beginning-of-list-item
                                      'my-latex-end-of-list-item))

(defun my-latex-beginning-of-list-item ()
  "Go to the beginning of an LaTeX list item."
  (interactive)
  (end-of-line)
  (just-search-backward-one-of-regexp
   '("\\\\item"                         ;nofmt
     "\\\\begin *{\\(enumerate\\|itemize\\)}")))

(defun my-latex-end-of-list-item ()
  "Go to the end of an LaTeX list item."
  (interactive)
  (end-of-line)
  (just-search-forward-one-of-regexp
   '("\\\\item"                         ;nofmt
     "\\\\end *{\\(itemize\\|enumerate\\)}"))
  (beginning-of-line))

(defun my-latex-goto-backward-list-item ()
  "Go to the beginning of the backward list item."
  (beginning-of-line)
  (search-backward my-latex-list-item-string))

(defun my-latex-list-item-drag-p ()
  "Return t, when dragger for LaTeX list items should work."
  (interactive)
  (and (eq major-mode 'latex-mode) (my-latex-list-item-line-p)))

(defun my-latex-list-item-line-p ()
  "Return t, when current line is a LaTeX list item."
  (just-line-prefix-p "\\item" nil t))

;;;###autoload
(defun my-latex-try-drag-right-list-item ()
  "If the dragger for LaTeX list item should be work, drag that to right."
  (interactive)
  (when (my-latex-list-item-drag-p)
    (my-latex-drag-right-list-item)
    t))

;;;###autoload
(defun my-latex-try-drag-left-list-item ()
  "If the dragger for LaTeX list item should be work, drag that to left."
  (interactive)
  (when (my-latex-list-item-drag-p)
    (my-latex-drag-left-list-item)
    t))

(provide 'my-latex-drag)
;;; my-latex-drag.el ends here
