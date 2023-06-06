;;; my-lyrics.el --- My configuration of `lyrics' -*- lexical-binding: t; -*-

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

;; My configuration of `lyrics'.

;;; Code:

(require 'my-leaf)
(require 'my-lang-utils)
(require 'just)

(leaf my-lyrics
  :mode ("\\.lrc$" . my-lyrics-mode)
  :defer-config                               ;nofmt
  (defun my-lyrics-outline-level ()
    "Return of the heading level for `my-lyrics-mode'.

In `my-lirics-mode' outline line is the following:

[intro]

If you should define outline heading with greater level, then you should add
spaces before [, for example

  [intro]"
    (-
     (save-excursion (beginning-of-line-text) (point))
     (pos-bol)))

  (defun my-lyrics-indent-line ()
    "Indent current line."
    (unless (just-line-prefix-p "[" nil t)
      (beginning-of-line-text)
      (delete-region (point) (pos-bol))
      (insert
       (just-call-on-prev-line*
        (when (just-line-is-whitespaces-p) (forward-line -1))
        (beginning-of-line)
        (make-string
         ;; it's return spaces at the line start
         (skip-chars-forward " ")
         ? )))))

  (defvar my-lyrics-outline-regexp "^ *\\[.*?]"
    "Regexp indicating outline heading line in `my-lyrics-mode'.")

  (defvar my-lyrics-highlights
    `((,my-lyrics-outline-regexp . font-lock-keyword-face))
    "Thing for `my-lyrics-mode' font-lock.")

  (defface my-lyrics-outline-face
    '((t (:foreground "#Ee9a00")))
    "Face to fontify outline headings in `my-lyrics-mode'.

Outline headings demote lines which has the following form

[intro]

instead of intro can be other word"
    :group 'my)

  (define-derived-mode my-lyrics-mode text-mode "Lyrics"
    "Major mode to edit lyrics for songs (panches)."
    (setq-local outline-regexp my-lyrics-outline-regexp)
    (setq-local outline-level 'my-lyrics-outline-level)
    (setq-local comment-start "#")
    (setq-local indent-line-function 'my-lyrics-indent-line)
    (setq-local indent-region-function 'indent-region-line-by-line)
    (visual-fill)
    (setq-local font-lock-defaults '(my-lyrics-highlights))))

(provide 'my-lyrics)
;;; my-lyrics.el ends here
