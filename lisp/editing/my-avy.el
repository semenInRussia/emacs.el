;;; my-avy.el --- My config for `avy' -*- lexical-binding: t; -*-

;; Copyright (C) 2022 semenInRussia

;; Author: semenInRussia <hrams205@gmail.com>
;; Version: 0.1
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

;; My config for `avy'.

;;; Code:

(require 'my-leaf)

(require 's)

(defvar my-avy-map (make-sparse-keymap))

(define-prefix-command 'my-avy
                       'my-avy-map)

(leaf avy
  :ensure t
  :defvar (avy-goto-word-1 avy-word-punc-regexp)
  :defun (((sp-change-enclosing sp-splice-sexp)
           . smartparens)
          (clear-current-line . my-deleting)
          (avy-jump
           avy-with
           avy-goto-line
           avy-goto-word-1-with-action
           avy-goto-line-1-with-action
           avy-action-kill-move
           avy-action-yank
           avy-action-clear-line
           avy-action-insert-new-line-at-eol
           avy-action-insert-new-line-at-bol))
  :custom (avy-background . t)
  :bind ("C-;" . 'my-avy)
  :bind (:my-avy-map
         (";"   . avy-goto-char)
         ("v"   . avy-yank-word)
         ("x"   . avy-teleport-word)
         ("c"   . avy-copy-word)
         ("2"   . avy-mark-word)
         ("d"   . avy-kill-word-stay)

         ("s ;" . avy-insert-new-line-at-eol)
         ("s h" . avy-insert-new-line-at-bol)

         ("5"   . avy-zap)
         ("TAB" . avy-transpose-words)
         ("w"   . avy-clear-line)
         ("-"   . avy-sp-splice-sexp-in-word)
         ("r"   . avy-kill-word-move)
         ("o"   . avy-change-word)
         ("9"   . avy-sp-change-enclosing-in-word)
         ("z"   . avy-comment-line)

         ("t v" . avy-copy-region)
         ("t d" . avy-kill-region)
         ("t x" . avy-move-region)
         ("t c" . avy-kill-ring-save-region)

         ("n"   . avy-goto-end-of-line)
         ("h"   . avy-goto-begin-of-line-text)

         ("k v" . avy-copy-line)
         ("k x" . avy-move-line)
         ("k c" . avy-kill-ring-save-whole-line)
         ("k d" . avy-kill-whole-line))
  :config

  (defun avy-goto-word-1-with-action (char action ;nofmt
                                           &optional arg beg end symbol)
    "Jump to the currently visible CHAR at a word start.
The window scope is determined by `avy-all-windows'.
When ARG is non-nil, do the opposite of `avy-all-windows'.
BEG and END narrow the scope where candidates are searched.
When SYMBOL is non-nil, jump to symbol start instead of word start.
Do action of `avy' ACTION.'"
    (interactive (list (read-char "char: " t) current-prefix-arg))
    (avy-with avy-goto-word-1
      (let* ((str (string char))
             (regex
              (cond
               ((string= str ".")
                "\\.")
               ((and avy-word-punc-regexp
                     (string-match avy-word-punc-regexp str))
                (regexp-quote str))
               ((<= char 26)
                str)
               (t (concat (if symbol "\\_<" "\\b") str)))))
        (avy-jump regex
                  :window-flip arg
                  :beg beg
                  :end end
                  :action action))))

  (defun avy-zap (char &optional arg)
    "Zapping to next CHAR navigated by `avy'."
    (interactive "cchar:\nP")
    (avy-jump
     (s-concat (char-to-string char))
     :window-flip arg
     :beg (point-min)
     :end (point-max)
     :action 'avy-action-zap-to-char))

  (defun avy-teleport-word (char)
    "Teleport word searched by `arg' with CHAR.
Pass ARG to `avy-jump'."
    (interactive "cchar:\n")
    (avy-goto-word-1-with-action char 'avy-action-teleport))

  (defun avy-mark-word (char)
    "Mark word begining with CHAR searched by `avy'."
    (interactive "cchar: ")
    (avy-goto-word-1-with-action char 'avy-action-mark))

  (defun avy-copy-word (char)
    "Copy word searched by `arg' with CHAR.
Pass ARG to `avy-jump'."
    (interactive "cchar:\n")
    (avy-goto-word-1-with-action char 'avy-action-copy))

  (defun avy-yank-word (char)
    "Paste word searched by `arg' with CHAR.
Pass ARG to `avy-jump'."
    (interactive "cchar:\n")
    (avy-goto-word-1-with-action char 'avy-action-yank))

  (defun avy-kill-word-stay (char)
    "Paste word searched by `arg' with CHAR.
Pass ARG to `avy-jump'."
    (interactive "cchar:\n")
    (avy-goto-word-1-with-action char 'avy-action-kill-stay))

  (defun avy-kill-word-move (char)
    "Paste word searched by `arg' with CHAR.
Pass ARG to `avy-jump'."
    (interactive "cchar:\n")
    (avy-goto-word-1-with-action char 'avy-action-kill-move))

  (defun avy-goto-line-1-with-action (action)
    "Goto line via `avy' with CHAR and do ACTION."
    (interactive)
    (avy-jump "^." :action action))

  (defun avy-comment-line ()
    "With `avy' move to line and comment its."
    (interactive)
    (avy-goto-line-1-with-action 'avy-action-comment))

  (defun avy-action-comment (pt)
    "Saving excursion comment line at point PT."
    (save-excursion (goto-char pt) (comment-line 1)))

  (defun avy-sp-change-enclosing-in-word (ch)
    "With `avy' move to word starting with CH and `sp-change-enclosing'."
    (interactive "cchar:")
    (avy-goto-word-1-with-action ch 'avy-action-sp-change-enclosing))

  (defun avy-action-sp-change-enclosing (pt)
    "Saving excursion `sp-change-enclosing' in word at point PT."
    (save-excursion (goto-char pt) (sp-change-enclosing)))

  (defun avy-sp-splice-sexp-in-word (ch)
    "With `avy' move to word starting with CH and `sp-splice-sexp'."
    (interactive "cchar:")
    (avy-goto-word-1-with-action ch 'avy-action-sp-splice-sexp))

  (defun avy-action-sp-splice-sexp (pt)
    "Saving excursion `sp-splice-sexp' in word at point PT."
    (save-excursion (goto-char pt) (sp-splice-sexp)))

  (defun avy-change-word (ch)
    "With `avy' move to word starting with CH and change its any other."
    (interactive "cchar:")
    (avy-goto-word-1-with-action ch 'avy-action-change-word))

  (defun avy-action-change-word (pt)
    "Saving excursion navigate to word at point PT and change its."
    (save-excursion
      (avy-action-kill-move pt)
      (insert (read-string "new word, please: " (current-kill 0)))))

  (defun avy-transpose-words (char)
    "Goto CHAR via `avy' and transpose at point word to word at prev point."
    (interactive "cchar: ")
    (avy-goto-word-1-with-action char 'avy-action-transpose-words))

  (defun avy-action-transpose-words (second-pt)
    "Goto SECOND-PT via `avy' and transpose at point to word at point ago."
    (avy-action-yank second-pt)
    (kill-sexp)
    (goto-char second-pt)
    (yank)
    (kill-sexp))

  (defun avy-goto-begin-of-line-text (&optional arg)
    "Call `avy-goto-line' and move to the begin of the text of line.
ARG is will be passed to `avy-goto-line'"
    (interactive "p")
    (avy-goto-line arg)
    (beginning-of-line-text))

  (defun avy-clear-line ()
    "Move to any line via `avy' and clear this line from begin to end.
ARG is will be passed to `avy-goto-line'"
    (interactive)
    (avy-goto-line-1-with-action #'avy-action-clear-line))

  (defun avy-action-clear-line (pt)
    "Move to PT, and clear current line, move back.
Action of `avy', see `avy-action-yank' for example"
    (save-excursion (goto-char pt) (clear-current-line)))

  (defun avy-insert-new-line-at-eol ()
    "Move to any line via `avy' and insert new line at end of line."
    (interactive)
    (avy-goto-line-1-with-action #'avy-action-insert-new-line-at-eol))

  (defun avy-action-insert-new-line-at-eol (pt)
    "Move to PT, and insert new line at end of line, move back.
Action of `avy', see `avy-action-yank' for example"
    (save-excursion (goto-char pt) (end-of-line) (newline)))

  (defun avy-insert-new-line-at-bol ()
    "Move to any line via `avy' and insert new at beginning of line."
    (interactive)
    (avy-goto-line-1-with-action #'avy-action-insert-new-line-at-bol))

  (defun avy-action-insert-new-line-at-bol (pt)
    "Move to PT, and insert new at beginning of line, move back.
Action of `avy', see `avy-action-yank' for example"
    (save-excursion (goto-char pt) (beginning-of-line) (newline))))

(provide 'my-avy)
;;; my-avy.el ends here
