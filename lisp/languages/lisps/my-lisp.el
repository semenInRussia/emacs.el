;;; my-lisp.el --- my-lisp

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

;;; Code:

(require 'my-leaf)

(declare-function god-mode "god-mode.el")

(require 'dash)
(require 'my-lib)

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
  :ensure t
  :preface (define-prefix-command 'my-lisp-map)
  :defun (paxedit-delete
          paxedit-sexp-region
          paxedit-transpose-backward
          paxedit-transpose-forward)
  :bind ((:paxedit-mode-map
          (";" . 'paxedit-insert-semicolon)
          ("(" . 'paxedit-open-round)
          ("[" . 'paxedit-open-bracket)
          ("{" . 'paxedit-open-curly))
         (:my-lisp-map
          ("C-c C-t" . 'my-paxedit-transpose-forward)
          ("C-c C-u C-t" . 'my-paxedit-transpose-backward)
          ("C-c C-w" . 'paxedit-kill)
          ("C-c C-;" . 'my-paxedit-comment)
          ("C-c C_d" . 'paxedit-symbol-kill)
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
    (god-mode))

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

(leaf lisp-mode :custom (lisp-body-indent . 2))

(provide 'my-lisp)
;;; my-lisp.el ends here
