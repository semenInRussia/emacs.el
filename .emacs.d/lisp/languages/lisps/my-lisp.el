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

(require 'dash)
(require 'my-lib)

(leaf paxedit
  :ensure t
  :init (define-prefix-command 'my-lisp-map)
  :bind ((:paxedit-mode-map
          (";" . 'paxedit-insert-semicolon)
          ("(" . 'paxedit-open-round)
          ("[" . 'paxedit-open-bracket)
          ("{" . 'paxedit-open-curly))
         (:my-lisp-map
          ("o" . 'my-paxedit-transpose-forward)
          ("u" . 'my-paxedit-transpose-backward)
          ("x" . 'paxedit-kill)
          ("z" . 'my-paxedit-comment)
          ("w" . 'my-paxedit-change)
          ("d" . 'paxedit-symbol-kill)
          ("q" . 'paxedit-compress)
          ("k" . 'paxedit-delete-whitespace)
          ("y" . 'my-paxedit-duplicate)))
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
    (xah-fly-insert-mode-activate))

  (defun my-paxedit-duplicate ()
    "Make copy of the Lisp expression at the cursor."
    (interactive)
    (let* ((reg (paxedit-sexp-region))
           (beg (car reg))
           (end (cdr reg))
           (sexp (buffer-substring beg end))
           (sep (if (my-lisp-sexp-whole-line-p) "\n" " ")))
      (goto-char end)
      (insert sep sexp)))

  (defun my-lisp-sexp-whole-line-p ()
    "Return t, when the Lisp sexp at the point being at whole of line."
    (interactive "P")
    (-let
        (((beg . end)
          (paxedit-sexp-region)))
      (and
       (= beg (save-excursion (beginning-of-line-text) (point)))
       (= end (point-at-eol)))))

  (defun my-paxedit-transpose-forward ()
    (interactive)
    (call-interactively #'paxedit-transpose-forward)
    (repeat-at-last-keystroke))

  (defun my-paxedit-transpose-backward ()
    (interactive)
    (call-interactively #'paxedit-transpose-backward)
    (repeat-at-last-keystroke)))

(leaf lisp-mode :custom (lisp-body-indent . 2))

(provide 'my-lisp)
;;; my-lisp.el ends here
