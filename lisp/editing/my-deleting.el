;;; my-deleting.el --- My confing for some deleting functions

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

;; My confing for some deleting functions

;;; Code:
(require 'my-leaf)
(require 'smartparens)
(require 'my-lib)


(declare-function meow-insert "meow-command.el")


(defun delete-and-edit-current-line ()
  "Delete current line and instroduce to insert mode."
  (interactive)
  (beginning-of-line-text)
  (sp-kill-whole-line)
  (open-line-above)
  (meow-insert))

(defun clear-current-line ()
  "Clear content of current line (including whitespaces)."
  (interactive)
  (kill-region (line-beginning-position) (line-end-position)))

(defun select-current-or-next-word ()
  "If word was selected, then move to next word, otherwise select word."
  (interactive)
  (if (use-region-p)
      (forward-word)
    (forward-sexp -1)
    (push-mark (point) nil t)
    (forward-sexp 1)))

(leaf-keys
 (("M-@"       . select-current-or-next-word)
  ("C-x M-DEL" . clear-current-line)
  ("C-x w"     . delete-and-edit-current-line)))

(provide 'my-deleting)
;;; my-deleting.el ends here
