;;; my-deleting.el --- my-deleting

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
(defun delete-and-edit-current-line ()
  "Delete current line and instroduce to insert mode."
  (interactive)
  (beginning-of-line-text)
  (sp-kill-whole-line)
  (open-line-above)
  (xah-fly-insert-mode-init))

(define-key xah-fly-command-map "w" 'delete-and-edit-current-line)

(defun clear-current-line ()
  "Clear content of current line (including whitespaces)."
  (interactive)
  (kill-region (line-beginning-position) (line-end-position)))

(define-key xah-fly-command-map (kbd "SPC w") 'clear-current-line)

(defun select-current-or-next-word ()
  "If word was selected, then move to next word, otherwise select word."
  (interactive)
  (if (use-region-p)
      (forward-word)
    (xah-extend-selection)))

(define-key xah-fly-command-map (kbd "8") 'select-current-or-next-word)

(define-key-when
  my-cancel-selection-or-delete-text-block
  xah-fly-command-map
  "g"
  'deactivate-mark
  'use-region-p)

(define-key-when
  my-exchange-point-and-mark-or-splice-sexp
  xah-fly-command-map
  "-"
  'exchange-point-and-mark
  'use-region-p)

(provide 'my-deleting)
;;; my-deleting.el ends here