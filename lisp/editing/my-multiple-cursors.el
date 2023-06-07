;;; my-multiple-cursors.el --- My configuration for the `multiple-cursors'

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

;; My configuration for the `multiple-cursors'

;;; Code:
(require 'my-leaf)

(declare-function consult-buffer "consult")

(leaf multiple-cursors
  :ensure t
  :defun (mc/vertical-align-with-space
          mc/edit-lines
          mc/mark-all-words-like-this)
  :defvar multiple-cursors-mode
  :custom (my-mc-cmds-to-run-once .
                                  '(my-mark-all
                                    my-bob-or-mc-align
                                    my-eob-or-align-with-spaces
                                    my-mc-mark-like-this-or-edit-lines
                                    my-mc-mark-like-this-or-edit-lines
                                    toggle-input-method))
  :bind (("M-i"       . 'mc/edit-lines)
         ("C-,"       . 'mc/mark-next-like-this-word)
         ("C-<"       . mc/mark-previous-like-this-word)
         ;; ("SPC TAB 7" . mc/reverse-regions)
         ;; ("SPC d 7"   . mc/unmark-next-like-this)
         ("M-<"     . my-bob-or-mc-align)
         ("M->"     . my-eob-or-mc-align-with-space)
         ("C-x C-," . my-mark-all))
  :config                             ;nofmt

  (defun my-mark-all ()
    "Mark all words like this for `multiple-cursors', otherwise mark buffer."
    (interactive)
    (if multiple-cursors-mode
        (mc/mark-all-words-like-this)
      (call-interactively 'mark-whole-buffer)))

  (defun my-bob-or-mc-align ()
    "If enabled `multiple-cursors', then mark then align by regexp, other bob.

BOB - is `beginning-of-buffer'"
    (interactive)
    (if multiple-cursors-mode
        (call-interactively 'mc/vertical-align)
      (call-interactively 'beginning-of-buffer)))

  (defun my-eob-or-mc-align-with-space ()
    "If enabled `multiple-cursors', then align by spaces, other eob.

EOB - is `end-of-buffer'"
    (interactive)
    (if multiple-cursors-mode
        (mc/vertical-align-with-space)
      (goto-char (point-max)))))

(provide 'my-multiple-cursors)
;;; my-multiple-cursors.el ends here
