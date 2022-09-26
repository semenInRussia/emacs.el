;;; my-multiple-cursors.el --- my-multiple-cursors

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
(use-package multiple-cursors
    :ensure t
    :custom
    (my-mc-cmds-to-run-once . '(my-mark-all
                                my-bob-or-mc-align
                                my-eob-or-align-with-spaces
                                my-mc-mark-like-this-or-edit-lines
                                my-mc-mark-like-this-or-edit-lines
                                toggle-input-method))
    :bind
    ((:map xah-fly-command-map)
     ("SPC f"     . my-buffer-list-or-edit-lines)
     ("7"         . my-mc-mark-like-this-or-edit-lines)
     ("SPC 7"     . mc/mark-previous-like-this-word)
     ("SPC TAB 7" . mc/reverse-regions)
     ("SPC d 7"   . mc/unmark-next-like-this)
     ("SPC h"     . my-bob-or-mc-align)
     ("SPC n"     . my-eob-or-mc-align-with-space)
     ("SPC a"     . my-mark-all))

    :config
    (defun my-buffer-list-or-edit-lines ()
      "Do `helm-buffer-list' or `mc/edit-lines'."
      (interactive)
      (if (use-region-p)
          (call-interactively #'mc/edit-lines)
        (call-interactively #'helm-multi-files)))

    (defun my-mark-all ()
      "Mark all words like this for `multiple-cursors', otherwise mark buffer."
      (interactive)
      (if multiple-cursors-mode
          (mc/mark-all-words-like-this)
        (mark-whole-buffer)))

    (defun my-bob-or-mc-align ()
      "If enabled `multiple-cursors', then mark then align by regexp, other bob.
BOB - is `beginning-of-buffer'"
      (interactive)
      (if multiple-cursors-mode
          (call-interactively 'mc/vertical-align)
        (beginning-of-buffer)))

    (defun my-eob-or-mc-align-with-space ()
      "If enabled `multiple-cursors', then align by spaces, other eob.
EOB - is `end-of-buffer'"
      (interactive)
      (if multiple-cursors-mode
          (mc/vertical-align-with-space)
        (end-of-buffer)))

    (defun my-mc-mark-like-this-or-edit-lines ()
      "If region on some lines, `mc/edit-lines' other `mc/mark-next-like-this'."
      (interactive)
      (if (and (region-active-p)
               (not (eq (line-number-at-pos (region-beginning))
                        (line-number-at-pos (region-end)))))
          (call-interactively 'mc/edit-lines)
        (call-interactively 'mc/mark-next-like-this-word))))

(provide 'my-multiple-cursors)
;;; my-multiple-cursors.el ends here