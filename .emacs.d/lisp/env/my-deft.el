;;; my-deft.el --- my-deft

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
(use-package deft
    :ensure t
    :bind
    ((:map deft-mode-map)
     ([remap my-kill-rectangle-or-delete-char] . "DEL")
     ([remap syntax-subword-backward-kill]     . "M-DEL")
     ([remap xah-paste-or-paste-previous]      . 'deft-filter-yank)
     ([remap xah-cut-all-or-region]            . 'my-deft-filter-decrement-all))

    :hook
    (deft-filter . my-deft-titlize-regexp)

    :custom
    ((deft-directory             "~/notes/")
     (deft-default-extension     "org")
     (deft-recursive             t)
     (deft-use-filename-as-title nil))

    :config
    (defun my-deft-titlize-regexp ()
      (when (= (length deft-filter-regexp) 1)
        (setf (car deft-filter-regexp)
              (my-upcase-first-char (car deft-filter-regexp))))
      (deft-filter-update))

    (defun my-upcase-first-char (str)
      "Upper case first char of the STR rest chars not take."
      (s-concat (s-upcase (substring str 0 1))
                (substring str 1)))

    (defun fast-exec-deft-keys ()
      "Get some useful keymaps of  `fast-exec' for deft."
      (fast-exec/some-commands
       ("Manage Notes" 'deft)))

    (fast-exec/register-keymap-func 'fast-exec-deft-keys)
    (fast-exec/reload-functions-chain))

(provide 'my-deft)
;;; my-deft.el ends here