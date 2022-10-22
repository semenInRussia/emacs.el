;;; my-outline.el --- My configuration of `outline' -*- lexical-binding: t; -*-

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

;; My configuration of `outline'.

;;; Code:

(leaf outline
  :ensure t
  :bind (:xah-fly-command-map
         :package xah-fly-keys
         ("SPC o o"   . nil)
         ("SPC o o a" . 'outline-show-all)
         ("SPC o o o" . 'outline-show-entry)
         ("SPC o o d" . 'outline-hide-entry)
         ("SPC o o j" . 'my-outline-cycle))
  :config                               ;nofmt
  (defun my-outline-cycle ()
    "My version of the `outline-cycle', different is that repeat at last key."
    (interactive)
    (outline-cycle)
    (repeat-at-last-keystroke)))

(provide 'my-outline)
;;; my-outline.el ends here
