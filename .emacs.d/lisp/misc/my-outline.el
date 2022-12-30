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

(defvar my-outline-navigation-map (make-sparse-keymap))

(leaf outline
  :ensure t
  :bind ((:xah-fly-command-map
          :package xah-fly-keys
          ("SPC o o" . 'outline-show-entry)
          ("SPC o d" . 'outline-hide-entry)
          ("SPC o a" . 'outline-show-all)
          ("SPC o s" . 'my-outline-cycle)
          ("SPC o x" . 'my-outline-cycle-buffer)
          ("SPC o k" . 'my-outline-forward-same-level)
          ("SPC o i" . 'my-outline-backward-same-level)
          ("SPC o l" . 'my-outline-next-heading)
          ("SPC o j" . 'my-outline-previous-heading))
         (:my-outline-navigation-map
          ("l" . 'my-outline-next-heading)
          ("k" . 'my-outline-forward-same-level)
          ("j" . 'my-outline-previous-heading)
          ("i" . 'my-outline-backward-same-level)))
  :config                               ;nofmt
  (defun my-outline-cycle ()
    "My version of the `outline-cycle', different is that repeat at last key."
    (interactive)
    (outline-cycle)
    (repeat-at-last-keystroke))

  (defun my-outline-cycle-buffer ()
    "Version of the `outline-cycle-buffer', different is repeat at last key."
    (interactive)
    (outline-cycle-buffer)
    (repeat-at-last-keystroke))

  (defun my-outline-forward-same-level ()
    "Go to the forward `outline' heading with same level as at point."
    (interactive)
    (outline-forward-same-level 1)
    (set-transient-map my-outline-navigation-map))

  (defun my-outline-backward-same-level ()
    "Go to the backward `outline' heading with same level as at point."
    (interactive)
    (outline-backward-same-level 1)
    (set-transient-map my-outline-navigation-map))

  (defun my-outline-backward-same-level ()
    "Go to the backward `outline' heading with same level as at point."
    (interactive)
    (outline-backward-same-level 1)
    (set-transient-map my-outline-navigation-map))

  (defun my-outline-next-heading ()
    "Go to the next `outline' heading."
    (interactive)
    (outline-next-heading)
    (set-transient-map my-outline-navigation-map))

  (defun my-outline-previous-heading ()
    "Go to the previous `outline' heading."
    (interactive)
    (outline-previous-heading)
    (set-transient-map my-outline-navigation-map)))

(provide 'my-outline)
;;; my-outline.el ends here
