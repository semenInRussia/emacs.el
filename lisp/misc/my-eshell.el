;;; my-eshell.el --- My configuration of `eshell' -*- lexical-binding: t; -*-
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
;; My configuration of `eshell'.
;;; Code:
(require 'my-leaf)
(require 'dash)
(require 'my-xah)
(defcustom my-eshell-commands-using-minibuffer
  '(completion-at-point)
  "List of the `eshell' commands using the minubuffer."
  :type '(repeat symbol)
  :group 'my)

(leaf eshell
  :ensure t
  :bind (:eshell-mode-map
         :package esh-mode
         ([remap beginning-of-line] . 'eshell-begin-on-new-line)
         ([remap beginning-of-line-text] . 'eshell-begin-on-new-line))
  :config                               ;nofmt
  (leaf company-shell :ensure t :require t)
  (--each my-eshell-commands-using-minibuffer
    (advice-add it :after
                (lambda (&rest _) (xah-fly-insert-mode-activate))
                '((name . xah-fly-insert-mode-activate)))))
(provide 'my-eshell)
;;; my-eshell.el ends here
