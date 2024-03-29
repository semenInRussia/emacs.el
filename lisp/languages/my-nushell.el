;;; my-nushell.el --- My configuration of `nushell' -*- lexical-binding: t; -*-

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

;; My configuration of `nushell'.

;;; Code:
(require 'my-leaf)
(defvar aggressive-indent-excluded-modes)

(leaf nushell-mode
  :ensure (nushell-mode :host github :repo "azzamsa/emacs-nushell"))

(leaf yenushell
  :load-path "~/projects/yenushell/"
  :mode ("\\.nu$" . yenushell-mode)
  :config (add-to-list
           'aggressive-indent-excluded-modes
           'yenushell-mode))

(provide 'my-nushell)
;;; my-nushell.el ends here
