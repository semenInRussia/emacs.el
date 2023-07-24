;;; my-treesit.el --- My configuration of `treesit' -*- lexical-binding: t; -*-

;; Copyright (C) 2022, 2023 semenInRussia

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

;; My configuration of `treesit'.

;;; Code:
(require 'my-leaf)

(require 'my-lib)
(require 'dash)

;; (leaf ts-fold
;;   :ensure (ts-fold :type git :host github :repo "emacs-treesit/ts-fold")
;;   :global-minor-mode global-ts-fold-mode
;;   :defun ts-fold-toggle
;;   :bind (:tree-sitter-mode-map
;;          :package treesit
;;          ([remap outline-hide-entry]      . 'ts-fold-close)
;;          ([remap outline-show-entry]      . 'ts-fold-open)
;;          ([remap my-outline-cycle-buffer] . 'ts-fold-close-all)
;;          ([remap my-outline-cycle]        . 'my-ts-fold-toggle))
;;   :config (defun my-ts-fold-toggle
;;               ()
;;             "Like on `ts-fold-toggle', but repeat at last keystrokes."
;;             (interactive)
;;             (ts-fold-toggle)
;;             (repeat-at-last-keystroke)))

(provide 'my-treesit)
;;; my-treesit.el ends here
