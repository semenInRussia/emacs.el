;;; my-tree-sitter.el --- My configuration of `tree-sitter' -*- lexical-binding: t; -*-

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

;; My configuration of `tree-sitter'.

;;; Code:

(leaf tree-sitter-langs
  :ensure t
  :config (defvar tree-sitter-mode-map (make-sparse-keymap))
  (with-no-warnings
    (add-minor-mode
     'tree-sitter-mode '" tree-sitter" tree-sitter-mode-map nil nil))
  :global-minor-mode global-tree-sitter-mode
  :hook (tree-sitter-after-on-hook . tree-sitter-hl-mode)
  :fast-exec ("Do Tree Sitter Query for Buffer" 'tree-sitter-query-builder))

(leaf ts-fold
  :ensure (ts-fold :type git :host github :repo "emacs-tree-sitter/ts-fold")
  :global-minor-mode global-ts-fold-mode
  :bind (:tree-sitter-mode-map
         :package tree-sitter
         ([remap outline-hide-entry]      . 'ts-fold-close)
         ([remap outline-show-entry]      . 'ts-fold-open)
         ([remap my-outline-cycle-buffer] . 'ts-fold-close-all)
         ([remap my-outline-cycle]        . 'my-ts-fold-toggle))
  :config (defun my-ts-fold-toggle
              ()
            "Like on `ts-fold-toggle', but repeat at last keystrokes."
            (interactive)
            (ts-fold-toggle)
            (repeat-at-last-keystroke)))

(leaf helm-tree-sitter
  :ensure t
  :bind (:tree-sitter-mode-map
         :package tree-sitter
         ([remap helm-imenu] . 'helm-tree-sitter)))

(leaf tree-sitter-occur
  :ensure (tree-sitter-occur :type git
                             :host github
                             :repo "ymarco/tree-sitter-occur"))

(provide 'my-tree-sitter)
;;; my-tree-sitter.el ends here
