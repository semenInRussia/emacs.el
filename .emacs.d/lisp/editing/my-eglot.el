;;; my-eglot.el --- My configuration for lsp -*- lexical-binding: t; -*-

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

;; My configuration of lsp.  I am use `eglot'

;;; Code:

(defcustom my-eglot-major-modes
  '(rust-mode python-mode haskell-mode)
  "List of the major modes in which should work `eglot'."
  :group 'my
  :type '(repeat major-mode))

(leaf eglot
  :ensure t
  :custom `(;; Performance
            (gc-cons-threshold . 100000000)
            (read-process-output-max . ,(* 1024 1024)) ;; 1mb
            (eglot-send-changes-idle-time . 1)         ; in seconds
            )
  :custom-face (eglot-highlight-symbol-face . '((t (:inherit lazy-highlight))))
  :hook (eglot-managed-mode-hook . turn-off-flycheck)
  :bind ((:xah-fly-command-map
          :package xah-fly-keys
          ("SPC SPC 7"   . 'eglot-rename)
          ("SPC SPC RET" . 'eglot-code-actions)
          ("SPC SPC i" . 'eglot-code-action-organize-imports))
         (:eglot-mode-map
          ([remap my-format-expression] . 'eglot-format)))
  :config                               ;nofmt
  (leaf flymake
    :require t
    :bind (:xah-fly-command-map
           :package xah-fly-keys
           ("SPC SPC p" . 'flymake-show-project-diagnostics)))

  (leaf eldoc-box
    :after my-eldoc
    :hook (eglot-managed-mode-hook . eldoc-box-hover-mode))

  (leaf eglot-x
    :ensure (eglot-x :host github :repo "nemethf/eglot-x")
    :require t
    :bind (:xah-fly-command-map
           :package xah-fly-keys
           ("SPC SPC ," . 'eglot-x-find-refs))))

(provide 'my-eglot)
;;; my-eglot.el ends here
