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

(require 'my-leaf)
(require 'dash)

(declare-function turn-off-flycheck "my-flycheck.el")


(leaf eglot
  :custom `((eglot-ignored-server-capabilities
             . '(;; disable code lens
                 :codeLensProvider
                 ;; disable inlay hints
                 :inlayHintProvider)))
  :defun eglot-inlay-hints-mode
  :bind (:eglot-mode-map
         ("C-c lr" . 'eglot-rename)
         ("<f6>"   . 'eglot-rename)
         ("C-c la"  . 'eglot-code-actions)
         ("C-c ll"  . 'eglot-code-actions)
         ([remap my-format-expression] . 'eglot-format))
  :fast-exec (("Start a LSP Server for Current Buffer" 'eglot)
              ("Reconnect the LSP Server" 'eglot-reconnect)
              ("Disable the LSP Server" 'eglot-shutdown))
  :config
  ;; `eglot' use `flymake' instead of `flycheck', so i disable `flycheck'
  (add-hook 'eglot-managed-mode-hook #'turn-off-flycheck)

  (leaf flymake
    :bind (:flymake-mode-map
           ("C-c fd" . 'flymake-show-project-diagnostics)
           ([remap next-error] . 'flymake-goto-next-error)
           ([remap prev-error] . 'flymake-goto-prev-error)))

  ;; set default LSP servers for all supported languages
  (defvar eglot-server-programs)  ; make compiler happier
  ;; python (pyright)
  (setf (alist-get '(python-mode python-ts-mode) eglot-server-programs)
        '("pyright-langserver" "--stdio")))

(provide 'my-eglot)
;;; my-eglot.el ends here
