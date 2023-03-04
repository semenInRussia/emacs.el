;;; my-lsp-bridge.el --- My configuration of `lsp-bridge': the fastest LSP client -*- lexical-binding: t; -*-

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

;; My configuration of `lsp-bridge'.  The fastest LSP client

;;; Code:


(leaf posframe :ensure t)

(leaf lsp-bridge
  :init (add-to-list 'load-path "~/.emacs.d/lisp/site-lisp/lsp-bridge")
  :hook ((lsp-bridge-mode-hook . (lambda () (company-mode 0)))
         (lsp-bridge-mode-hook . turn-off-flycheck))
  :custom (;; features
           (lsp-bridge-enable-hover-diagnostic . t)
           (acm-enable-tabnine . nil)
           (acm-enable-quick-access . t)
           ;; choose LSP servers
           (lsp-bridge-signature-show-function
            . 'eldoc-box--eldoc-message-function)
           (lsp-bridge-python-lsp-server . 'pyright)
           (lsp-bridge-tex-lsp-server . 'texlab))
  :bind ((:xah-fly-command-map
          :package xah-fly-keys
          ("SPC SPC p"   . 'lsp-bridge-popup-documentation)
          ("SPC SPC 7"   . 'lsp-bridge-rename)
          ("SPC SPC RET" . 'lsp-bridge-code-action)
          ("SPC , ,"     . 'lsp-bridge-find-references))
         (:lsp-bridge-mode-map
          ([remap xref-find-definitions] . 'lsp-bridge-find-def)
          ([remap helm-imenu-anywhere] . 'lsp-bridge-workspace-list-symbols)
          ([remap my-format-expression] . 'lsp-bridge-code-format)
          ([remap next-error] . 'lsp-bridge-diagnostic-jump-next)
          ([remap previous-error] . 'lsp-bridge-diagnostic-jump-prev)))
  :fast-exec (("Start a LSP Server for Current Buffer" 'lsp-bridge-mode)
              ("Reconnect the LSP Server" 'lsp-bridge-restart-process))
  :advice ((:after lsp-bridge-code-action xah-fly-insert-mode-activate))
  :bind (:lsp-bridge-call-hierarchy-mode-map
         ("ESC" . nil)
         ("M-n" . 'lsp-bridge-call-hierarchy-next)
         ("M-p" . 'lsp-bridge-call-hierarchy-prev)))

(provide 'my-lsp-bridge)
;;; my-lsp-bridge.el ends here
