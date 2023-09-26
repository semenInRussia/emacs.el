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

(require 'my-leaf)
(require 'my-flycheck)
(require 'dash)

(declare-function corfu-mode "corfu")


(leaf posframe
  :ensure (posframe :repo "tumashu/posframe" :host github))

(leaf lsp-bridge
  :when (file-exists-p (locate-user-emacs-file "lisp/site-lisp/lsp-bridge/lsp-bridge.el"))
  :load-path* "lisp/site-lisp/lsp-bridge"
  :hook ((lsp-bridge-user-multiserver-dir . "~/lsp/multi")
         (lsp-bridge-user-langserver-dir . "~/lsp/single/")
         (lsp-bridge-mode-hook . (lambda ()
                                   (and (fboundp 'corfu-mode) (corfu-mode 0)))))
  :custom (;; features
           (lsp-bridge-enable-hover-diagnostic . t)
           (acm-enable-tabnine . nil)
           (acm-enable-quick-access . t)
           ;; choose LSP servers
           (lsp-bridge-tex-lsp-server . 'texlab)
           (lsp-bridge-multi-lang-server-extension-list . nil)
           ;; misc
           (lsp-bridge-python-command . "python")
           (lsp-bridge-diagnostic-display-errors-delay . 0.9)
           ;; use `consult' instead of popup for autofixes
           (lsp-bridge-code-action-enable-popup-menu . nil)
           (lsp-bridge-code-action-preview-delay . 20)
           (lsp-bridge-signature-show-function
            . 'eldoc-box--eldoc-message-function))
  :bind ((:lsp-bridge-mode-map
          ("C-c C-d" . 'lsp-bridge-popup-documentation)
          ("<f6>"    . 'lsp-bridge-rename)
          ("C-c ll"  . 'lsp-bridge-code-action)
          ("C-c la"  . 'lsp-bridge-code-action)
          ("M-,"     . 'lsp-bridge-find-references)
          ([remap xref-pop-marker-stack] . 'lsp-bridge-pop)
          ([remap xref-find-definitions] . 'lsp-bridge-find-def)
          ([remap consult-imenu-multi] . 'lsp-bridge-workspace-list-symbols)
          ([remap my-format-expression] . 'lsp-bridge-code-format)
          ([remap next-error] . 'lsp-bridge-diagnostic-jump-next)
          ([remap previous-error] . 'lsp-bridge-diagnostic-jump-prev))
         (:lsp-bridge-call-hierarchy-mode-map
          ("ESC" . nil)
          ("M-n" . 'lsp-bridge-call-hierarchy-next)
          ("M-p" . 'lsp-bridge-call-hierarchy-prev)))
  :fast-exec (("Start a LSP Server for Current Buffer" 'lsp-bridge-mode)
              ("Reconnect the LSP Server" 'lsp-bridge-restart-process))
  :config
  (advice-add 'acm-is-elisp-mode-p
              :override
              'ignore)
  (add-hook 'lsp-bridge-mode-hook #'turn-off-flycheck))

(provide 'my-lsp-bridge)
;;; my-lsp-bridge.el ends here
