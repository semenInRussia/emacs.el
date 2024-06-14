;;; my-eglot.el --- My configuration for lsp -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024 semenInRussia

;;; Commentary:

;; My configuration of lsp.  I am use `eglot'

;;; Code:

(require 'my-leaf)
(require 'dash)

(declare-function turn-off-flycheck "my-flycheck.el")
(declare-function eglot-rename "eglot")


(defun my-interactive-eglot-rename (id)
  "A wrapper for `embark' over `eglot-rename' that run it interactively.

Rename variable which is symbol ID"
  (eglot-rename
   (read-from-minibuffer (format "Rename `%s' to: "  id)
                         nil nil nil nil
                         (format "%s" id))))

(leaf eglot
  :custom `((eglot-sync-connect . 1)
            (eglot-autoshutdown . t)
            (eglot-ignored-server-capabilities
             . '(;; disable code lens
                 :codeLensProvider
                 ;; disable inlay hints
                 :inlayHintProvider))
            (eglot-events-buffer-config . 0)
            (eglot-auto-display-help-buffer . nil))
  :defun eglot-inlay-hints-mode
  :bind ((:eglot-mode-map
          ("C-c lr" . 'eglot-rename)
          ("<f6>"   . 'eglot-rename)
          ("C-c la"  . 'eglot-code-actions)
          ("C-c ll"  . 'eglot-code-actions)
          ([remap my-format-expression] . 'eglot-format))
         (:embark-identifier-map
          :package embark
          ("r" . my-interactive-eglot-rename)))
  :config
  ;; `eglot' use `flymake' instead of `flycheck', so i disable `flycheck'
  (add-hook 'eglot-managed-mode-hook #'turn-off-flycheck)

  ;; set default LSP servers for all supported languages
  (defvar eglot-server-programs)  ; make compiler happier
  ;; python (pyright)
  (setf (alist-get '(python-mode python-ts-mode) eglot-server-programs)
        '("pyright-langserver" "--stdio")))

(leaf consult-eglot
  :ensure t)

(provide 'my-eglot)
;;; my-eglot.el ends here
