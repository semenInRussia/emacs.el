;;; my-eglot.el --- My configuration for lsp -*- lexical-binding: t; -*-
;; Copyright (C) 2022-2025 semenInRussia

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
                         (format "%s" id) nil nil nil
                         (format "%s" id))))

(leaf eglot
  :ensure (eglot :type built-in)
  :custom `(;; (eglot-sync-connect . 1)
            (eglot-events-buffer-size . 0)
            (eglot-autoshutdown . t)
            (eglot-ignored-server-capabilities
             . '(;; disable code lens
                 :codeLensProvider
                 ;; disable inlay hints
                 :inlayHintProvider
                 ;; dont higlight symbol
                 :documentHighlightProvider))
            (eglot-events-buffer-config . 0)
            (eglot-report-progress . nil))
  :defun eglot-inlay-hints-mode eglot-code-actions jsonrpc--log-event
  :bind ((:eglot-mode-map
          ("C-c lr" . eglot-rename)
          ("<f6>" . eglot-rename)
          ("C-c la" . eglot-code-actions)
          ("C-c lg" . eglot-reconnect)
          ("M-q" . eglot-format))
         (:embark-identifier-map
          :package embark
          ("r" . my-interactive-eglot-rename)))
  :config
  ;; `eglot' use `flymake' instead of `flycheck', so i disable `flycheck'
  (add-hook 'eglot-managed-mode-hook #'turn-off-flycheck)

  ;; experiment: cache LSP completions
  (when (featurep 'cape)
    (advice-add 'eglot-completion-at-point :around #'cape-wrap-buster))

  ;; set default LSP servers for all supported languages
  (defvar eglot-server-programs)        ; make compiler happier
  ;; python (pyright)
  (setf (alist-get '(python-mode python-ts-mode) eglot-server-programs
                   nil nil 'equal)
        '("pyright-langserver" "--stdio"))

  (fset #'jsonrpc--log-event #'ignore))  ; massive perf boost---don't log every event

(leaf consult-eglot
  :ensure t)

(provide 'my-eglot)
;;; my-eglot.el ends here
