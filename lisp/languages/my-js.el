;;; my-js.el --- My configuration for JavaScript and TypeScript -*- lexical-binding: t -*-

;; Copyright (C) 2022-2025 semenInRussia

;;; Commentary:

;; My configuration for JavaScript and TypeScript.

;;; Code:

(require 'my-leaf)

(leaf js
  :defvar lsp-bridge-single-lang-server-mode-list
  ;; :defvar lsp-bridge-multi-lang-server-mode-list
  :mode "\\.js$"
  :config
  (leaf eglot
    :hook (js-mode-hook . my-lsp-ensure))

  ;; (if (not (require 'lsp-bridge nil :noerror))
  ;;     (ignore-errors
  ;;       (user-error "`lsp-bridge' didn't installed!!!  LSPs for JS can't work"))
  ;;   (add-to-list 'lsp-bridge-multi-lang-server-mode-list
  ;;                '((typescript-mode js-mode)
  ;;                  . "typescript_rome")))
  (leaf js-comint
    :ensure (js-comint :repo "redguardtoo/js-comint" :host github)))

(leaf typescript-mode
  :ensure (typescript-mode :repo "emacs-typescript/typescript.el" :host github)
  :custom (typescript-indent-level . 2)
  :config
  ;; (if (not (require 'lsp-bridge nil :noerror))
  ;;     (user-error "`lsp-bridge' didn't installed!!!  LSPs for JS can't work")
  ;;   (add-to-list 'lsp-bridge-multi-lang-server-mode-list
  ;;                '((typescript-mode js-mode)
  ;;                  . "typescript_rome")))
  (leaf eglot
    :hook (typescript-mode-hook . my-lsp-ensure)))

(provide 'my-js)
;;; my-js.el ends here
