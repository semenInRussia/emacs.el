;;; my-lua.el --- My configuration for the Lua Language -*- lexical-binding: t; -*-

;; Copyright (C) 2024 semenInRussia

;;; Commentary:

;; My configuration for the Lua Language.

;;; Code:

(require 'leaf)


(leaf lua-mode
  :ensure t
  :custom (lua-indent-level . 2)
  :config
  ;; load LSP
  (add-hook 'lua-mode-hook #'my-lsp-ensure)
  (when (equal major-mode 'lua-mode) (my-lsp-ensure)))

;;; my-lua.el ends here
