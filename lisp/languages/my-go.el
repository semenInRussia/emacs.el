;;; my-go.el --- My configuration for Go-language -*- lexical-binding: t -*-

;; Copyright (C) 2022-2025 semenInRussia

;;; Commentary:

;; My configuration for Go-language.

;;; Code:

(require 'my-leaf)

(leaf go-mode
  :ensure t
  :config (add-hook 'go-mode-hook 'my-lsp-ensure))

(provide 'my-go)
;;; my-go.el ends here
