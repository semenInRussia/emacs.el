;;; my-elm.el --- My configuration for Elm language -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2025 semenInRussia

;;; Commentary:

;; My configuration for Elm language.

;;; Code:

(require 'my-leaf)

(leaf elm-mode
  :ensure t
  :bind (:elm-mode-map
         ([remap my-format-expression] . elm-format))
  :config
  (leaf eglot
    :hook (elm-mode-hook . my-lsp-ensure)))

(provide 'my-elm)
;;; my-elm.el ends here
