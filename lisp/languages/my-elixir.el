;;; my-elixir.el --- My configuration of `elixir' -*- lexical-binding: t; -*-

;; Copyright (C) 2023 semenInRussia

;;; Commentary:

;; My configuration of `elixir'.

;;; Code:

(require 'f)
(require 'my-leaf)


(leaf elixir-mode
  :after elixir-mode eglot
  :defvar eglot-server-programs
  :config
  (require 'eglot)
  (add-to-list 'eglot-server-programs '(elixir-mode "~/temp/language_server.sh"))
  (add-hook 'elixir-mode-hook 'my-lsp-ensure))

(leaf elixir-mode
  :ensure t
  :defer-config
  (add-hook 'elixir-mode-hook 'my-lsp-ensure))

;;; my-elixir.el ends here
