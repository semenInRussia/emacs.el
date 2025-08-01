;;; my-rust.el --- My configuration for rust -*- lexical-binding: t -*-
;; Copyright (C) 2022-2025 semenInRussia

;;; Commentary:
;; My configuration for rust

;;; Code:
(require 'my-leaf)

(leaf rust-mode
  :ensure t
  :defun my-lsp-ensure
  ;; some functions are define inside `my-rust-editing'
  :bind (:rust-mode-map
         ("C-c C-m" . 'rust-toggle-mutability)
         ("C-c M-p" . 'my-rust-toggle-pub)
         ("C-c C-t" . 'my-rust-visit-Cargo.toml))
  :config
  (add-hook 'rust-mode-hook #'my-lsp-ensure))

(provide 'my-rust)
;;; my-rust.el ends here
