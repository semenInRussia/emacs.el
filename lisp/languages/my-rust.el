;;; my-rust.el --- My configuration for rust

;; Copyright (C) 2022, 2023 semenInRussia

;; Author: semenInRussia <hrams205@gmail.com>
;; Version: 0.1
;; URL: https://github.com/semenInRussia/emacs.el

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

(autoload 'my-rust-embrace-hook "my-rust-editing")
(leaf embrace
  :after embrace
  :hook (rust-mode-hook . my-rust-embrace-hook))

(provide 'my-rust)
;;; my-rust.el ends here
