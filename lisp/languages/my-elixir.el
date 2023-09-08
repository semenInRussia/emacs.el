;;; my-elixir.el --- My configuration of `elixir' -*- lexical-binding: t; -*-

;; Copyright (C) 2023 semenInRussia

;;; Commentary:

;; My configuration of `elixir'.

;;; Code:

(require 'f)
(require 'my-leaf)


(leaf elixir-mode
  :ensure t
  :config
  ;; This is optional. It automatically runs `M-x eglot` for you whenever you are in `elixir-mode`:
  ;; (add-hook 'elixir-mode-hook 'eglot-ensure)

  ;; (with-eval-after-load 'eglot
  ;;   (setf (alist-get 'elixir-mode eglot-server-programs)
  ;;         '("language_server.bat")))
  )

;;; my-elixir.el ends here
