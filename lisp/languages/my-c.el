;;; my-c.el --- My configuration of c and c++ languages -*- lexical-binding: t; -*-

;; Copyright (C) 2023 semenInRussia

;;; Commentary:

;; My configuration of c and c++ languages

;;; Code:
(require 'my-leaf)


(defvar my-c-backend 'lsp
  "A symbol which tells to Emacs which one choose: LSP or ctags.")

(leaf cc-mode
  :config (leaf google-c-style
            :ensure (google-c-style :repo "google/styleguide" :host github)
            :hook ((c++-mode-hook c-mode-hook)   . google-set-c-style)))

(defun my-c-update-backend (backend &rest _ignore)
  "Change backend to a given BACKEND for C/C++ development.

Backend is either symbol tags or lsp"
  (leaf citre
    :when (equal backend 'tags)
    :remove-hook ((c++-mode-hook c-mode-hook) . my-lsp-ensure)
    :hook (c++-mode-hook c-mode-hook))

  (leaf eglot
    :when (equal backend 'lsp)
    :remove-hook ((c++-mode-hook c-mode-hook) . citre-mode)
    :hook ((c++-mode-hook c-mode-hook) . my-lsp-ensure)))

(my-c-update-backend my-c-backend)
(add-variable-watcher 'my-c-backend
                      #'my-c-update-backend)

(provide 'my-c)
;;; my-c.el ends here
