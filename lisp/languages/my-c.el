;;; my-c.el --- My configuration of c and c++ languages -*- lexical-binding: t; -*-

;; Copyright (C) 2023 semenInRussia

;;; Commentary:

;; My configuration of c and c++ languages

;;; Code:
(require 'my-leaf)


(defvar my-c-backend 'lsp
  "A symbol which tells to Emacs which one choose: LSP or ctags.")

(leaf cc-mode
  :setq-default (;; enable auto insert newline after ";"
                 (c-auto-newline . t)
                 (c-electric-flag . t)
                 (c-hungry-delete-key . t))
  :config (leaf google-c-style
            :ensure (google-c-style :repo "google/styleguide" :host github)
            :hook ((c++-mode-hook c-mode-hook)   . google-set-c-style)))

(add-variable-watcher
 'my-c-backend
 (defun my-c-update-backend (backend &rest _ignore)
   "Change backend for C/C++ development.

Backend is either symbol tags or lsp"
   (leaf citre
     :when (equal my-c-backend 'tags)
     :remove-hook ((c++-mode-hook c-mode-hook) . eglot-ensure)
     :hook (c++-mode-hook c-mode-hook))

   (leaf eglot
     :when (equal my-c-backend 'lsp)
     :remove-hook ((c++-mode-hook c-mode-hook) . citre-mode)
     :hook ((c++-mode-hook c-mode-hook) . eglot-ensure))))

(my-c-update-backend my-c-backend)

(provide 'my-c)
;;; my-c.el ends here
