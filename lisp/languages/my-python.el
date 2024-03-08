;;; my-python.el --- My configuration for Python -*- lexical-binding: t -*-

;; Copyright (C) 2022-2024 semenInRussia

;;; Commentary:

;; My configuration for Python.

;;; Code:

(require 'dash)
(require 'just)
(require 'my-leaf)
(require 's)


(leaf python-mode
  :ensure t
  :mode "\\.py\\'"
  :custom (;; don't guess the indent. it's always 4.  PEP rocks!
           (python-indent-guess-indent-offset . nil)
           ;; customize `lsp-bridge', now I prefer `eglot', but maybe..
           (lsp-bridge-python-lsp-server . nil)
           (lsp-bridge-python-multi-lsp-server . "pyright_ruff"))
  :config
  ;; source is inside "local-projects" directory
  (leaf my-python-editing
    :bind (:python-mode-map
           :package python
           ("C-c C-i" . py-sort-imports)
           ("C-c C-o" . my-python-optional-type)
           ("C-c M-p" . my-python-split-params))))

(leaf flymake-ruff
  :ensure t
  :hook (python-mode-hook . flymake-ruff-load))

(leaf eglot
  :ensure t
  :hook (python-mode-hook . my-lsp-ensure))

(provide 'my-python)
;;; my-python.el ends here
