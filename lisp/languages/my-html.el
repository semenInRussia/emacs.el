;;; my-html.el --- My configuration for HTML -*- lexical-binding: t -*-

;; Copyright (C) 2022-2025 semenInRussia

;;; Commentary:

;; My configuration for HTML

;;; Code:

(require 'my-leaf)
(require 'dash)
(require 'custom)


(defvar my-html-supported-modes
  '(web-mode mhtml-mode)
  "List of `html` major modes."
  ;; :group 'my
  ;; :type '(repeat symbol)
  )

;; (defun my-html-supported-modes-hooks ()
;;   "Return list from the hooks for each of `my-html-supported-modes'."
;;   (-map 'my-major-mode-to-hook my-html-supported-modes))

;; (defun my-html-supported-modes-maps ()
;;   "Return list from the maps for each of `my-html-supported-modes'."
;;   (-map 'my-major-mode-to-map my-html-supported-modes))

(leaf mhtml-mode
  :mode "\\.html$"
  :config
  (leaf eglot
    :hook (mhtml-mode-hook . my-lsp-ensure))
  (leaf auto-rename-tag
    :ensure (auto-rename-tag :repo "jcs-elpa/auto-rename-tag" :host github))

  ;; (leaf tagedit
  ;;   :ensure (tagedit :repo "magnars/tagedit" :host github))

  (leaf emmet-mode
    :ensure t
    :hook mhtml-mode-hook)

  ;;; DEPRECATED: I don't use it
  (leaf impatient-mode
    :disabled t
    :ensure (impatient-mode :repo "skeeto/impatient-mode" :host github)
    :defun (imp-visit-buffer impatient-mode)
    :bind (:html-mode-map
           :package mhtml-mode
           ("C-c C-e" . my-enable-impatient-mode))
    :config                             ;nofmt
    (defun my-enable-impatient-mode ()
      "Enable `impatient-mode' open page of the file in the web browser."
      (interactive)
      (impatient-mode +1)
      (imp-visit-buffer))))

(provide 'my-html)
;;; my-html.el ends here
