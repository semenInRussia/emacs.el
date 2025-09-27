;;; my-ui.el --- My misc configuration of user interface -*- lexical-binding: t; -*-

;; Copyright (C) 2025 semenInRussia
;; Author: semenInRussia <hrams205@gmail.com>

;;; Commentary:
;; My misc configuration of user interface.

;;; Code:
(require 'my-leaf)

;; highlight the current paragraph and don't highlight other ones
(leaf focus
  :ensure t
  :bind ("M-F" . focus-mode))

;; replace ^L with a horizontal rule
(leaf page-break-lines
  :ensure t)

(leaf prog-mode
  :hook (LaTeX-mode-hook . prettify-symbols-mode))

;; I don't love when line numbers are displayed, but if needed enable it with
;; M-L
(defvar display-line-numbers-type)
(setq display-line-numbers-type nil)
(keymap-global-set "M-L" #'my-toggle-line-numbers)

;; truncate long lines
(setq-default truncate-lines t
              truncate-partial-width-windows t)

;; higlight the current line
(add-hook 'after-init-hook #'global-hl-line-mode)
(keymap-global-set "M-H" #'global-hl-line-mode)

(provide 'my-ui)
;;; my-ui.el ends here
