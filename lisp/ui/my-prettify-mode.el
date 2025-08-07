;;; my-prettify-mode.el --- My config for `prettify-mode' -*- lexical-binding: t -*-

;; Copyright (C) 2022-2025 semenInRussia

;;; Commentary:

;; My config for `prettify-mode'

;;; Code:

(require 'my-leaf)

(leaf prog-mode :hook (LaTeX-mode-hook . prettify-symbols-mode))

(provide 'my-prettify-mode)
;;; my-prettify-mode.el ends here
