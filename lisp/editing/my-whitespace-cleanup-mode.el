;;; my-whitespace-cleanup-mode.el --- My configuration of `whitespaces-cleanup-mode' -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024 semenInRussia
;; Author: semenInRussia <hrams205@gmail.com>

;; My configuration of `whitespace-cleanup-mode'.

;;; Code:

(require 'my-leaf)

(leaf whitespace-cleanup-mode
  :ensure (whitespace-cleanup-mode :repo "purcell/whitespace-cleanup-mode" :host github)
  :global-minor-mode global-whitespace-cleanup-mode)

(provide 'my-whitespace-cleanup-mode)
;;; my-whitespace-cleanup-mode.el ends here
