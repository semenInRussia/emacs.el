;;; my-recentf.el --- My configuration of `recentf' -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2023 semenInRussia

;;; Commentary:

;; My configuration of `recentf'.

;;; Code:

(require 'my-leaf)


(leaf recentf
  :global-minor-mode recentf-mode
  :bind ("C-c r" . recentf))

(provide 'my-recentf)
;;; my-recentf.el ends here
