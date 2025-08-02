;;; my-cowsay.el --- My config of `cowsay' -*- lexical-binding: t -*-

;; Copyright (C) 2022-2025 semenInRussia

;;; Commentary:

;; My config of `cowsay': cow can say

;;; Code:

(require 'my-leaf)
(require 'dash)

(leaf cowsay
  :ensure t
  :defun cowsay--get-default-cow cowsay-load-cows
  :defvar cowsay-cows cowsay-directories
  :custom ((cowsay-directories . `(,(locate-user-emacs-file "cows"))))
  :defer-config (cowsay-load-cows))

(provide 'my-cowsay)
;;; my-cowsay.el ends here
