;;; my-gcmh.el --- My configuration for garbage collection hacks -*- lexical-binding: t; -*-

;; Copyright (C) 2024 semenInRussia

;;; Commentary:

;; My configuration for garbage collection hacks

;;; Code:

(require 'my-leaf)

(leaf gcmh
  :ensure t
  :hook emacs-startup-hook)

;;; my-gcmh.el ends here
(provide 'my-gcmh)
