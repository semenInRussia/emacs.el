;;; my-gcmh.el --- My configuration for garbage collection hacks -*- lexical-binding: t; -*-

;; Copyright (C) 2024 semenInRussia

;;; Commentary:

;; My configuration for garbage collection hacks

;;; Code:

(require 'leaf)

(leaf gcmh
  :ensure t
  :hook emacs-startup-hook
  :custom ((gcmh-idle-delay . 'auto)
           (gcmh-auto-idle-delay-factor . 10)
           (gcmh-high-cons-threshold . #x1000000)))

;;; my-gcmh.el ends here
(provide 'my-gcmh)
