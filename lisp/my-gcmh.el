;;; my-gcmh.el --- My configuration for garbage collection hacks -*- lexical-binding: t; -*-

;; Copyright (C) 2024 semenInRussia

;;; Commentary:

;; My configuration for garbage collection hacks

;;; Code:

(require 'leaf)

(leaf gcmh
  :ensure t
  :global-minor-mode t)

;;; my-gcmh.el ends here
(provide 'my-gcmh)
