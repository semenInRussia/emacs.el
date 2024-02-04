;;; my-gcmh.el --- My configuration for some garbage collection hacks -*- lexical-binding: t; -*-

;; Copyright (C) 2024 semenInRussia

;; Author: semenInRussia <hrams205@gmail.com>
;; Version: 0.1
;; Homepage: https://github.com/semeninrussia/emacs.el

;;; Commentary:

;;My configuration for some garbage collection hacks

;;; Code:

(require 'leaf)

(leaf gcmh
  :ensure t
  :global-minor-mode t)

;;; my-gcmh.el ends here
(provide 'my-gcmh)
