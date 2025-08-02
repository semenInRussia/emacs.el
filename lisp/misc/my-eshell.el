;;; my-eshell.el --- My configuration of `eshell' -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025 semenInRussia

;;; Commentary:

;; My configuration of `eshell': multi-platrform shell inside Emacs.

;;; Code:

(require 'my-leaf)
(require 'dash)


(leaf eshell
  :bind (("C-`" . eshell)
         (:eshell-mode-map
          :package esh-mode
          ([remap beginning-of-line] . 'eshell-begin-on-new-line)
          ([remap beginning-of-line-text] . 'eshell-begin-on-new-line))))

(leaf eshell-git-prompt
  :ensure t)

(provide 'my-eshell)
;;; my-eshell.el ends here
