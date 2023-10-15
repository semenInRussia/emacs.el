;;; my-eshell.el --- My configuration of `eshell' -*- lexical-binding: t; -*-

;; Copyright (C) 2022 semenInRussia
;; Author: semenInRussia <hrams205@gmail.com>
;; Version: 0.1
;; Homepage: https://github.com/semeninrussia/emacs.el

;;; Commentary:

;; My configuration of `eshell'.

;;; Code:

(require 'my-leaf)
(require 'dash)


(leaf eshell
  :bind (:eshell-mode-map
         :package esh-mode
         ([remap beginning-of-line]      . 'eshell-begin-on-new-line)
         ([remap beginning-of-line-text] . 'eshell-begin-on-new-line)))

(leaf eshell-git-prompt
  :ensure t
  :hook (ehsell-mode-hook . my-disable-hl-line-mode)
  :config
  (defun my-disable-hl-line-mode ()
    "Don't highlight the line at point, if this mode is enabled.

This mode is called `hl-line-mode'."
    (hl-line-mode 0)))

(provide 'my-eshell)
;;; my-eshell.el ends here
