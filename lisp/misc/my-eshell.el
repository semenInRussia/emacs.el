;;; my-eshell.el --- My configuration of `eshell' -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024 semenInRussia
;; Author: semenInRussia <hrams205@gmail.com>
;; Version: 0.1
;; Homepage: https://github.com/semeninrussia/emacs.el

;;; Commentary:

;; My configuration of `eshell'.

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
