;;; my-lisp.el --- General configuration for all Lisp languages -*- lexical-binding: t -*-

;; Copyright (C) 2022-2025 semenInRussia

;;; Commentary:

;; General configuration for all Lisp languages.

;;; Code:

(require 'my-leaf)
(require 'dash)

(declare-function meow-insert "meow-commands.el")


(leaf paredit
  :ensure t
  :hook ((emacs-lisp-mode-hook . paredit-mode)
         (racket-mode-hook . paredit-mode)))

(leaf lisp-mode
  :custom (lisp-body-indent . 2))

(provide 'my-lisp)
;;; my-lisp.el ends here
