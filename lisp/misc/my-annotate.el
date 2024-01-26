;;; my-annotate.el --- My configuration of `annotate' -*- lexical-binding: t; -*-

;; Copyright (C) 2022 semenInRussia
;; Author: semenInRussia <hrams205@gmail.com>
;; Version: 0.1
;; Homepage: https://github.com/semeninrussia/emacs.el

;;; Commentary:

;; My configuration of `annotate'.

;;; Code:

(require 'my-leaf)

(leaf annotate
  :ensure t
  :custom ((annotate-use-echo-area . t)
           (annotate-print-annotation-under-cursor . t)
           (annotate-print-annotation-under-cursor-prefix . "[ann] "))
  :bind (:org-mode-map
         :package org
         ("C-c M-a" . 'annotate-annotate)
         ("C-c C-u M-a" . 'annotate-delete-annotation))
  :commands annotate-mode
  :config (annotate-mode))

(provide 'my-annotate)
;;; my-annotate.el ends here
