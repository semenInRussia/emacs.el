;;; my-goto-last-change.el --- My configuration of `goto-last-change' -*- lexical-binding: t; -*-
;; Copyright (C) 2022-2025 semenInRussia

;;; Commentary:
;; My configuration of `goto-last-change'.

;;; Code:
(require 'my-leaf)


(leaf goto-last-change
  :ensure (goto-last-change :repo "camdez/goto-last-change.el" :host github)
  :bind (("C-_" . 'goto-last-change)
         (:meow-normal-state-keymap
          :package meow
          ("_" . goto-last-change))))

(provide 'my-goto-last-change)
;;; my-goto-last-change.el ends here
