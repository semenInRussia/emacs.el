;;; my-orderless.el --- Settings of `orderless': the match the completion with only some symbols -*- lexical-binding: t -*-
;;; Copyright (c) 2023-2025 semenInRussia

;;; Commentary:
;; Settings of `orderless': the match the completion with only some symbols.

;;; Code:

(require 'my-leaf)


(leaf orderless
  :ensure t
  :commands orderless
  :init (setq completion-styles '(orderless)))

(provide 'my-orderless)
;;; my-orderless.el ends here
