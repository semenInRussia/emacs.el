;;; my-orderless.el --- Settings of `orderless': the match the completion with only some symbols -*- lexical-binding: t -*-
;;; Copyright (c) 2023 semenInRussia

;;; Commentary:
;; Settings of `orderless': the match the completion with only some symbols.

;;; Code:

(require 'my-leaf)


(leaf orderless
  :ensure (orderless :host github
                     :repo "minad/orderless"
                     :files ("*.el" "extensions/*.el"))
  :commands orderless
  :init (setq completion-styles '(orderless)))

(provide 'my-orderless)
;;; my-orderless.el ends here
