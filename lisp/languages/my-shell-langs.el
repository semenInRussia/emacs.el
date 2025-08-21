;;; my-shell-langs.el --- My configuration to edit some "shell" languages -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2025 semenInRussia

;;; Commentary:

;; My configuration to edit some shell languages.
;;
;; Here i configure the following languages:
;; - The Fish Language is a language to configure the fish shell
;; - bat - "a BATch file", like .sh but for windows

;;; Code:

(require 'leaf)


(leaf fish-mode
  :ensure t)

;; "a batch file" for Windows (MS DOS)
(leaf bat-mode
  :config
  (define-auto-insert 'bat-mode '(insert "@echo off")))

(provide 'my-shell-langs)
;;; my-shell-langs.el ends here
