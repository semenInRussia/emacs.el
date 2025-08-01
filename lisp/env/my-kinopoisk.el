;;; my-kinopoisk.el --- My configuration of `kinopoisk' -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2025 semenInRussia

;;; Commentary:

;; My configuration of client to kinopoisk: my package. Kinopoisk is
;; russian cinema service

;;; Code:

(require 'my-leaf)


(leaf kinopoisk
  :ensure (kinopoisk :host github
                     :repo "semenInRussia/emacs-kinopoisk"
                     :files ("*.el" "extensions/*.el")))

(pam-use-package 'helm-core)


;;; my-kinopoisk.el ends here
(provide 'my-kinopoisk)
