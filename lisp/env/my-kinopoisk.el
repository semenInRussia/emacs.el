;;; my-kinopoisk.el --- My configuration of `kinopoisk' -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2024 semenInRussia

;;; Commentary:

;; My configuration of `kinopoisk'.

;;; Code:

(require 'my-leaf)


(leaf kinopoisk
  :ensure (kinopoisk :host github
                     :repo "semenInRussia/emacs-kinopoisk"
                     :files ("*.el" "extensions/*.el")))

;;; my-kinopoisk.el ends here
