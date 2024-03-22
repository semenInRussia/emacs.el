;;; my-jinx.el --- My configuration for jinx: a cool spell checker -*- lexical-binding: t; -*-

;; Copyright (C) 2024 semenInRussia

;; Author: semenInRussia <hrams205@gmail.com>
;; Version: 0.1
;; Homepage: https://github.com/semeninrussia/emacs.el

;;; Commentary:

;; My configuration for jinx: a cool spell checker.

;;; Code:

(require 'leaf)


(defvar my-enchant-executable "enchant" "A path to executable of the Enchant program.")

(leaf jinx
  :ensure t
  :custom (jinx-languages . "ru_RU en")
  :when (and
         (executable-find "enchant-2")
         (seq-find #'executable-find '("gcc" "clang" "cc")))
  :bind (:meow-normal-state-keymap
         :package meow-core
         ("$" . jinx-correct))
  :hook (text-mode-hook prog-mode-hook))

;;; my-jinx.el ends here
