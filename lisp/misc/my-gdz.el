;;; my-gdz.el --- My configuration of `gdz' -*- lexical-binding: t; -*-

;; Copyright (C) 2022 semenInRussia
;; Author: semenInRussia <hrams205@gmail.com>
;; Version: 0.1
;; Homepage: https://github.com/semeninrussia/emacs.el

;;; Commentary:

;; My configuration of `gdz': done home word.

;;; Code:


(require 'my-leaf)
(require 'dash)
(require 's)

;; ;; (defun my-gdz (lesson)
;; ;;   "Interactively Open the gdz web-page."
;; ;;   (interactive (list )
;;                ))

(defun my-gdz-geo (paragraph)
  "Open the web page for done tasks of PARAGRAPH."
  (interactive "nParagraph of geo, please: ")
  (->>
   paragraph
   (+ (- 7))
   (number-to-string)
   (s-prepend
    "https://resheba.me/gdz/geografija/9-klass/alekseev-bolysov/paragraph-")
   (browse-url)))

(provide 'my-gdz)
;;; my-gdz.el ends here
