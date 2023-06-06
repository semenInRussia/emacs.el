;;; my-gdz.el --- My configuration of `gdz' -*- lexical-binding: t; -*-
;; Copyright (C) 2022 semenInRussia
;; Author: semenInRussia <hrams205@gmail.com>
;; Version: 0.1
;; Homepage: https://github.com/semeninrussia/emacs.el
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.
;;; Commentary:
;; My configuration of `gdz'.
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
