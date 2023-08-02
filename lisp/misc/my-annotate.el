;;; my-annotate.el --- My configuration of `annotate' -*- lexical-binding: t; -*-

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
