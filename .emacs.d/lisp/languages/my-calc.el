;;; my-calc.el --- My configuration of `calc' -*- lexical-binding: t; -*-

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

;; My configuration of `calc'.  For main configuration of the `calc' visit
;; the file ~/.emacs.d/calc.el created automatically by `calc'

;;; Code:

(require 'dash)

(defvar my-calc-operations
  '(calcDigit-start calc-convert-units calc-algebraic-entry calc-solve-for)
  "List of the function after which will be actived insert mode.")

(leaf calc
  :hook (calc-start-hook . xah-fly-insert-mode-activate)
  :bind (:calc-edit-mode-map
         :package calc-yank
         ([remap save-buffer] . calc-edit-finish))
  :config                               ;nofmt
  (--each my-calc-operations
    (advice-add it :after
                (lambda (&rest _)
                  (message "OK!")
                  (xah-fly-insert-mode-activate))
                '((name . xah-fly-insert-mode-activate)))))

(provide 'my-calc)
;;; my-calc.el ends here
