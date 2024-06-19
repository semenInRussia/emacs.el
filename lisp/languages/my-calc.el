;;; my-calc.el --- My configuration of `calc' -*- lexical-binding: t; -*-

;; Copyright (C) 2022-23 semenInRussia

;;; Commentary:

;; My configuration of `calc'.  For main configuration of the `calc' visit
;; the file ~/.emacs.d/calc.el created automatically by `calc'

;;; Code:

(require 'my-leaf)

(require 'dash)
(require 's)

;; also see `my-calc-funcs'
(leaf calc
  :defun (calc-yank-internal calc-pack calc-vector-mean)
  :custom (calc-left-label . "  ")
  :bind (:calc-edit-mode-map
         :package calc-yank
         ([remap save-buffer] . calc-edit-finish)))

(provide 'my-calc)
;;; my-calc.el ends here
