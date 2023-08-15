;;; my-calc.el --- My configuration of `calc' -*- lexical-binding: t; -*-

;; Copyright (C) 2022-23 semenInRussia

;;; Commentary:

;; My configuration of `calc'.  For main configuration of the `calc' visit
;; the file ~/.emacs.d/calc.el created automatically by `calc'

;;; Code:

(require 'my-leaf)

(require 'dash)
(require 's)


(leaf calc
  :defun (calc-yank-internal calc-pack calc-vector-mean)
  :bind ((:calc-mode-map
          :package calc
          ("v" . nil)
          ("v y" . my-calc-mean-yank))
         (:calc-edit-mode-map
          :package calc-yank
          ([remap save-buffer] . calc-edit-finish)))
  :config
  (defun my-calc-mean-yank (vec)
    "Yank to calculator vector of numbers VEC as string and compute mean.

When call interactively, VEC equal lines of the clipboard as numbers, same
mechanism use `calc-yank'"
    (interactive
     (list
      (->>
       (current-kill 0 t)
       (s-split-words)
       (-remove-item "•")
       (s-join "\n"))))
    (calc-yank-internal 0 vec)
    (calc-pack (length (s-lines vec)))
    (calc-vector-mean nil)))

(provide 'my-calc)
;;; my-calc.el ends here
