;;; my-calc-funcs.el --- Some functions for `calc' -*- lexical-binding: t; -*-
;; Copyright (C) 2024 semenInRuddia

;;; Commentary:
;; Some functions for `calc'.

;;; Code:
(require 'calc)
(require 'dash)
(require 's)

(declare-function calc-vector-mean "calc-stat")

;;;###autoload
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
  (calc-vector-mean nil))

(provide 'my-calc-funcs)
;;; my-calc-funcs.el ends here
