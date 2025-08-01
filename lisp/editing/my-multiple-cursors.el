;;; my-multiple-cursors.el --- My configuration for the `multiple-cursors' -*- lexical-binding: t -*-

;; Copyright (C) 2022-2025 semenInRussia

;;; Commentary:

;; My configuration for the `multiple-cursors'.  I often use multiple
;; cursors, because `meow-beacon' is norm.  But when I need to select
;; two , three words in buffer, i use C-, and C-< it's fast

;;; Code:
(require 'my-leaf)

(leaf multiple-cursors
  :ensure t
  :bind (("M-i" . mc/edit-lines)
         ("C-," . mc/mark-next-like-this-word)
         ("C-<" . mc/mark-previous-like-this-word)))

(provide 'my-multiple-cursors)
;;; my-multiple-cursors.el ends here
