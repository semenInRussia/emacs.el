;;; my-helpful.el --- My config for `helpful'

;; Copyright (C) 2022-2025 semenInRussia

;;; Commentary:

;; My config for `helpful'.

;;; Code:

(require 'my-leaf)


(leaf helpful
  :ensure t
  :bind (("C-h f"   . helpful-callable)
         ("C-h v"   . helpful-variable)
         ("C-h k"   . helpful-key)
         ("C-c C-d" . helpful-at-point)
         ("C-h F"   . helpful-function)
         ("C-h C"   . helpful-command)))

(provide 'my-helpful)
;;; my-helpful.el ends here
