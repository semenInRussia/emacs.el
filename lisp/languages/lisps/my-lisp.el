;;; my-lisp.el --- my-lisp

;; Copyright (C) 2022-2024 semenInRussia

;;; Commentary:

;;; Code:

(require 'my-leaf)
(require 'dash)

(declare-function meow-insert "meow-commands.el")


(leaf paredit
  :ensure t
  :hook ((emacs-lisp-mode-hook . paredit-mode)
         (racket-mode-hook . paredit-mode)))

(leaf lisp-mode
  :custom (lisp-body-indent . 2))

(provide 'my-lisp)
;;; my-lisp.el ends here
