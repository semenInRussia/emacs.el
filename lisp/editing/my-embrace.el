;;; my-embrace.el --- My configuration of `embrace' -*- lexical-binding: t; -*-

;; Copyright (C) 2025 semenInRussia

;;; Commentary:
;; My configuration of embrace: add, remove, change parens

;;; Code:
(require 'my-leaf)

(leaf embrace
  :ensure t
  :bind ("M-[" . embrace-commander)
  :init
  ;; support meow
  (with-eval-after-load 'meow-helpers
    (declare-function meow-normal-define-key "meow-helpers")
    (meow-normal-define-key '("z" . embrace-commander)))
  (add-hook 'org-mode-hook 'embrace-org-mode-hook)
  (add-hook 'org-mode-hook 'embrace-emacs-lisp-mode-hook))

(provide 'my-embrace)
;;; my-embrace.el ends here
