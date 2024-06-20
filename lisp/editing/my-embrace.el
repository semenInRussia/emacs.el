;;; my-embrace.el --- My configuration of the `embrace'
;; Copyright (C) 2022-2024 semenInRussia

;;; Commentary:
;; My configuration of the `embrace'

;;; Code:
(require 'my-leaf)

(leaf embrace
  :ensure (embrace :repo "cute-jumper/embrace.el" :host github)
  :setq-default (embrace-show-help-p . nil)
  :bind ("C-z" . embrace-commander)
  :defun embrace-emacs-lisp-mode-hook
  :config
  (add-hook 'emacs-lisp-mode-hook #'embrace-emacs-lisp-mode-hook)
  (and (eq major-mode 'emacs-lisp-mode)
       (embrace-emacs-lisp-mode-hook)))

(provide 'my-embrace)
;;; my-embrace.el ends here
