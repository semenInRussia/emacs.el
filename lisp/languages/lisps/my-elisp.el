;;; my-elisp.el --- My configuration of the elisp
;; Copyright (C) 2022-2025 semenInRussia

;;; Commentary:
;; My configuration of the `emacs-lisp-mode': language I use to
;; configure Emacs

;;; Code:
(require 'dash)
(require 'my-leaf)
(require 's)

(leaf elisp-mode
  :config
  (leaf inspector
    :ensure (inspector :repo "emacs-straight/inspector" :host github)
    :bind (:emacs-lisp-mode-map
           :package elisp-mode
           ("C-c C-i" . inspector-inspect-last-sexp)))

  (leaf paredit
    :ensure t
    :hook emacs-lisp-mode-hook)

  (leaf eros
    :ensure t
    :bind (([remap eval-last-sexp] . #'eros-eval-last-sexp)
           ([remap eval-defun] . #'eros-eval-defun)))

  (leaf elisp-refs
    :ensure t)

  ;; my small package to insert a new struct field at M-ret
  (leaf my-elisp-class-fields
    :bind (:emacs-lisp-mode-map
           :package elisp-mode
           ("M-RET" . my-elisp-new-field-of-class)))

  ;; from local-projects
  (leaf my-elisp-smartparens
    :hook (emacs-lisp-mode-hook . (lambda () (require 'my-elisp-smartparens)))))

(leaf suggest
  :ensure (suggest :repo "Wilfred/suggest.el" :host github))

(leaf mocker
  :ensure (mocker :repo "sigma/mocker.el" :host github)
  :doc "A library for testing `elisp' with mocks")

(leaf my-reload :hook emacs-lisp-mode-hook)

(leaf helpful
  :ensure t
  :bind (("C-h f"   . helpful-callable)
         ("C-h v"   . helpful-variable)
         ("C-h k"   . helpful-key)
         ("C-c C-d" . helpful-at-point)
         ("C-h F"   . helpful-function)
         ("C-h C"   . helpful-command)))

(provide 'my-elisp)
;;; my-elisp.el ends here
