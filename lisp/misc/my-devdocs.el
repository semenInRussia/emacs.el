;;; my-devdocs.el --- My config for `devdocs'

;; Copyright (C) 2022 Semen Khramtsov
;; Author: Semen Khramtsov <hrams205@gmail.com>
;; Version: 0.1
;; URL: https://github.com/semenInRussia/emacs.el

;;; Commentary:

;; My config for `devdocs'

;;; Code:
(require 'my-leaf)
(require 'dash)


(leaf devdocs
  :ensure t
  :hook ((python-mode-hook . my-devdocs-python-hook)
         (emacs-lisp-mode-hook . my-devdocs-emacs-lisp-hook)
         (rust-mode-hook . my-devdocs-rust-hook)
         (c++-mode-hook . my-devdocs-c++-hook)
         (LaTeX-mode-hook . my-devdocs-latex-hook)
         (haskell-mode-hook . my-devdocs-haskell-hook))
  :fast-exec (("Install DevDocs Docset" 'devdocs-install)
              ("Delete DevDocs Docset" 'devdocs-delete))
  :bind ("C-c d" . 'devdocs-lookup)
  :config                               ;nofmt

  (defun my-devdocs-python-hook ()
    "Set docsets of `devdocs' for `python-mode'."
    (setq-local devdocs-current-docs '("python~3.11")))

  (defun my-devdocs-latex-hook ()
    "Set docsets of `devdocs' for `latex-mode'."
    (setq-local devdocs-current-docs '("latex")))

  (defun my-devdocs-emacs-lisp-hook ()
    "Set docsets of `devdocs' for `emacs-lisp-mode'."
    (setq-local devdocs-current-docs '("elisp")))

  (defun my-devdocs-rust-hook ()
    "Set docsets of `devdocs' for `rust-mode'."
    (setq-local devdocs-current-docs '("rust")))

  (defun my-devdocs-c++-hook ()
    "Set docsets of `devdocs' for `c++-mode'."
    (setq-local devdocs-current-docs '("gcc~12_cpp" "cpp")))

  (defun my-devdocs-haskell-hook ()
    "Set docsets of `devdocs'for `haskell-mode'."
    (setq-local devdocs-current-docs '("haskell~9"))))

(provide 'my-devdocs)
;;; my-devdocs.el ends here
