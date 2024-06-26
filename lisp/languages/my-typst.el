;;; my-typst.el --- My configuration for `typst' -*- lexical-binding: t; -*-
;; Copyright (C) 2023-2024 semenInRussia

;;; Commentary:
;; My configuration for `typst'.

;;; Code:
(require 'my-leaf)
(require 'dash)


(leaf typst-ts-mode
  :ensure (typst-ts-mode :host sourcehut :repo "meow_king/typst-ts-mode")
  :bind (:typst-ts-mode-map
         ("C-m" . typst-ts-mode-return))
  :custom (typst-ts-mode-indent-offset . 2)
  :defun (texmathp . texmathp)
  :defer-config
  (add-hook 'typst-ts-mode-hook 'visual-line-mode)
  (add-hook 'typst-ts-mode-hook 'my-lsp-ensure)

  (leaf laas
    :ensure t
    :hook typst-ts-mode-hook
    :config
    (advice-add 'laas-mathp
                :around
                (defun my-typst--mathp (&rest r)
                  (if (derived-mode-p 'typst-ts-mode)
                      (texmathp)
                    (apply r)))))

  (require 'my-autoformat)
  (declare-function my-autoformat-bind-for-major-mode "my-autoformat")
  (my-autoformat-bind-for-major-mode
   'typst-ts-mode
   ;; --
   'autoformat-typst-capitalize-heading-line
   'autoformat-typst-capitalize-list-item
   'my-autoformat-sentence-capitalization)

  (with-eval-after-load 'eglot
    (defvar eglot-server-programs)
    (add-to-list 'eglot-server-programs '(typst-ts-mode "typst-lsp"))
    (add-to-list 'eglot-server-programs
                 '(typst-ts-mode . ("typst-lsp"
                                    :initializationOptions
                                    (:exportPdf "onType"))))))



(provide 'my-typst)
;;; my-typst.el ends here
