;;; my-markdown.el --- My configuration for `markdown-mode' -*- lexical-binding: t -*-

;; Copyright (C) 2022-2025 semenInRussia

;;; Commentary:

;; My configuration for `markdown-mode'.

;;; Code:
(require 'my-leaf)

(leaf markdown-mode
  :ensure t
  :defer-config
  (add-hook 'markdown-mode-hook 'visual-line-mode)

  (leaf markdown-toc
    :ensure t
    :bind (:markdown-mode-map
           :package markdown-mode
           ("C-c C-t" . markdown-toc-generate-or-refresh-toc)))

  (leaf edit-indirect
    :ensure (edit-indirect :repo "Fanael/edit-indirect" :host github))

  (require 'my-autoformat)
  (declare-function my-autoformat-bind-for-major-mode "my-autoformat")
  (my-autoformat-bind-for-major-mode 'markdown-mode
                                     'autoformat-markdown-capitalize-heading-line
                                     'autoformat-markdown-capitalize-list-item
                                     'my-autoformat-sentence-capitalization))

(provide 'my-markdown)
;;; my-markdown.el ends here
