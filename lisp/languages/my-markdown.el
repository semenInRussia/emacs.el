;;; my-markdown.el --- My configuration for `markdown-mode'

;; Copyright (C) 2022-2024 Semen Khramtsov

;;; Commentary:

;; My configuration for `markdown-mode'.

;;; Code:

(require 'my-leaf)
(require 'just)
(require 'my-autoformat)

(defcustom my-markdown-imenu-generic-expression
  '(("title""^\\(.*\\)[\n]=+$" 1)
    ("h2-" "^\\(.*\\)[\n]-+$" 1)
    ("h1"   "^# \\(.*\\)$" 1)
    ("h2"   "^## \\(.*\\)$" 1)
    ("h3"   "^### \\(.*\\)$" 1)
    ("h4"   "^#### \\(.*\\)$" 1)
    ("h5"   "^##### \\(.*\\)$" 1)
    ("h6"   "^###### \\(.*\\)$" 1)
    ("fn" "^\\[\\^\\(.*\\)\\]" 1))
  "List of the specific for `markdown-mode' generic expressions.

See `imenu-generic-expression'"
  :group 'my
  :type '(repeat string))

(defun my-markdown-first-letter-of-heading-p ()
  "Return non-nil, when the cursor placed at the `markdown' heading start."
  (save-excursion
    (forward-char -1)
    (skip-chars-backward " #")
    (bolp)))

(defun autoformat-markdown-capitalize-heading-line ()
  "Capitalize first letter of a heading line (lines which started with #)."
  (and
   (just-line-prefix-p "#")
   (my-markdown-first-letter-of-heading-p)
   (upcase-char -1)))

(defun autoformat-markdown-capitalize-list-item ()
  "Capitalize first letter of a list item line."
  (interactive)
  (and
   ;; line starts with list item prefix
   (or
    (--any-p (just-line-prefix-p it nil 'trim)
             '("-" "+" "*"))
    (just-line-regexp-prefix-p
     " *[0-9]+\\."))
   ;; the cursor is after the first character
   (just-call-on-backward-char*
    (looking-back "^ *\\(-\\|\\+\\|\\*\\|[0-9]+\\.\\) +" nil))
   ;; then upcase
   (upcase-char -1)))

(leaf markdown-mode
  :ensure t
  :hook (markdown-mode-hook .
                            (lambda ()
                              (setq-local
                               imenu-generic-expression
                               my-markdown-imenu-generic-expression)))
  :config
  (add-hook 'markdown-mode-hook 'visual-line-mode)

  (leaf markdown-toc
    :ensure (markdown-mode :repo "jrblevin/markdown-mode" :host github)
    :bind (:markdown-mode-map
           :package markdown-mode
           ("C-T" . markdown-toc-generate-or-refresh-toc)))

  (leaf edit-indirect
    :ensure (edit-indirect :repo "Fanael/edit-indirect" :host github))

  (require 'my-autoformat)

  (my-autoformat-bind-for-major-mode
   'markdown-mode
   'autoformat-markdown-capitalize-heading-line
   'autoformat-markdown-capitalize-list-item
   'my-autoformat-sentence-capitalization))

(provide 'my-markdown)
;;; my-markdown.el ends here
