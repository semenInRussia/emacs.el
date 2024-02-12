;;; my-typst.el --- My configuration for `typst' -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2024 semenInRussia

;;; Commentary:

;; My configuration for `typst'.

;;; Code:

(require 'my-leaf)
(require 'dash)


(defcustom my-typst-imenu-generic-expression
  '(("h1"  "^# \\(.*\\)$" 1)
    ("h2"  "^## \\(.*\\)$" 1)
    ("h3"  "^### \\(.*\\)$" 1)
    ("h4"  "^#### \\(.*\\)$" 1)
    ("h5"  "^##### \\(.*\\)$" 1)
    ("h6"  "^###### \\(.*\\)$" 1)
    ("ref" "^.*?<\\(.*?\\)>.*$" 1))
  "List of the specific for `typst-mode' generic expressions.

See `imenu-generic-expression'"
  :group 'my
  :type '(repeat string))

(defun my-typst-first-letter-of-heading-p ()
  "Return non-nil, when the cursor placed at the typst heading start."
  (save-excursion
    (forward-char -1)
    (skip-chars-backward " =")
    (bolp)))

(defun autoformat-typst-capitalize-heading-line ()
  "Capitalize first letter of a heading line (lines which started with =).

It's working for typst."
  (and
   (just-line-prefix-p "=" nil 'trim)
   (my-typst-first-letter-of-heading-p)
   (upcase-char -1)))

(defun autoformat-typst-capitalize-list-item ()
  "Capitalize first letter of a list item line."
  (interactive)
  (and
   ;; line starts with list item prefix
   (--any-p (just-line-prefix-p it nil 'trim)
            '("-" "+" "*"))
   ;; the cursor is after the first character
   (just-call-on-backward-char*
    (looking-back (rx
                   bol
                   (* " ")
                   (group (or "-" "+" "*"))
                   (+ " "))
                  nil))
   ;; then upcase
   (upcase-char -1)))

(leaf typst-ts-mode
  :ensure (typst-ts-mode :host sourcehut :repo "meow_king/typst-ts-mode")
  :bind (:typst-ts-mode-map
         ("C-m" . typst-ts-mode-return))
  :custom (typst-ts-mode-indent-offset . 2)
  :config
  (add-hook 'typst-ts-mode-hook 'visual-line-mode)
  (add-hook 'typst-ts-mode-hook 'my-lsp-ensure)

  (require 'my-autoformat)

  (my-autoformat-bind-for-major-mode
   'typst-ts-mode
   'autoformat-typst-capitalize-heading-line
   'autoformat-typst-capitalize-list-item
   'my-autoformat-sentence-capitalization)

  (with-eval-after-load 'eglot
    (add-to-list 'eglot-server-programs
                 `(typst-ts-mode "typst-lsp"))
    (add-to-list 'eglot-server-programs
                 `(typst-ts-mode . ("typst-lsp"
                                    :initializationOptions
                                    (:exportPdf "onType"))))))



(provide 'my-typst)
;;; my-typst.el ends here
