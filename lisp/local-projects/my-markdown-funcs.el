;;; my-markdown-funcs.el --- Some helper functions for `markdown-mode' -*- lexical-binding: t -*-
;; semenInRussia 2025

;;; Commentary:
;; Some helper functions for `markdown-mode'.

;;; Code:
(require 'dash)
(require 'just)

;;; Autoformat (including autocapitalize):

;;;###autoload
(defun my-markdown-first-letter-of-heading-p ()
  "Return non-nil, when the cursor placed at the `markdown' heading start."
  (save-excursion
    (forward-char -1)
    (skip-chars-backward " #")
    (bolp)))

;;;###autoload
(defun autoformat-markdown-capitalize-heading-line ()
  "Capitalize first letter of a heading line (lines which started with #)."
  (and
   (just-line-prefix-p "#")
   (my-markdown-first-letter-of-heading-p)
   (upcase-char -1)))

;;;###autoload
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

;;; Imenu:

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

(when (eq major-mode 'markdown-mode)
  (setq-local imenu-generic-expression my-markdown-imenu-generic-expression))
(add-hook 'markdown-mode-hook
          (lambda ()
            (setq-local imenu-generic-expression my-markdown-imenu-generic-expression)))

(provide 'my-markdown-funcs)
;;; my-markdown-funcs.el ends here
