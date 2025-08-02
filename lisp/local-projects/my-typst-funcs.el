;;; my-typst-funcs.el --- Some helper functions for `typst-mode' -*- lexical-binding: t -*-
;; semenInRussia 2024-2025

;;; Commentary:
;;; Code:
(require 'just)

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

;;;###autoload
(defun autoformat-typst-capitalize-heading-line ()
  "Capitalize first letter of a heading line (lines which started with =).

It's working for typst."
  (and
   (just-line-prefix-p "=" nil 'trim)
   (my-typst-first-letter-of-heading-p)
   (upcase-char -1)))

;;;###autoload
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

(provide 'my-typst-funcs)
;;; my-typst-funcs.el ends here
