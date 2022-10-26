;;; my-markdown.el --- My configuration for `markdown-mode'

;; Copyright (C) 2022 Semen Khramtsov

;; Author: Semen Khramtsov <hrams205@gmail.com>
;; Version: 0.1
;; URL: https://github.com/semenInRussia/emacs.el

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; My configuration for `markdown-mode'

;;; Code:

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

(leaf markdown-mode
  :ensure t
  :major-mode-map markdown
  :bind (:my-markdown-local-map
         ("<SPC>" . markdown-toggle-gfm-checkbox)
         ("b"     . markdown-insert-bold)
         ("i"     . markdown-insert-italic)
         ("l"     . markdown-insert-link)
         ("p"     . markdown-live-preview-mode)
         ("'"     . markdown-edit-code-block)
         ("<RET>" . markdown-follow-thing-at-point))
  :hook ((markdown-mode-hook . visual-fill)
         (markdown-mode-hook .
                             (lambda ()
                               (setq-local
                                imenu-generic-expression
                                my-markdown-imenu-generic-expression))))
  :config                               ;nofmt
  (leaf markdown-toc
    :ensure t
    :bind (:my-markdown-local-map
           :package markdown-mode
           ("t" . markdown-toc-generate-or-refresh-toc)))

  (leaf edit-indirect :ensure t)

  (my-autoformat-bind-for-major-mode
   'markdown-mode
   'autoformat-markdown-capitalize-heading-line
   'autoformat-markdown-capitalize-heading-line)

  (defun autoformat-markdown-capitalize-heading-line ()
    "Capitalize first letter of a heading line (lines which started with #)."
    (and
     (just-line-prefix-p "#")
     (my-markdown-first-letter-of-heading-p)
     (upcase-char -1)))

  (defun my-markdown-first-letter-of-heading-p ()
    "Return non-nil, when the cursor placed at the `markdown' heading start."
    (save-excursion
      (forward-char -1)
      (skip-chars-backward " #")
      (bolp))))

(provide 'my-markdown)
;;; my-markdown.el ends here
