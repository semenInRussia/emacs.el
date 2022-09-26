;;; my-markdown.el --- my-markdown

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

;;; Code:
(use-package markdown-mode
    :ensure t
    :bind (:map
           my-markdown-mode-local-map
           ("<SPC>"     . markdown-toggle-gfm-checkbox)
           ("b"     . markdown-insert-bold)
           ("i"     . markdown-insert-italic)
           ("l"     . markdown-insert-link)
           ("p"     . markdown-live-preview-mode)
           ("'"     . markdown-edit-code-block)
           ("<RET>" . markdown-follow-thing-at-point))
    :hook (markdown-mode . visual-fill)
    :custom (markdown-imenu-generic-expression
             '(("title""^\\(.*\\)[\n]=+$" 1)
               ("h2-" "^\\(.*\\)[\n]-+$" 1)
               ("h1"   "^# \\(.*\\)$" 1)
               ("h2"   "^## \\(.*\\)$" 1)
               ("h3"   "^### \\(.*\\)$" 1)
               ("h4"   "^#### \\(.*\\)$" 1)
               ("h5"   "^##### \\(.*\\)$" 1)
               ("h6"   "^###### \\(.*\\)$" 1)
               ("fn" "^\\[\\^\\(.*\\)\\]" 1)))
    :config (add-hook 'markdown-mode-hook
                      (lambda ()
                        (setq-local imenu-generic-expression
                                    markdown-imenu-generic-expression)))
    (my-define-local-major-mode-map 'markdown-mode '(markdown-mode)))

(use-package markdown-toc
    :ensure t
    :bind (:map
           my-markdown-mode-local-map
           ("t" . 'markdown-toc-generate-or-refresh-toc)))

(my-also-use-autoformat-in-mode 'markdown-mode
                                markdown-capitalize-heading-line)
(my-also-use-autoformat-in-mode 'gfm-mode
                                markdown-capitalize-heading-line)

(defun autoformat-markdown-capitalize-heading-line ()
  "Capitalize first letter of a heading line (lines which started with #)."
  (when (and
         (just-line-prefix-p "#")
         (my-markdown-first-letter-of-heading))
    (undo-boundary)
    (capitalize-word -1)))

(defun my-markdown-first-letter-of-heading ()
  "Get t, when backward character is first letter of current markdown heading."
  (save-excursion
    (forward-char -1)
    (skip-chars-backward " ")
    (skip-chars-backward "#")
    (bolp)))

(provide 'my-markdown)
;;; my-markdown.el ends here
