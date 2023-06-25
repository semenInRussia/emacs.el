;;; my-python.el --- My configuration for `python'

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

;; My configuration for `python'

;;; Code:
(require 'my-leaf)
(require 'dash)
(require 's)
(require 'just)
(require 'smartparens)

(leaf python-mode
  :ensure t
  :mode "\\.py\\'"
  :custom (python-shell-interpreter . "python")
  :hook ((python-mode-hook . my-python-fix-whitespaces-mode)
         (python-mode-hook . my-lsp-ensure))
  :custom ((lsp-bridge-python-lsp-server . nil)
           (lsp-bridge-python-multi-lsp-server . "pyright_ruff"))
  :bind (:python-mode-map
         ("C-c C-i" . py-sort-imports)
         ("C-c C-o" . my-python-optional-type)
         ("C-c M-p" . my-python-split-params))
  :config                               ;nofmt
  (defun my-python-split-multi-imports-in-1-line ()
    "Find all lines importing more then 1 thing from module and split it."
    (interactive)
    (save-excursion
      (goto-char (point-min))
      (while (search-forward-regexp
              "^from +\\(.*?\\) +import +\\(\\(.*?\\), *\\)+" nil t)
        (let ((line (just-text-at-line))
              (from (match-string 1)))
          (delete-region (pos-bol) (pos-eol))
          (--each
              (->>
               line
               (s-split "import")
               (-last-item)
               (s-split ",")
               (-map 's-trim))
            (insert "from " from " import " it)
            (newline))))))

  (defun my-python-optional-type (beg end)
    "Turn a python type in the active region into optional.

Active region is region from BEG to END"
    (interactive "r")
    (let ((init-pos (point)))
      (goto-char end)
      (insert "]")
      (goto-char beg)
      (insert "Optional[")))

  (defun my-python-fix-whitespaces-mode ()
    "Fix `whitespace-mode' for `python-mode'."
    (interactive)
    (setq-local whitespace-line-column 88))

  (defun my-python-split-params ()
    "Split params of a python def block into some lines."
    (interactive)
    (save-excursion
      (end-of-line)
      (search-backward "def")
      (forward-sexp)
      (sp-get
          (sp-get-sexp)
        (replace-string-in-region "," ",\n" :beg :end))
      (sp-get (sp-get-sexp) (indent-region-line-by-line :beg :end)))))

(provide 'my-python)
;;; my-python.el ends here
