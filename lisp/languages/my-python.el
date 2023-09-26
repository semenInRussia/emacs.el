;;; my-python.el --- My configuration for Python -*- lexical-binding: t -*-

;; Copyright (C) 2022-2023 semenInRussia

;;; Commentary:

;; My configuration for Python.

;;; Code:

(require 'dash)
(require 'just)
(require 'my-leaf)
(require 's)
(require 'smartparens)


(leaf python-mode
  :ensure t
  :mode "\\.py\\'"
  :custom (python-shell-interpreter . "python")
  :hook (python-mode-hook . my-python-fix-whitespaces-mode)
  :custom ((lsp-bridge-python-lsp-server . nil)
           (lsp-bridge-python-multi-lsp-server . "pyright_ruff"))
  :bind (:python-mode-map
         ("C-c C-i" . py-sort-imports)
         ("C-c C-o" . my-python-optional-type)
         ("C-c M-p" . my-python-split-params))
  :config

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
    (goto-char end)
    (insert "]")
    (goto-char beg)
    (insert "Optional["))

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
      (sp-get (sp-get-sexp) (indent-region-line-by-line :beg :end))))

  (if (require 'lsp-bridge nil :noerror)
      (add-hook 'python-mode-hook 'lsp-bridge-mode)
    (add-hook 'python-mode-hook 'my-lsp-ensure)))

(provide 'my-python)
;;; my-python.el ends here
