;;; my-python-editing.el --- Some useful functions for Python editing -*- lexical-binding: t -*-

;;; Commentary:

;; Here are defined some useful for me functions to *edit* a Python source code.
;; NOTE: that binding of these functions are located inside my-python.el

;;; Code:

(require 'dash)
(require 'smartparens)
(require 's)
(require 'just)


;;;###autoload
(defun my-python-optional-type (beg end)
  "Turn a python type in the active region into optional.

Active region is region from BEG to END"
  (interactive "r")
  (goto-char end)
  (insert "]")
  (goto-char beg)
  (insert "Optional["))

;;;###autoload
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

;;;###autoload
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

(provide 'my-python-editing)
;;; my-python-editing.el ends here
