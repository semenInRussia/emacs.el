;;; my-mkn.el --- My configuration for life in mkn -*- lexical-binding: t; -*-

;; Copyright (C) 2025 semenInRussia
;; Author: semenInRussia <hrams205@gmail.com>

;;; Commentary:
;; My configuration for life in mkn.

;;; Code:
(require 'my-leaf)

(defun my-mkn-equation ()
  "Insert an equation into `org-mode' abstract."
  (interactive)
  (insert "\\(")
  (save-excursion
    (insert "\\)"))
  (set-input-method nil))

(defun my-mkn-equation-end ()
  "Insert an equation into `org-mode' abstract."
  (interactive)
  (search-forward "\\)")
  (set-input-method "russian-computer"))

(with-eval-after-load 'org
  (keymap-set org-mode-map "M-U" #'my-mkn-equation)
  (keymap-set org-mode-map "M-I" #'my-mkn-equation-end))

(provide 'my-mkn)
;;; my-mkn.el ends here
