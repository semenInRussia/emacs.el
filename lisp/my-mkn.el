;;; my-mkn.el --- My configuration for life in mkn -*- lexical-binding: t; -*-

;; Copyright (C) 2025 semenInRussia
;; Author: semenInRussia <hrams205@gmail.com>

;;; Commentary:
;; My configuration for life in mkn.

;;; Code:

(require 'my-leaf)
(declare-function laas-org-mathp "lass.el")

(defun my-mkn-equation-block ()
  "Insert begin block equation."
  (interactive)
  (if (laas-org-mathp)
      (progn
        (search-forward "\\end{equation}")
        (set-input-method "russian-computer"))
    (progn
      (insert "\\begin{equation}\n")
      (newline)
      (insert "\\end{equation}\n")
      (forward-line -2)
      (set-input-method nil))))

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
  (keymap-set org-mode-map "M-I" #'my-mkn-equation-end)
  (keymap-set org-mode-map "M-E" #'my-mkn-equation-block))

(define-minor-mode my-mkn-mode
  "Edit/see conspects I scratch on lectures in `org-mode'."
  :group 'misc
  :init-value nil
  (when my-mkn-mode
    (xenops-mode t)))

(defun my-maybe-mkn-mode ()
  "Maybe enable `my-mkn-mode'.

I am enable `my-mkn-mode' for files, that have a susbstring \"mkn\" if file
path."
  (and
   (buffer-file-name)
   (s-contains-p "mkn" (buffer-file-name))
   (my-mkn-mode +1)))

(add-hook 'org-mode-hook 'my-maybe-mkn-mode)

(provide 'my-mkn)
;;; my-mkn.el ends here
