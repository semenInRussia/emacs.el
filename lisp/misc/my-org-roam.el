;;; my-org-roam.el --- My configuration of `org-roam' -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2023 semenInRussia

;;; Commentary:

;; My configuration of `org-roam'.

;;; Code:
(require 'my-leaf)
(require 'f)

(leaf emacsql
  :ensure t)

(leaf org-roam
  :ensure t
  :init (f-mkdir "~/org-roam")
  :commands org-roam-ui-mode
  :defun org-roam-db-autosync-mode
  :bind (("C-c z f"   . org-roam-node-find)
         ("C-c z t t" . org-roam-tag-add)
         ("C-c z t d" . org-roam-tag-remove)
         ("C-c z s s" . org-roam-ref-add)
         ("C-c z s d" . org-roam-ref-remove)
         ("C-c z a a" . org-roam-alias-add)
         ("C-c z a d" . org-roam-alias-remove)
         ("C-c z o"   . org-roam-buffer-toggle)
         ("C-c z j"   . org-roam-node-insert))
  :custom (org-roam-mode-sections . '(org-roam-backlinks-section))
  :config                               ;nofmt
  (org-roam-db-autosync-mode t)

  ;; (require 'org-roam-export)
  ;; (require 'org-roam-protocol)

  (leaf simple-httpd
    :ensure t)

  (leaf org-roam-ui
    :ensure t
    :defun org-roam-ui-mode
    :config (org-roam-ui-mode t)))

(provide 'my-org-roam)
;;; my-org-roam.el ends here
