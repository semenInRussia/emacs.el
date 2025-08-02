;;; my-org.el --- My configuration for `org-mode' -*- lexical-binding: t -*-
;; Copyright (C) 2022-2025 semenInRussia

;;; Commentary:
;; My configuration for `org-mode'

;;; Code:
(require 'my-leaf)
(require 's)
(require 'dash)

(leaf org
  :defun (meow-insert . meow-command)
  :custom (;; `org-refile'
           (org-refile-use-outline-path . 'file)
           (org-outline-path-complete-in-steps . nil)
           ;; `org' startup
           (org-fold-core-style . 'overlays)
           (org-startup-folded . t)
           (org-startup-indented . t)
           (org-startup-with-inline-images . t))
  :hook (org-mode-hook . org-toggle-pretty-entities)
  :bind (;; NOTE: `org-capture' and `org-agenda' in the my-organization.el file
         ;; ("C-c z c" . org-capture)
         (:org-mode-map
          ("C-c tab"   . org-refile)
          ("C-c C-j"   . org-id-get-create)))
  :config
  (add-hook 'org-mode-hook 'visual-line-mode)
  (add-hook 'org-mode-hook 'aas-activate-for-major-mode))

(leaf my-org-editing
  :bind (:org-mode-map
         :package org
         ("C-c M-i"   . my-org-insert-image)
         ("C-c M-u"   . my-org-insert-img-at-url)))

(leaf toc-org :ensure t)

(leaf org-preview
  :ensure (org-preview :repo "karthink/org-preview"
                       :host github)
  :commands org-preview-mode)

(leaf xenops
  :ensure t
  :bind (:org-mode-map
         :package org
         ("C-c C-x C-l" . xenops-mode))
  :custom (xenops-math-image-scale-factor . 2))

(leaf laas
  :ensure t
  :hook org-mode-hook)

;; format `org-mode' code after every key hit
(leaf my-org-autoformat
  :hook (org-mode-hook . my-autoformat-mode))

(leaf consult
  :bind (:org-mode-map
         :package org
         ([remap consult-imenu] . consult-outline)))

;; remove some useless things from the current `org-mode' buffer
(leaf my-org-do-tidy
  :bind (:org-mode-map
         :package org
         ("C-c M-q" . my-org-tidy)))

;; transient to change values of #+OPTIONS and other #+<THINGS>
;;
;; (Info-goto-node "(org)Export Settings")
;; (leaf my-org-options
;;   :bind (:org-mode-map
;;          :package org
;;          ("C-c C-." . my-org-options-transient)))

;; very beautiful `org'
;;
;; for example, it show [1/3] like a pie progress. :o
(leaf org-modern
  :ensure t
  :hook org-mode-hook)

(leaf org-autolist
  :ensure t
  ;; :hook org-mode-hook
  :commands org-autolist-mode
  :config
  (run-with-idle-timer 3 nil (lambda () (require 'org-autolist)))
  (add-hook 'org-mode-hook 'org-autolist-mode))

(leaf rorg
  :ensure (rorg :host github :repo "semenInRussia/rorg")
  :bind (:org-mode-map
         :package org
         ("C-c M-s" . rorg-splice-subtree)
         ("C-c C-0" . rorg-wrap-region-or-current-heading)
         ("C-c M-(" . rorg-wrap-region-or-current-heading)
         ("C-c C-{" . rorg-forward-slurp-subtree)
         ("C-c C-}" . rorg-backward-barf-subtree)
         ("C-c {" . rorg-backward-slurp-subtree)
         ("C-c [" . rorg-forward-barf-subtree)))

(defun doom-docs-org-mode () (interactive))

(leaf org-download
  :ensure (org-download :repo "abo-abo/org-download" :host github)
  :after org dired
  :hook (dired-mode-hook . org-download-enable))

;; `org-mode' exporter
(leaf ox
  :custom ((org-export-coding-system . 'utf-8)
           (org-export-with-smart-quotes . t)
           (org-latex-caption-above . '(table))
           (org-latex-default-figure-position . "H")
           (org-latex-image-default-width . "5cm")
           (org-latex-packages-alist .
                                     '(("AUTO" "babel" nil ("pdflatex"))
                                       ("AUTO" "polyglossia" t ("xelatex"))
                                       ("" "cmap" nil ("pdflatex"))
                                       ("" "float" nil
                                        ("pdflatex" "xelatex"))))))

(provide 'my-org)
;;; my-org.el ends here
