;;; my-org.el --- My configuration for `org-mode'
;; Copyright (C) 2022-2024 semenInRussia
;; Author: semenInRussia <hrams205@gmail.com>

;;; Commentary:
;; My configuration for `org-mode'
;;; Code:

(require 'my-leaf)
(require 's)
(require 'just)
(require 'my-lib)
(require 'dash)


(leaf org
  ;; :ensure t
  :defun (meow-insert . meow-command)
  :custom ((org-file-apps
            . '(("\\.\\'" . default)
                ("\\.pdf\\'" . "start %s")
                ("\\.png\\'" . "start %s")
                ("\\.jpg\\'" . "start %s")))
           ;; `org-refile'
           (org-refile-use-outline-path . 'file)
           (org-outline-path-complete-in-steps . nil)
           ;; `org' startup
           (org-fold-core-style . 'overlays)
           (org-startup-folded . t)
           (org-startup-indented . t)
           (org-startup-with-inline-images . t))
  :bind (;; NOTE: `org-capture' and `org-agenda' in the my-organization.el file
         ;; ("C-c z c" . org-capture)
         (:org-mode-map
          ("C-c tab"   . org-refile)
          ("C-c C-j"   . org-id-get-create)))
  :config
  (add-hook 'org-mode-hook 'visual-line-mode)
  (add-hook 'org-mode-hook 'aas-activate-for-major-mode)

  (leaf my-org-editing
    :bind (:org-mode-map
           :package org
           ("C-c M-i"   . my-org-insert-image)
           ("C-c M-u"   . my-org-insert-img-at-url)
           ("C-c C-M-w" . my-org-clear-subtree)
           ("C-c C-t"   . my-org-todo)))

  (leaf org-preview
    :ensure (org-preview :repo "karthink/org-preview"
                         :host github)
    :commands org-preview-mode)

  (leaf xenops
    :ensure t
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

  ;; remove some useless things from the current `org-mode' buffer
  (leaf my-org-do-tidy
    :bind (:org-mode-map
           :package org
           ("C-c M-q" . my-org-tidy)))

  ;; transient to change values of #+OPTIONS and other #+<THINGS>
  ;;
  ;; (Info-goto-node "(org)Export Settings")
  (leaf my-org-options
    :bind (:org-mode-map
           :package org
           ("C-c C-." . my-org-options-transient)))

  ;; very beautiful `org'
  ;;
  ;; for example, it show [1/3] like a pie progress. :o
  (leaf org-modern
    :ensure t
    :hook org-mode-hook)

  (leaf org-autolist
    :ensure t
    :hook org-mode-hook)

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

  (defun doom-docs-org-mode () (interactive)))

(leaf org-download
  :ensure (org-download :repo "abo-abo/org-download" :host github)
  :after org
  :hook (dired-mode-hook . org-download-enable))

(provide 'my-org)
;;; my-org.el ends here
