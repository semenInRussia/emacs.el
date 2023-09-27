;;; my-org.el --- My configuration for `org-mode'

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

;; My configuration for `org-mode'

;;; Code:

(require 'my-leaf)
(require 's)
(require 'just)
(require 'my-lib)
(require 'dash)


(leaf org
  :ensure t
  :defun ((aas-set-snippets . aas)
          (meow-insert . meow-command))
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
  ;; the following code should add some auto activating snippets, for example,
  ;; if I type "exthe", then it should be extended to the "Explore the"
  ;; see `aas-mode'
  :aas (org-mode
        "exthe" "explore the"
        "Exthe" "Explore the"
        "misc " "miscellaneous"
        "Misc " "Miscellaneous"
        "iau" "I am use")
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
                                          ("pdflatex" "xelatex")))))
    :config
    (leaf ox-json
      :ensure (ox-json :repo "jlumpe/ox-json" :host github)
      :commands (ox-json-export-to-buffer
                 ox-json-export-to-file)
      :after ox
      ;; load after `ox' (`org-export')
      :init
      (org-export-define-backend 'json
                                 ;; Transcoders
                                 (append
                                  '((template . ox-json-transcode-template)
                                    (plain-text . ox-json-transcode-plain-text)
                                    (headline . ox-json-transcode-headline)
                                    (link . ox-json-transcode-link)
                                    (timestamp . ox-json-transcode-timestamp))
                                  (cl-loop for type in (append org-element-all-elements org-element-all-objects)
                                           collect (cons type #'ox-json-transcode-base)))
                                 ;; Filters
                                 :filters-alist '()
                                 ;; Options
                                 :options-alist
                                 '((:json-data-type-property nil "json-data-type-property" "$$data_type")
                                   (:json-exporters nil nil nil)
                                   (:json-property-types nil nil nil)
                                   (:json-strict nil nil nil)
                                   (:json-include-extra-properties nil nil t))
                                 ;; Menu
                                 :menu-entry
                                 '(?j "Export to JSON" ((?J "As JSON buffer" ox-json-export-to-buffer)
                                                        (?j "To JSON file" ox-json-export-to-file))))))

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

  ;; very beautifull `org'
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
  :hook (dired-mode-hook . org-download-enable))

(provide 'my-org)
;;; my-org.el ends here
