;;; my-bib.el --- My configuration for bibliography management -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2023 semenInRussia

;; Author: semenInRussia <hrams205@gmail.com>

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; My configuration for bibliography management.

;;; Code:

(require 'my-leaf)

(require 'dash)


(leaf bibtex
  :custom ((bibtex-align-at-equal-sign  . t)
           (bibtex-user-optional-fields .
                                        '(("file" "Link to document file."
                                           ":")))
           (bibtex-dialect . 'biblatex))
  :bind ((:bibtex-mode-map
          ([remap my-format-expression] . 'bibtex-reformat)))
  :config
  (leaf bibtex-utils
    :ensure (bibtex-utils :repo "plantarum/bibtex-utils" :host github)))

;; load `citar', but for `embark'
(leaf citar
  :ensure t
  ;; :defun (unless (functionp 'citar-capf) (defalias 'citar-capf #'ignore))
  ;; :init (add-to-list 'completion-at-point-functions 'citar-capf)
  :hook (org-mode-hook . citar-capf-setup)
  :bind (:org-mode-map
         :package org
         ("C-c C-S-b" . 'org-cite-insert))
  :custom `((org-cite-global-bibliography . '("~/bib.bib"))
            (org-cite-insert-processor   . 'citar)
            (org-cite-follow-processor   . 'citar)
            (org-cite-activate-processor . 'citar))
  :defvar (org-cite-global-bibliography
           citar-indicators
           citar-bibliography
           org-roam-directory)
  :config
  ;; `citar' have integration with `all-the-icons',
  ;; but not with `nerd-icons'

  ;; TODO: `nerd-icons' + `citar'

  ;; (defvar my-citar-indicator-notes-nerd-icons
  ;;   (citar-indicator-create
  ;;    :symbol (nerd-icons-codicon "nf-cod-notebook")
  ;;    :function #'citar-has-notes
  ;;    :padding "  "
  ;;    :tag "has:notes"))

  ;; (setq citar-indicators
  ;;       (list
  ;;        ;; plain text
  ;;        citar-indicator-files
  ;;        my-citar-indicator-notes-nerd-icons))

  (with-eval-after-load 'org
    (setq citar-bibliography org-cite-global-bibliography))

  ;; `citar' + `embark'
  (leaf citar-embark
    :ensure t
    :after (citar embark)
    :global-minor-mode citar-embark-mode)

  ;; `org-roam' + `citar':
  (leaf parsebib :ensure t)

  (leaf citar-org-roam
    :ensure t
    :after org-roam
    :global-minor-mode citar-org-roam-mode
    :config
    ;; `org-roam' has your own the bibliography.bib file
    ;; -- (in my config)
    (with-eval-after-load 'org-roam
      (add-to-list 'org-cite-global-bibliography (f-join org-roam-directory "bibliography.bib"))
      (add-to-list 'citar-bibliography (f-join org-roam-directory "bibliography.bib")))))

(provide 'my-bib)
;;; my-bib.el ends here
