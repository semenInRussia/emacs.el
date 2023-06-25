;;; my-bib.el --- My configuration of `bib' -*- lexical-binding: t; -*-

;; Copyright (C) 2022 semenInRussia

;; Author: semenInRussia <hrams205@gmail.com>
;; Version: 0.1
;; Homepage: https://github.com/semeninrussia/emacs.el

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; My configuration of `bib'.

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
  :config                               ;nofmt
  (leaf bibtex-utils :ensure t))

;; (leaf company-bibtex
;; :ensure (company-bibtex :repo "semenInRussia/company-bibtex")
;;   :defvar company-backends
;;   :hook (org-mode-hook . company-bibtex-org-mode-hook)
;;   :custom (company-bibtex-org-citation-regex . "\\(ebib:\\|cite:@\\)")
;;   :config (add-to-list 'company-backends 'company-bibtex)
;;   (defun company-bibtex-org-mode-hook ()
;;     "Hook for `org-mode' enabling `comapany-bibtex' for current buffer."
;;     (interactive "P")
;;     (->>
;;      company-backends
;;      (--remove
;;       (and (listp it) (eq (car it) 'company-bbdb)))
;;      (setq-local company-backends))))

(provide 'my-bib)
;;; my-bib.el ends here
