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

(leaf bibtex
  :major-mode-map ((bibtex-mode)
                   bibtex)
  :custom ((bibtex-align-at-equal-sign . t)
           (bibtex-user-optional-fields .
                                        '(("file" "Link to document file."
                                           ":")))
           (bibtex-dialect . 'biblatex))
  :bind ((:bibtex-mode-map
          ([remap my-format-expression] . 'bibtex-reformat))
         (:my-bibtex-local-map
          ("x"   . 'bibtex-kill-entry)
          ("d"   . 'bibtex-kill-field)
          ("e"   . 'bibtex-validate)
          ("RET" . 'bibtex-url)
          ("v"   . 'bibtex-yank)))
  :config                               ;nofmt
  (leaf helm-bibtex :ensure t)
  (leaf company-bibtex
    :ensure t
    :config (add-to-list 'company-backends 'company-bibtex))
  (leaf bibtex-utils :ensure t))

(leaf latex
  :after tex-mode
  :bind (:my-latex-local-map
         ("." . 'helm-bibtex-with-local-bibliography)))

(provide 'my-bib)
;;; my-bib.el ends here
