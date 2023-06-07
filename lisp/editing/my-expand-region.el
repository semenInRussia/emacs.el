;;; my-expand-region.el --- My configuration of the `expand-region'

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

;; My configuration of the `expand-region'

;;; Code:

(require 'my-leaf)
(require 'smartparens)
(require 'dash)
(require 'just)

(leaf expand-region
  :ensure t
  :defun (((sp-get-sexp sp-get)
           . smartparens))
  :bind (("C-x C-<SPC>" . er/expand-region)
         ("C-x C-@" . my-mark-inner-or-expand-region))
  :config                             ;nofmt
  (defun my-mark-inner-or-expand-region ()
    "If text is selected, expand region, otherwise mark inner of brackets."
    (interactive)
    (if (use-region-p)
        (call-interactively 'er/expand-region)
      (--when-let
          (sp-get-sexp)
        (sp-get it (just-mark-region :beg-in :end-in))))))

(provide 'my-expand-region)
;;; my-expand-region.el ends here
