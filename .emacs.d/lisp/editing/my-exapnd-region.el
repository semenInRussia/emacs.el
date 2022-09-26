;;; my-exapnd-region.el --- my-exapnd-region

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

;;; Code:
(use-package expand-region
    :ensure t
    :bind ((:map xah-fly-command-map)
           ("1" . er/expand-region)
           ("9" . mark-inner-or-expand-region)
           ("m" . sp-backward-up-sexp))
    :config
    (defun mark-inner-or-expand-region ()
      "If text is selected, expand region, otherwise mark inner of brackets."
      (interactive)
      (if (use-region-p)
          (call-interactively 'er/expand-region)
        (progn
          (-when-let (ok (sp-get-sexp))
            (sp-get ok
              (set-mark :beg-in)
              (goto-char :end-in)))))))

(provide 'my-exapnd-region)
;;; my-exapnd-region.el ends here