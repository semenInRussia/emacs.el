;;; my-indent.el --- My configuration for the indentation

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

;; My configuration for the indentation

;;; Code:

(require 'my-leaf)
(require 'my-xah)

;; disable tabs
(setq-default indent-tabs-mode nil)

(defun my-indent-line-or-region ()
  "If text selected, then indent it, otherwise indent current line."
  (interactive)
  (save-excursion
    (if (use-region-p)
        (indent-region (region-beginning) (region-end))
      (funcall indent-line-function))))

(leaf-keys
 ((prog-mode-map ("RET" . newline-and-indent))
  (xah-fly-command-map
   :package xah-fly-keys
   ("RET"   . newline-and-indent)
   ("q"     . my-indent-line-or-region)
   ("SPC q" . join-line))))

(provide 'my-indent)
;;; my-indent.el ends here
