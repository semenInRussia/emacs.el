;;; my-deft.el --- My configuration for `deft'

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

;; My configuration for `deft'

;;; Code:
(require 's)

(leaf deft
  :ensure t
  :after org-roam
  :defvar (deft-filter-regexp org-roam-directory)
  :defun deft-filter-update
  :commands deft
  :bind (:deft-mode-map
         ([remap my-kill-rectangle-or-delete-char]
          . 'deft-filter-decrement)
         ([remap syntax-subword-backward-kill]
          . 'deft-filter-decrement-word)
         ([remap xah-paste-or-paste-previous] . 'deft-filter-yank)
         ([remap xah-cut-all-or-region] . 'my-deft-filter-decrement-all))
  :hook (deft-filter-hook . my-deft-titlize-regexp)
  :custom ((deft-directory                      . org-roam-directory)
           (deft-default-extension              . "org")
           (deft-use-filter-string-for-filename . t)
           (deft-use-filename-as-title          . nil))
  :fast-exec ("Manage Notes" 'deft)
  :config                               ;nofmt
  (defun my-deft-titlize-regexp ()
    (when (= (length deft-filter-regexp) 1)
      (setf
       (car deft-filter-regexp)
       (my-upcase-first-char (car deft-filter-regexp))))
    (deft-filter-update))

  (defun my-upcase-first-char (str)
    "Upper case first char of the STR rest chars not take."
    (s-concat (s-upcase (substring str 0 1)) (substring str 1))))

(provide 'my-deft)
;;; my-deft.el ends here
