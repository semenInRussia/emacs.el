;;; my-kmacro.el --- my-kmacro

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
(use-package kmacro
    :bind ((:map xah-fly-command-map)
           ("\\"      . 'kmacro-start-or-end-macro)
           ("SPC RET" . 'kmacro-call-macro-or-apply-to-lines)))

(defun kmacro-start-or-end-macro ()
  "If macro record have just started, then stop this record, otherwise start."
  (interactive)
  (if defining-kbd-macro
      (kmacro-end-macro 1)
    (kmacro-start-macro 1)))

(defun kmacro-call-macro-or-apply-to-lines (&optional top bottom)
  "If selected region, then apply macro to selected lines, otherwise call macro."
  (interactive
   (if (use-region-p)
       (list (region-beginning) (region-end))
     (list nil nil)))
  (if (use-region-p)
      (apply-macro-to-region-lines top bottom)
    (kmacro-call-macro 1)))

(provide 'my-kmacro)
;;; my-kmacro.el ends here