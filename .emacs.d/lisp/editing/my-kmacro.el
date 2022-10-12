;;; my-kmacro.el --- My configuration for the macros

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

;; My configuration for the macros

;;; Code:
(leaf kmacro
  :bind (:xah-fly-command-map
         :package xah-fly-keys
         ("\\"      . kmacro-start-or-end-macro)
         ("SPC RET" . kmacro-call-macro-or-apply-to-lines))
  :config                             ;nofmt
  (defun kmacro-call-macro-or-apply-to-lines (&optional top bottom)
    "If has region, call macro to lines beetween TOP BOTTOM, else call macro."
    (interactive
     (and
      (use-region-p)
      (list (region-beginning) (region-end))))
    (if (use-region-p)
        (apply-macro-to-region-lines top bottom)
      (kmacro-call-macro 1)))

  (defun kmacro-start-or-end-macro ()
    "Start macro record (if not started) or stop record."
    (interactive)
    (if defining-kbd-macro
        (kmacro-end-macro 1)
      (kmacro-start-macro 1))))

(provide 'my-kmacro)
;;; my-kmacro.el ends here
