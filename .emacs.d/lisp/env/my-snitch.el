;;; my-snitch.el --- my-snitch

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
(defun run-command-recipe-snitch ()
  "Recipes of `run-command' for snitch."
  (when (f-directory-p (f-join (projectile-acquire-root)
                               ".git"))
    (list
     (list :command-name "sntich-list"
           :display "See List of TODOs from via Snitch"
           :command-line "snitch list")
     (list :command-name "sntich-report"
           :display "Report to VC TODOs of Project via Snitch"
           :command-line "snitch list"))))

(add-to-list 'run-command-recipes 'run-command-recipe-snitch)

(provide 'my-snitch)
;;; my-snitch.el ends here