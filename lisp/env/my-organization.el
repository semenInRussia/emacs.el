;;; my-organization.el --- My configuration for the my organization

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

;; My configuration for the my organization

;;; Code:

(require 'my-leaf)
(require 'just)
(require 'dash)


(declare-function org-schedule "org.el")
(declare-function org-mark-subtree "org.el")

(declare-function my-open-main-agenda-file "my-organization-commands.el")
(declare-function my-goto-targets-on-day "my-organization.el")
(declare-function my-delete-and-get-text-of-org-subtree "my-organization.el")


(leaf org-agenda
  :custom ((org-agenda-files .
                             '("~/agenda.org"
                               "~/tasks-archive/task-archive.org"))
           (org-agenda-span . 14))
  :bind ("C-c a" . org-agenda))

(leaf nano-agenda
  :ensure t
  :after org-agenda
  :bind (:org-agenda-keymap
         :package org-agenda
         ("a" . nano-agenda)))

(leaf org-capture
  :bind ("C-c z c" . org-capture)
  :custom ((org-capture-templates
            .
            '(("d"
               "Target on Day"
               entry
               (file+headline "~/agenda.org" "Targets on Day")
               "* TODO %?\n  SCHEDULED: %t\n  \n")
              ("w"
               "Target on Week"
               entry
               (file+headline "~/agenda.org" "Targets on Week")
               "* TODO %?\n  \n")
              ("f"
               "Film for See"
               entry
               (file+headline "~/agenda.org" "Films")
               (function my-films-format-as-org-heading)))))
  :bind (:org-capture-mode-map
         ([remap save-buffer] . org-capture-finalize)))

(provide 'my-organization)
;;; my-organization.el ends here
