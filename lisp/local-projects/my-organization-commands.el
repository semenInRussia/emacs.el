;;; my-organization-commands.el --- Commands for organization in emacs -*- lexical-binding: t; -*-

;; Copyright (C) 2023 semenInRussia

;; Author: $6 <hrams@DESKTOP-CQH054L>
;; Version: 0.0.1
;; Package-Requires: ((dash "2.18.0") (s "1.12.0"))

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

;; Commands for organization in Emacs.

;;; Code:

(require 'org)

;;;###autoload
(defun my-agenda-plan-new-day ()
  "Switch to the new day in my organization system."
  (interactive)
  (my-org-archive-done-and-saw-headings))

;;;###autoload
(defun my-org-archive-done-and-saw-headings ()
  "Archieve all `org-mode' headings which has the label done."
  (save-excursion
    (goto-char (point-min))
    (org-map-entries 'org-archive-subtree "/+DONE" nil)))

;;;###autoload
(defun my-open-main-agenda-file ()
  "Open agenda.org."
  (interactive)
  (find-file "~/agenda.org"))

(provide 'my-organization-commands.el)
;;; my-organization-commands.el ends here
