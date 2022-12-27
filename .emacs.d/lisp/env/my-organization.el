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

(require 'just)

(leaf org-agenda                        ;nofmt
  :custom ((org-agenda-files .
                             '("~/agenda.org"
                               "~/agnia.org"
                               "~/tasks-archive/task-archive.org"))
           (org-agenda-span . 14))
  :bind (("<f9>"      . org-agenda)
         ("S-<f9>"    . org-agenda-list)
         (:xah-fly-command-map
          :package xah-fly-keys
          ("SPC <f9>" . org-agenda-list)
          ("SPC i p"  . my-open-main-agenda-file)))
  :config                               ;nofmt
  (defun my-agenda-plan-new-day ()
    "Switch to the new day in my organization system."
    (interactive)
    (my-org-archive-done-and-saw-headings))

  (defun my-org-archive-done-and-saw-headings ()
    "Archieve all `org-mode' headings which has the label done."
    (save-excursion
      (goto-char (point-min))
      (org-map-entries 'org-archive-subtree "/+DONE" nil)))

  (defun my-open-main-agenda-file ()
    "Open agenda.org."
    (interactive)
    (find-file "~/agenda.org")))

(leaf org-agenda
  :after (org fast-exec)
  :fast-exec ("Plane New Day" 'my-agenda-plan-new-day)

  (defun my-add-org-subtree-to-targets-on-day ()
    "Add  a `org-mode' subtree at the point to the targets on day."
    (interactive)
    (save-excursion
      (let ((subtree-text (my-delete-and-get-text-of-org-subtree)))
        (my-goto-targets-on-day)
        (newline)
        (insert subtree-text)
        (delete-char -1)
        (org-schedule t (format-time-string "%Y-%m-%d")))))

  (defun my-goto-targets-on-day ()
    "Visit `org-mode' subtree of the targets on day."
    (my-open-main-agenda-file)
    (goto-char (point-min))
    (search-forward "* Targets on Day")
    (forward-char))

  (defun my-delete-and-get-text-of-org-subtree (&optional pt)
    "Parse a `org-mode' subtree at the PT, delete it and return text of subtree."
    (or pt (setq pt (point)))
    (org-mark-subtree)
    (prog1
        (just-text-in-region)
      (delete-region (region-beginning) (region-end)))))

(leaf-keys
 (xah-fly-command-map                   ;nofmt
  :package xah-fly-keys
  ("SPC i a" . my-add-org-subtree-to-targets-on-day)))

(leaf org-capture
  :commands org-capture
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
