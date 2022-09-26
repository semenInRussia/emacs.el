;;; my-organization.el --- my-organization

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
  (find-file "~/agenda.org"))

(defun fast-exec-agenda-keys ()
  "Get some useful keymaps of  `fast-exec' for agenda."
  (fast-exec/some-commands ("Plane New Day" 'my-agenda-plan-new-day)))

(fast-exec/register-keymap-func 'fast-exec-agenda-keys)
(fast-exec/reload-functions-chain)

(setq org-agenda-files '("~/agenda.org"))

(bind-keys
 ("<f9>" . org-agenda)
 ("S-<f9>" . org-agenda-list)
 :map xah-fly-command-map
 ("SPC <f9>" . org-agenda-list)
 ("SPC i p" . my-open-main-agenda-file))

(defun my-add-org-subtree-to-targets-on-day ()
  "Add  a `org-mode' subtree at the point to the targets on day."
  (interactive)
  (save-excursion
    (let ((subtree-text (my-delete-and-get-text-of-org-subtree)))
      (my-goto-targets-on-day)
      (newline)
      (insert subtree-text)
      (delete-char -1)
      (beginning-of-line)
      (my-org-headline-set-todo-keyword "TODO")
      (org-schedule t (format-time-string "%Y-%m-%d")))))

(defun my-goto-targets-on-day ()
  "Visit `org-mode' subtree of the targets on day."
  (my-open-main-agenda-file)
  (beginning-of-buffer)
  (search-forward "* Targets on Day")
  (forward-char))

(defun my-delete-and-get-text-of-org-subtree (&optional pt)
  "Parse a `org-mode' subtree at the PT, delete it and return text of subtree."
  (or pt (setq pt (point)))
  (org-mark-subtree)
  (prog1
      (just-text-in-region)
    (delete-region (region-beginning) (region-end))))

(defun my-org-headline-set-todo-keyword (new-keyword &optional pt)
  "Set todo keyword of a headline at PT to NEW-KEYWORD.

PT defaults to the current `point'"
  (or pt (setq pt (point)))
  (org-ml-update-subtree-at* pt
    (org-ml-set-property :todo-keyword new-keyword it)))

(bind-keys
 :map xah-fly-command-map
 ("SPC i a" . my-add-org-subtree-to-targets-on-day))

(use-package org-capture
    :custom ((org-capture-templates
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
    :bind ((:map org-capture-mode-map)
           ([remap save-buffer] . 'org-capture-finalize)))

(provide 'my-organization)
;;; my-organization.el ends here