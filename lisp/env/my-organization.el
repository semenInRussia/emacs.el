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

;; My configuration for the my organization it inspired with GTD.  Some things I
;; grabbed from the rougier tutorial emacs-gtd here link:
;;
;; https://www.labri.fr/perso/nrougier/GTD/index.html

;;; Code:

(require 'my-leaf)
(require 'just)
(require 'dash)


(leaf org-agenda
  :bind ("C-c a" . org-agenda)
  :custom ((org-agenda-files .
                             '("~/agenda.org"
                               "~/projects.org"
                               "~/inbox.org"
                               "~/tasks-archive/task-archive.org"))
           (org-agenda-hide-tags-regexp . ".")
           (org-agenda-span . 14)
           (org-agenda-prefix-format
            . '((agenda . " %i %-12:c%?-12t% s")
                (todo   . " %i %-12:c")
                (tags   . " %i %-12:c")
                (search . " %i %-12:c")))))

(leaf org-refile
  :custom (org-refile-targets . '(;; the current file
                                  (nil :maxlevel . 9)
                                  ;; some headings from agenda.org
                                  ;;
                                  ;; it consists of Maybe, Films and Books to Read
                                  ("~/agenda.org" :regexp . "\\(?:Maybe\\|Films\\|Books\\)")
                                  ;; GTD projects
                                  ("~/projects.org" :level . 2)))
  :config
  ;; save all agenda files, after I do refiling
  (defun my-gtd-save-org-buffers ()
    "Save `org-agenda-files' buffers without user confirmation.
See also `org-save-all-org-buffers'"
    (interactive)
    (message "Saving org-agenda-files buffers...")
    (save-some-buffers t (lambda ()
                           (when (member (buffer-file-name) org-agenda-files)
                             t)))
    (message "Saving org-agenda-files buffers... done"))

  ;; Add it after refile
  (advice-add 'org-refile :after
              (lambda (&rest _)
                (my-gtd-save-org-buffers))))

(leaf nano-agenda
  :ensure t
  :after org-agenda
  :bind (:org-agenda-keymap
         :package org-agenda
         ("a" . nano-agenda))
  :custom
  (org-agenda-custom-commands
   . '(("g" "Get Things Done (GTD)"
        ((agenda
          ""
          ((org-agenda-skip-function
            '(org-agenda-skip-entry-if 'deadline))
           (org-deadline-warning-days 0)))
         (todo
          "NEXT"
          ((org-agenda-skip-function
            '(org-agenda-skip-entry-if 'deadline))
           (org-agenda-prefix-format "  %i %-12:c [%e] ")
           (org-agenda-overriding-header "\nTasks\n")))
         (agenda
          nil
          ((org-agenda-entry-types '(:deadline))
           (org-agenda-format-date "")
           (org-deadline-warning-days 7)
           (org-agenda-skip-function
            '(org-agenda-skip-entry-if 'notregexp "\\* NEXT"))
           (org-agenda-overriding-header "\nDeadlines")))
         (tags-todo
          "inbox"
          ((org-agenda-prefix-format "  %?-12t% s")
           (org-agenda-overriding-header "\nInbox\n")))
         (tags
          "CLOSED>=\"<today>\""
          ((org-agenda-overriding-header "\nCompleted today\n"))))))))

(leaf org-capture
  :bind ("C-c z c" . org-capture)
  :custom ((org-capture-templates
            . '(("d"
                 "Target on Day"
                 entry
                 (file+headline "~/agenda.org" "Targets on Day")
                 "* TODO %?\n  SCHEDULED: %t\n  \n")
                ("i"
                 "Inbox Entry"
                 entry
                 (file "~/inbox.org")
                 "* TODO %?\n  \n")
                ("f"
                 "Film for See"
                 entry
                 (file+headline "~/agenda.org" "Films")
                 (function my-films-format-as-org-heading)))))
  :bind (:org-capture-mode-map
         ([remap save-buffer] . org-capture-finalize)))

;; my project: sync inbox (see GTD terms) with messages from the Telegram chat
(leaf tg-inbox
  :load-path "~/projects/tg-inbox/"
  :when (file-exists-p "~/projects/tg-inbox/")
  :commands tg-inbox-sync
  :config (add-hook 'tg-inbox-sync-post-hook #'tg-inbox-maybe-send-done-msg))

(provide 'my-organization)
;;; my-organization.el ends here
