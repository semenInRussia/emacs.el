;;; my-organization.el --- My configuration for my organization

;; Copyright (C) 2022-2023 Semen Khramtsov

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
                             '("~/agenda/agenda.org"
                               "~/agenda/projects.org"
                               "~/agenda/inbox.org"
                               "~/agenda/tasks-archive/task-archive.org"))
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
                                  ("~/agenda/agenda.org" :regexp . "\\(?:Maybe\\|Films\\|Books\\)")
                                  ;; GTD projects
                                  ("~/agenda/projects.org" :level . 2)
                                  ;; my target is to win an ICT
                                  ;; olympiad, so I learn the computer
                                  ;; science
                                  ("~/agenda/CS.org")))
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
  :ensure (tg-inbox :repo "semenInRussia/tg-inbox" :host github)
  :commands tg-inbox-sync
  :config (add-hook 'tg-inbox-sync-post-hook #'tg-inbox-maybe-send-done-msg))

(provide 'my-organization)
;;; my-organization.el ends here
