;;; my-org-do-tidy.el --- Provide a function to clean up an `org-mode' source -*- lexical-binding: t; -*-

;; Copyright (C) 2023 semenInRussia

;; Author: semenInRussia <hrams205@gmail.com>
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

;; Provide a function to clean up an `org-mode' source.

;;; Code:

(require 'org)

;;;###autoload
(defun my-org-tidy ()
  "Use each of rules tidy org."
  (interactive)
  ;; Clean up metadata
  (my-org-remove-redundant-tags)
  (my-org-check-misformatted-subtree)
  ;; Repair other elements of org-mode
  (my-org-fix-blank-lines t)
  (my-org-align-all-tables))

(defun my-org-remove-redundant-tags ()
  "Remove redundant tags of headlines in current buffer.

A tag is considered redundant if it is local to a headline and
inherited by a parent headline.

Thanks to David Maus!"
  (interactive)
  (when (eq major-mode 'org-mode)
    (save-excursion
      (org-map-entries
       (lambda ()
         (let ((alltags
                (split-string
                 (or (org-entry-get (point) "ALLTAGS") "")
                 ":"))
               local inherited tag)
           (dolist (tag alltags)
             (if (get-text-property 0 'inherited tag)
                 (push tag inherited)
               (push tag local)))
           (dolist (tag local)
             (if (member tag inherited) (org-toggle-tag tag 'off)))))
       t nil))))

(defun my-org-check-misformatted-subtree ()
  "Check misformatted entries in the current buffer."
  (interactive)
  (outline-show-all)
  (org-map-entries
   (lambda ()
     (when (and
            (move-beginning-of-line 2)
            (not (looking-at org-heading-regexp)))
       (if (or
            (and
             (org-get-scheduled-time (point))
             (not (looking-at (concat "^.*" org-scheduled-regexp))))
            (and
             (org-get-deadline-time (point))
             (not (looking-at (concat "^.*" org-deadline-regexp)))))
           (when (y-or-n-p "Fix this subtree? ")
             (message "Call the function again when you're done fixing this subtree.")
             (recursive-edit))
         (message "All subtrees checked."))))))

(defun my-org-fix-blank-lines (&optional prefix)
  "Ensure that Blank lines exist between headings and their contents.

With prefix, operate on whole buffer. Ensures that blank lines
exist after each headings's drawers."
  (interactive "P")
  (org-map-entries
   (lambda ()
     (org-with-wide-buffer
      ;; `org-map-entries' narrows the buffer, which prevents us from seeing
      ;; newlines before the current heading, so we do this part widened.
      (while (not (looking-back "\n\n" nil))
        ;; Insert blank lines before heading.
        (insert "\n")))
     (let ((end (org-entry-end-position)))
       ;; Insert blank lines before entry content
       (forward-line)
       (while (and
               (org-at-planning-p)
               (< (point) (point-max)))
         ;; Skip planning lines
         (forward-line))
       (while (re-search-forward org-drawer-regexp end t)
         ;; Skip drawers. You might think that `org-at-drawer-p' would suffice, but
         ;; for some reason it doesn't work correctly when operating on hidden text.
         ;; This works, taken from `org-agenda-get-some-entry-text'.
         (re-search-forward "^[ \t]*:END:.*\n?" end t)
         (goto-char (match-end 0)))
       (unless (or
                (= (point) (point-max))
                (org-at-heading-p)
                (looking-at-p "\n"))
         (insert "\n"))))
   t
   (if prefix nil 'tree)))

(defun my-org-align-all-tables ()
  (interactive)
  (org-table-map-tables 'org-table-align 'quietly))

(provide 'my-org-do-tidy)
;;; my-org-do-tidy.el ends here
