;;; my-org.el --- my-org

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
(use-package org
    :custom (org-refile-use-outline-path nil)
    (org-refile-targets '((org-agenda-files :maxlevel . 2)))
    :bind ((:map xah-fly-command-map)
           ("SPC z" . 'org-capture)
           (:map my-org-local-map)

           ;; Insert anything
           ("l"   . 'org-insert-link)
           ("s"   . 'org-schedule)
           ("d"   . 'org-deadline)
           ("i"   . 'my-org-insert-image)
           ("u"   . 'my-org-insert-img-at-url)

           ;; Manipulations with a subtree
           ("c"   . 'org-copy-subtree)
           ("x"   . 'org-cut-subtree)
           ("q"   . 'my-org-indent-subtree)
           ("6"   . 'org-mark-subtree)
           ("w"   . 'my-org-clear-subtree)
           ([tab] . 'org-refile)
           ("1"   . 'my-org-todo)
           (";"   . 'org-set-tags-command)
           ("a"   . 'org-archive-subtree)
           ("z"   . 'org-toggle-comment)

           ;; Org Babel
           ("b t" . 'org-babel-tangle)
           ("b f" . 'org-babel-tangle-file)
           ("b e" . 'org-babel-execute)
           ("b e" . 'org-edit-special)
           (:map org-src-mode-map)
           ([remap save-buffer] . 'org-edit-src-exit)

           (:map my-org-local-map)

           ;; Manipulations with a table
           ("t n" . 'org-table-create-or-convert-from-region)
           ("="   . 'org-table-eval-formula)
           ("t f" . 'my-org-table-eval-formula-in-field)
           ("t i" . 'org-table-import)
           ("t e" . 'org-table-export)
           ("t g" . 'org-table-recalculate)
           ("t x" . 'org-table-kill-row)
           ("-"   . 'org-table-insert-hline)
           ("t o" . 'org-table-toggle-coordinate-overlays)
           ;; sum
           ("+"   . 'org-table-sum)
           ("t +" . 'org-table-sum)
           ("t s" . 'org-table-sum)

           ;; Export
           ("p"   . 'org-publish)
           ("e"   . 'org-export-dispatch)

           ;; Context Commands
           ("g"   . 'org-ctrl-c-ctrl-c)
           ("'"   . 'org-edit-special)
           ;; other being in the `org-mode-map' section

           ;; Miscellaneous
           ("j"   . 'org-latex-preview)
           ("SPC" . 'org-toggle-checkbox)
           ("RET" . 'org-open-at-point)
           ("r"   . 'my-org-schedule-to-today)
           ("/"   . 'org-sparse-tree)

           (:map org-mode-map)
           ;; Here continoue of the Context Commands...
           ;; M-
           ("M-j"   . 'org-metaleft)
           ("M-i"   . 'org-metaup)
           ("M-k"   . 'org-metadown)
           ("M-l"   . 'org-metaright)
           ;; M-S-
           ("M-S-j" . 'org-shiftmetaleft)
           ("M-S-i" . 'org-shiftmetaup)
           ("M-S-k" . 'org-shiftmetadown)
           ("M-S-l" . 'org-shiftmetaright)
           ;; C-S-
           ("C-S-j" . 'org-shiftcontrolleft)
           ("C-S-i" . 'org-shiftcontrolup)
           ("C-S-k" . 'org-shiftcontroldown)
           ("C-S-l" . 'org-shiftcontrolright))
    :config (my-define-local-major-mode-map 'org '(org-mode)))

(defun my-org-clear-subtree ()
  "Kill subtree at the position, and activate insertion mode."
  (org-cut-subtree)
  (xah-fly-insert-mode-activate))

(defun my-org-table-eval-formula-in-field ()
  "Eval formula with `orgtbl-mode' syntax for the current field of the table."
  (interactive)
  (org-table-eval-formula '(4)))

(defun my-org-schedule-to-today ()
  "Scheduale a `org-mode' heading to today."
  (interactive)
  (org-schedule t (format-time-string "%Y-%m-%d")))

(defun my-org-indent-subtree ()
  "Indent current the `org-mode' subtree at current position."
  (interactive)
  (save-excursion
    (org-mark-subtree)
    (indent-region (region-beginning) (region-end))))

(defun my-org-todo ()
  "My version of the `org-todo'.

Different with the original functions is that this function can be repeated by
pressing of the previous last pressed char.  So if functions is bound to
\"SPC l 1\", that after pressing that user can press \"1\" and this command will
be repeated"
  (interactive)
  (call-interactively #'org-todo)
  (one-shot-keybinding "1" 'my-org-todo))

(defun my-org-insert-image (filename &optional caption)
  "Insert a image with FILENAME.

By default, caption for the image don't inserts, but if CAPTION is a
string, then define caption of the image to the CAPTION.

In the interactive, If the region is active, the FILENAME will be text
in the region."
  (interactive
   (list (my-org-read-image-filename) (my-org-read-image-caption)))
  (just-ensure-empty-line)
  (when caption ;nofmt
    (insert "#+CAPTION: " caption)
    (newline))
  (insert "[[" filename "]]"))

(defun my-org-read-image-filename ()
  "Read a image filename.

If the region is active, then return text in the region as filename, otherwise
return filename readed from the minibuffer."
  (or
   (just-text-in-region)
   (read-file-name "Please, choose image to insert: ")))

(defun my-org-read-image-caption ()
  "Read a image caption from the minibuffer.

If the user insert any caption, return its, otherwise return nil."
  (let ((caption (read-string "Caption for the image, please: ")))
    (unless (s-blank-p caption) caption)))

(defcustom my-org-default-images-dir "./images/"
  "Default directory for images of a `org-mode' document."
  :type 'string)

(defun my-org-insert-img-at-url (url &optional new-file-name images-dir caption)
  "Insert org image at URL, download it into IMAGES-DIR with name NEW-FILE-NAME.

If the region is active return it, otherwise read URL from the minibuffer.
If caption isn't empty string, then insert image with the caption CAPTION."
  (interactive (my--get-arguments-for-org-insert-img-at-url))
  (or images-dir (setq images-dir my-org-default-images-dir))
  (let ((new-filename (f-join images-dir new-file-name)))
    (my-download url new-filename)
    (my-org-insert-image new-filename caption)))

(defun my--get-arguments-for-org-insert-img-at-url ()
  "Get arguments from the user for `my-org-insert-img-at-url'."
  (let* ((url (my-read-image-url))
         (new-file-name (my-org-read-new-image-at-url-file-name url))
         (images-dir (my-org-read-images-dir))
         (caption (my-org-read-image-caption)))
    (list url new-file-name images-dir caption)))

(defun my-org-read-images-dir ()
  "Read directory path for downloading of the image."
  (read-directory-name "Image will download into directory:"
                       my-org-default-images-dir))

(defun my-org-read-new-image-at-url-file-name (url)
  "Read from the minibuffer new file name for the image at URL."
  (read-string "Image will be downloaded with name: "
               (my-uri-of-url url)))

(defun my-download (url new-filename)
  "Download file at URL as file with NEW-FILENAME."
  (make-directory (f-dirname new-filename) t)
  (url-copy-file url new-filename t))

(add-hook 'org-mode-hook
          (lambda () (call-interactively 'visual-fill)))

(add-hook 'org-mode-hook 'aas-activate-for-major-mode)

(aas-set-snippets 'org-mode
  "exthe" "explore the"
  "misc " "miscellaneous"
  "Iau" "I am use")

(my-use-autoformat-in-mode 'org-mode org-sentence-capitalization)

(defvar autoformat-org-title-line-start-regexp
  (rx
   (or
    (1+ "*")                      ; Headline
    (seq (0+ " ")                 ; List Items
         "-"                      ;    itemized
         (seq digit ".")          ;    enumerated
         )
    (seq "#+"                     ; Attributes which start with #+
         (or "AUTHOR" "TITLE")
         ":"
         (0+ " ")))
   (1+ " ")))

(defvar autoformat-org-line-for-capitalization-regexp
  (rx line-start
      (optional
       (regexp autoformat-org-title-line-start-regexp)
       (optional (or "TODO" "DONE") (1+ " ")))
      letter)
  "Regular expression indicates the necessary of the last word capitalization.")

(defun autoformat-org-sentence-capitalization ()
  "Capitalize first letter of a sentence in the `org-mode'."
  (interactive)
  (autoformat-sentence-capitalization)
  (when (and
         (autoformat-org-at-non-text-line-p)
         (looking-back autoformat-org-line-for-capitalization-regexp))
    (undo-boundary)
    (capitalize-word -1)))

(defun autoformat-org-at-non-text-line-p ()
  "Return t, when the cursor being at non-text position for `org-mode'."
  (or
   (just-line-regexp-prefix-p autoformat-org-title-line-start-regexp)
   (just-call-on-prev-line*
    (just-line-regexp-prefix-p autoformat-org-title-line-start-regexp))))

(use-package wikinforg :ensure t)

(use-package org-download
    :ensure t
    :hook (dired-mode-hook . org-download-enable))

(use-package org-keys
    :after (org)
    :custom (org-use-speed-commands
             (lambda ()
               (and (not (bobp)) (looking-back "^\**"))))
    (org-speed-commands-default
     '(("k" . org-forward-heading-same-level)
       ("i" . org-backward-heading-same-level)
       ("j" . org-previous-visible-heading)
       ("l" . org-next-visible-heading)
       ("h" . my-org-goto-parent)
       ("z" . org-toggle-comment)
       ("x" . org-cut-subtree)
       ("d" . org-deadline)
       ("s" . org-schedule)
       ("w" . my-org-schedule-to-today)
       (" " . my-add-org-subtree-to-targets-on-day)
       ("'" . org-toggle-narrow-to-subtree)
       ("f" .
        (progn
          (skip-chars-forward "*")
          (forward-char)
          (xah-fly-command-mode-activate)))
       ("B" . org-previous-block)
       ("F" . org-next-block)
       ("g" . (org-refile t))
       ("c" . org-cycle)
       ("=" . org-columns)
       ("I" . org-metaup)
       ("K" . org-metadown)
       ("o" . org-metaright)
       ("u" . org-metaleft)
       ("O" . org-shiftmetaright)
       ("U" . org-shiftmetaleft)
       ("n"
        .
        (progn
          (forward-char 1)
          (call-interactively 'org-insert-heading-respect-content)))
       ("a" . org-archive-subtree-default-with-confirmation)
       ("6" . org-mark-subtree)
       ("t" . org-todo)
       ("," . (org-priority))
       ("0" . (org-priority 32))
       ("1" . (org-priority 65))
       ("2" . (org-priority 66))
       ("3" . (org-priority 67))
       (":" . org-set-tags-command)
       ("e" . org-set-effort)
       ("E" . org-inc-effort)
       ("/" . org-sparse-tree)
       ("?" . org-speed-command-help)))
    :bind ((:map my-org-local-map)
           ("h" . 'my-org-to-heading-start))
    :config (defun my-org-to-heading-start
                ()
              "Go to the beginning of the heading after activate insert mode."
              (interactive)
              (end-of-line)
              (search-backward-regexp "^\*" nil t)
              (xah-fly-insert-mode-activate))

    (defun my-org-goto-parent ()
      "Go to the parent of the `org-mode' heading at the cursor."
      (interactive)
      (->>
       (->
        (org-current-level)
        (1-)
        (s-repeat "*")
        (regexp-quote))
       (s-prepend "^")
       (s-append " ")
       (re-search-backward))))

(use-package helm-org
    :ensure t
    :bind ((:map org-mode-map)
           ([remap helm-imenu] . helm-org-in-buffer-headings)))

(use-package ox

    ;; bound of the keybinding for the `org-export' is already defined in the
    ;; start of the Heading "Org"

    :custom ((org-export-coding-system     'utf-8)
             (org-export-with-smart-quotes t)))

(use-package ox-latex
    :after (ox)
    :custom ((org-latex-listings 'minted)
             (org-latex-caption-above '(table image))
             (org-latex-packages-alist '(("AUTO" "babel" nil ("pdflatex"))))))

(use-package org-ql :ensure t :config (require 'org-ql))

(use-package helm-org-ql :ensure t)

(defun fast-exec-org-ql-keys ()
  "Get some useful keymaps of  `fast-exec' for org-ql."
  (fast-exec/some-commands
   ("Search Org Files via Org Query Language" 'org-ql-search)))

(fast-exec/register-keymap-func 'fast-exec-org-ql-keys)
(fast-exec/reload-functions-chain)

(use-package org-cliplink
    :ensure t
    :bind ((:map my-org-local-map)
           ("i" . org-cliplink)))

(use-package toc-org
    :ensure t
    :hook (org-mode . toc-org-mode)
    :custom (toc-org-max-depth 4))

(use-package org
    :custom (org-startup-folded t)
    (org-startup-indented t)
    (org-startup-with-inline-images t))

(defun turn-on-org-cdlatex-mode ()
  "Turn on `org-cdlatex-mode'."
  (interactive)
  (org-cdlatex-mode t))

(add-hook 'org-mode-hook #'turn-on-org-cdlatex-mode)

(defun my-org-remove-empty-property-drawers ()
  "Remove all empty property drawers in current file."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward ":PROPERTIES:" nil t)
      (save-excursion
        (org-remove-empty-drawer-at (match-beginning 0))))))

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
  (show-all)
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
  "Ensure that Blank lines exist between headings and between headings and their contents.
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

(defun my-org-tidy ()
  "Use each of rules tidy org."
  (interactive)
  ;; Clean up metadata
  (my-org-remove-redundant-tags)
  (my-org-remove-empty-property-drawers)
  (my-org-check-misformatted-subtree)
  ;; Repair other elements of org-mode
  (my-org-fix-blank-lines t)
  (my-org-align-all-tables))

(use-package org
    :bind ((:map my-org-local-map)
           ("k" . 'my-org-tidy)))

(provide 'my-org)
;;; my-org.el ends here
