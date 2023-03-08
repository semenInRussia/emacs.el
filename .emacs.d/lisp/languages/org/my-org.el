;;; my-org.el --- My configuration for `org-mode'

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


;; My configuration for `org-mode'

;;; Code:
(leaf org
  :ensure t
  :defvar (my-org-set-option
           my-org-set-one-of-options
           my-org-set-options
           my-org-get-options
           my-org-get-option-value
           my-org-goto-option
           transient-define-prefix
           my-org-options-transient
           my-org)
  :defun (transient-define-prefix
          my-org-goto-option
          my-org-get-option-value
          my-org-get-options
          my-org-set-options
          my-org-set-one-of-options
          my-org-set-option)
  :custom ((org-refile-use-outline-path . nil)
           (org-refile-targets . '((org-agenda-files :maxlevel . 2)))
           (org-startup-folded . t)
           (org-startup-indented . t)
           (org-startup-with-inline-images . t)
           (org-file-apps .
                          '(("\\.\\'" . default)
                            ("\\.pdf\\'" . "start %s")
                            ("\\.png\\'" . "start %s")
                            ("\\.jpg\\'" . "start %s"))))
  :major-mode-map (org (org-mode))
  :hook ((org-mode-hook . visual-fill)
         (org-mode-hook . aas-activate-for-major-mode)
         (org-mode-hook . org-cdlatex-mode))
  :bind (("<f5>" . org-ctrl-c-ctrl-c)
         (:xah-fly-command-map          ;nofmt
          ("SPC z z" . org-capture))
         (:my-org-local-map             ;nofmt
          ;; Insert anything
          ("l"   . org-insert-link)
          ("s"   . org-schedule)
          ("d"   . org-deadline)
          ("i"   . my-org-insert-image)
          ("u"   . my-org-insert-img-at-url)

          ;; Manipulations with a subtree
          ("c"   . org-copy-subtree)
          ("x"   . my-org-cut)
          ;; heading => plain text
          ;; 8 is * without shift
          ("8"   . org-toggle-heading)
          ("q"   . my-org-indent-subtree)
          ("6"   . org-mark-subtree)
          ("w"   . my-org-clear-subtree)
          ([tab] . org-refile)
          ("1"   . my-org-todo)
          (";"   . org-set-tags-command)
          ("o"   . org-id-get-create)
          ("a"   . org-archive-subtree)
          ("y"   . org-attach)
          ("z"   . org-toggle-comment)

          ;; Change the Priority of a Subtree
          ("," . org-priority)
          ;; Org Babel
          ("b t" . org-babel-tangle)
          ("b f" . org-babel-tangle-file)
          ("b e" . org-babel-execute)
          ("b e" . org-edit-special))
         (:org-src-mode-map             ;nofmt
          ([remap save-buffer] . org-edit-src-exit))
         (:my-org-local-map
          ;; Manipulations with a table
          ("t n" . org-table-create-or-convert-from-region)
          ("="   . org-table-eval-formula)
          ("t f" . my-org-table-eval-formula-in-field)
          ("t i" . org-table-import)
          ("t e" . org-table-export)
          ("t g" . org-table-recalculate)
          ("t x" . org-table-kill-row)
          ("-"   . org-table-insert-hline)
          ("t o" . org-table-toggle-coordinate-overlays)
          ;; sum
          ("+"   . org-table-sum)
          ("t +" . org-table-sum)
          ("t s" . org-table-sum)

          ;; Export
          ("p"   . org-publish)
          ("e"   . org-export-dispatch)

          ;; Context Commands
          ("'"   . org-edit-special)
          ;; other being in the `:org-mode-map' section

          ;; Miscellaneous
          ("SPC" . org-toggle-checkbox)
          ("RET" . org-open-at-point)
          ("r"   . my-org-schedule-to-today)
          ("/"   . org-sparse-tree))

         (:org-mode-map ;; Here continoue of the Context Commands...
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
          ("C-S-l" . 'org-shiftcontrolright)))
  ;; the following code should add some auto activating snippets, for example,
  ;; if I type "exthe", then it should be extended to the "Explore the"
  ;; see `aas-mode'
  :aas (org-mode
        "exthe" "explore the"
        "Exthe" "Explore the"
        "misc " "miscellaneous"
        "Misc " "Miscellaneous"
        "iau" "I am use")
  :config                               ;nofmt
  (leaf my-org-editing
    :commands (my-org-todo
               my-org-indent-subtree my-org-clear-subtree ;nofmt
               my-org-cut my-org-schedule-to-today
               my-org-insert-image))

  (leaf xenops :ensure t :hook (org-mode-hook . xenops-mode))

  (leaf my-org-autoformat
    :config                             ;nofmt
    (defcustom my-org-list-labels-regexps
      '("\\+" "-" "[0-9]+\\.")
      "List of the regexp prefixes indicates a label of a `org-mode' list item.

Label is thing which just decorates a list, but it's not item content, for
example in the following list

- a
- b
- c

Label is \"-\""
      :group 'my
      :type '(repeat string))

    (defcustom my-org-keywords
      '("TODO" "DONE")
      "List of the `org-mode' keywords sush as TODO or DONE."
      :group 'my
      :type '(repeat string))

    (defcustom my-org-list-label-regexp
      (my-regexp-opt-of-regexp my-org-list-labels-regexps)
      "Regexp indicates a item of a `org-mode' list.

Label is thing which just decorates a list, but it's not item content, for
example in the following list

- a
- b
- c

Label is \"-\", you should consider that spaces before label shouldn't be in the
regexp"
      :group 'my
      :type '(repeat string))

    (defcustom my-org-list-item-checkbox-regexp
      "\\[.\\]"
      "Regexp indicates a `org-mode' checkbox."
      :group 'my
      :type 'regexp)

    (defcustom my-org-list-item-prefix-regexp
      (rx line-start
          (0+ " ")
          (regexp my-org-list-label-regexp)
          (? (1+ " ") (regexp my-org-list-item-checkbox-regexp))
          (0+ " "))
      "Regexp indicates a list item."
      :group 'my
      :type 'regexp)

    (my-autoformat-bind-for-major-mode
     'org-mode
     'my-org-sentence-capitalization
     'my-org-list-item-capitalization
     'my-org-heading-capitalization)

    (defun my-org-sentence-capitalization ()
      "Capitalize first letter of a sentence in the `org-mode'."
      (interactive)
      (cond
       ((just-call-on-prev-line*
         (or
          (just-line-is-whitespaces-p)
          (my-org-heading-p)
          (my-org-properties-end-p)
          (my-org-list-item-p)))
        (my-autoformat-sentence-capitalization t))
       ((just-call-on-prev-line* (equal (point-at-bol) (point-min)))
        (my-autoformat-sentence-capitalization))
       (t
        (just-call-on-backward-char*
         (and
          (looking-back my-autoformat-sentence-end nil)
          (looking-at-p "[[:alpha:]]")
          (upcase-char 1))))))

    (defun my-org-heading-p ()
      "Return t, when the cursor located at a `org-mode' heading text."
      ;; NOTE: don't handle cases when bold word located at the beginning of the
      ;; line.  For example:
      ;;
      ;; *bold word*
      ;;
      ;; this function in the above case return t, but excepted nil
      (just-line-prefix-p "*"))

    (defun my-org-list-item-capitalization ()
      "Capitalize first letter of a itemized list item."
      (interactive)
      (just-call-on-backward-char*
       (and
        (looking-at-p "[[:alpha:]]")
        (my-org-list-item-p)
        (looking-back my-org-list-item-prefix-regexp nil)
        (upcase-char 1))))

    (defun my-org-list-item-p ()
      "Return t, when the cursor located at an item of a `org-mode' list."
      (just-line-regexp-prefix-p my-org-list-item-prefix-regexp))

    (defun my-org-properties-end-p ()
      "Get t, if the point placed at the end of `org-mode' subtree properties."
      (string-equal (just-text-at-line nil t) ":END:"))

    (defun my-org-heading-capitalization ()
      "Capitalize first letter of a `org-mode' heading.

When `org-mode' heading has any keyword (like to TODO or DONE) first letter
demotes a first letter after keyword word."
      (interactive "d")
      (when (just-call-on-backward-char*
             (and
              (my-org-heading-p)
              (looking-at-p "[[:alpha:]]")
              (progn
                (skip-chars-backward " ")
                (my-org-skip-backward-keyword)
                (skip-chars-backward " *")
                (bolp))))
        (upcase-char -1)))

    (defun my-org-skip-backward-keyword ()
      "If right at the cursor placed `org-mode' keyword, then skipt it."
      (when (member (thing-at-point 'symbol) my-org-keywords)
        (backward-sexp))))

  (leaf org-download
    :ensure t
    :hook (dired-mode-hook . org-download-enable))

  (leaf org-keys
    :require t
    :custom ((org-use-speed-commands . t)
             (org-speed-commands
              .
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
                ("?" . org-speed-command-help))))
    :bind (:my-org-local-map :package org ("h" . my-org-to-heading-start))
    :config                             ;nofmt
    (defun my-org-to-heading-start ()
      "Go to the beginning of the heading after activate insert mode."
      (interactive)
      (end-of-line)
      (search-backward-regexp "^\*" nil t)
      (xah-fly-insert-mode-activate)
      (unless (eq current-input-method nil) (toggle-input-method)))

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

  (leaf helm-org
    :ensure t
    :after helm
    :bind (:org-mode-map
           :package org
           ([remap helm-imenu] . helm-org-in-buffer-headings)))

  ;; I am bind the command `org-export' with \"SPC l e\" in the root `leaf'
  (leaf ox
    :custom ((org-export-coding-system . 'utf-8)
             (org-export-with-smart-quotes . t)
             (org-latex-caption-above . '(table))
             (org-latex-default-figure-position . "H")
             (org-latex-image-default-width . "5cm")
             (org-latex-packages-alist .
                                       '(("AUTO" "babel" nil ("pdflatex"))
                                         ("AUTO" "polyglossia" t ("xelatex"))
                                         ("" "cmap" nil ("pdflatex"))
                                         ("" "float" nil
                                          ("pdflatex" "xelatex")))))
    :config                             ;nofmt
    (leaf latex-extra
      :ensure t
      :config                           ;nofmt
      (defun my-org-latex-compile (filename &optional snippet)
        "My version of the `ox-latex' way to compile a TeX file.

Using `latex/compile-commands-until-done'

TEXFILE is the name of the file being compiled.  Processing is
done through the command specified in `org-latex-pdf-process',
which see.  Output is redirected to \"*Org PDF LaTeX Output*\"
buffer.

When optional argument SNIPPET is non-nil, TEXFILE is a temporary
file used to preview a LaTeX snippet.  In this case, do not
create a log buffer and do not remove log files.

Return PDF file name or raise an error if it couldn't be
produced."
        (find-file filename)
        ;; if snippet - t, then not clean
        (latex/compile-commands-until-done (not snippet)))

      (defalias 'org-latex-compile 'my-org-latex-compile))

    (leaf ox-json :ensure t :require t)

    (leaf ox-beamer :require t))

  (leaf toc-org
    :ensure t
    :hook (org-mode-hook . toc-org-mode)
    :custom (toc-org-max-depth . 4))

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

  (defun my-org-tidy ()
    "Use each of rules tidy org."
    (interactive)
    ;; Clean up metadata
    (my-org-remove-redundant-tags)
    (my-org-remove-empty-property-drawers)
    (my-org-check-misformatted-subtree)
    ;; Repair other elements of org-mode
    (my-org-fix-blank-lines t)
    (my-org-align-all-tables)))

(leaf org-options
  :doc "Commands to set attribute #+OPTIONS of `org-mode'."
  :after transient
  :leaf-autoload nil
  :bind (:my-org-local-map :package org ("." . my-org-options-transient))
  :init (defvar my-org-options-map
          (make-sparse-keymap)
          "Map for setting `org-mode' options.")
  :transient (my-org-options-transient
              ()
              "Transient for setting of the `org-mode' options."
              [["Set Info"
                ("a" "Set Author" my-org-options-author)
                ("c" "Set Creator" my-org-options-creator)
                ("e" "Set Email" my-org-options-email)
                ("l" "Set Language" my-org-options-language)
                ("t" "Set Title" my-org-options-title)
                ]
               ["Export With..."
                ("'" "Export With Smart Quotes" my-org-options-smart-quotes)
                ("A" "Export With Author" my-org-options-with-author)
                ("DEL"
                 "Export With Archived Trees"
                 my-org-options-with-archived-trees)
                ("C" "Export With Clock" my-org-options-with-clocks)
                ("D" "Export With Drawers" my-org-options-with-drawers)
                ("E" "Export With Email" my-org-options-with-email)
                ("RET"
                 "Export With Preserve-Breaks"
                 my-org-options-with-preserve-breaks)
                ("S" "Export With String" my-org-options-with-special-string)
                ("T" "Export With Table of Contents" my-org-options-with-toc)
                ("^"
                 "Export With Supersripts"
                 my-org-options-with-sub-supersripts)
                ("*" "Export With Emphasize" my-org-options-with-emphasize)
                ("n" "Export with Sections Numbers"
                 my-org-options-with-emphasize)
                ]])
  :init                               ;nofmt
  (defun my-org-options-author (author)
    "Set #+AUTHOR option for the current `org-mode' option to AUTHOR."
    (interactive "sAuthor name, please: ")
    (my-org-set-option "AUTHOR" author))

  (defun my-org-options-author (author)
    "Set #+AUTHOR option for the current `org-mode' option to AUTHOR."
    (interactive "sAuthor name, please: ")
    (my-org-set-option "AUTHOR" author))

  (defun my-org-options-creator (creator)
    "Commands to set attribute #+CREATOR of `org-mode'."
    (interactive "sCreator, please: ")
    (my-org-set-option "CREATOR" creator))

  (defun my-org-options-date (date)
    "Commands to set attribute #+DATE of `org-mode'."
    (interactive "sDate, please: ")
    (my-org-set-option "DATE" date))

  (defun my-org-options-email (email)
    "Commands to set attribute #+EMAIL of `org-mode'."
    (interactive "sEMail, please: ")
    (my-org-set-option "EMAIL" email))

  (defun my-org-options-language (language)
    "Commands to set attribute #+LANGUAGE of `org-mode'."
    (interactive (list (read-string "Language, please: " "ru")))
    (my-org-set-option "LANGUAGE" language))

  (defun my-org-options-title (title)
    "Commands to set attribute #+TITLE of `org-mode'."
    (interactive (list (read-string "Title, please: ")))
    (my-org-set-option "TITLE" title))

  (defun my-org-options-smart-quotes (smart-quotes)
    "If SMART-QUOTES is t, add ':t #+OPTIONS, otherwise ':nil"
    (interactive (list (yes-or-no-p "With smart quotes? ")))
    (my-org-set-one-of-options "'" smart-quotes))

  (defun my-org-options-with-emphasize (with-emphasize)
    "If WITH-EMPHASIZE is t, add *:t #+OPTIONS, otherwise *:nil"
    (interactive (list (yes-or-no-p "With emphasize? ")))
    (my-org-set-one-of-options "*" with-emphasize))

  (defun my-org-options-with-emphasize (with-numbers)
    "If WITH-NUMBERS is t, add num:t #+OPTIONS, otherwise num:nil"
    (interactive (list (yes-or-no-p "With numbers? ")))
    (my-org-set-one-of-options "num" with-numbers))

  (defun my-org-options-with-special-string (with-special-string)
    "If WITH-SPECIAL-STRING is t, add -:t #+OPTIONS, otherwise -:nil"
    (interactive (list (yes-or-no-p "With special string? ")))
    (my-org-set-one-of-options "-" with-special-string))

  (defun my-org-options-with-special-string (with-timestamps)
    "If WITH-TIMESTAMPS is t, add ::t #+OPTIONS, otherwise ::nil"
    (interactive (list (yes-or-no-p "With timestamps? ")))
    (my-org-set-one-of-options "<" with-timestamps))

  (defun my-org-options-with-preserve-breaks (with-preserve-breaks)
    "If WITH-PRESERVE-BREAKS is t, add \\n:t #+OPTIONS, otherwise \\n:nil"
    (interactive (list (yes-or-no-p "With preserve breaks? ")))
    (my-org-set-one-of-options "\\n" with-preserve-breaks))

  (defun my-org-options-with-sub-supersripts (with-sub-supersripts)
    "If WITH-PRESERVE-SUB-SUPERSRIPTS  is t, add ^:t to #+OPTIONS."
    (interactive (list (yes-or-no-p "With sub supersripts? ")))
    (my-org-set-one-of-options "^" with-sub-supersripts))

  (defun my-org-options-with-toc (with-toc)
    "If WITH-TOC is non-nil, then add toc:t to #+OPTIONS, otherwise toc:nil."
    (interactive (list (yes-or-no-p "With table of contents? ")))
    (my-org-set-one-of-options "toc" with-toc))

  (defun my-org-options-with-archived-trees (arch)
    "If ARCH is t, add arch:t to #+OPTIONS, otherwise arch:nil."
    (interactive (list (yes-or-no-p "With archived trees? ")))
    (my-org-set-one-of-options "arch" arch))

  (defun my-org-options-with-author (author)
    "If AUTHOR is t, add author:t to #+OPTIONS, otherwise author:nil."
    (interactive (list (yes-or-no-p "Export with author? ")))
    (my-org-set-one-of-options "author" author))

  (defun my-org-options-with-clocks (clocks)
    "If CLOCKS is t, add c:t to #+OPTIONS, otherwise c:nil."
    (interactive (list (yes-or-no-p "Export with clocks? ")))
    (my-org-set-one-of-options "c" clocks))

  (defun my-org-options-with-drawers (drawers)
    "If DRAWERS is t, add d:t to #+OPTIONS, otherwise d:nil."
    (interactive (list (yes-or-no-p "Export with drawers? ")))
    (my-org-set-one-of-options "d" drawers))

  (defun my-org-options-with-email (email)
    "If DRAWERS is t, add email:t to #+OPTIONS, otherwise email:nil."
    (interactive (list (yes-or-no-p "Export with email? ")))
    (my-org-set-one-of-options "email" email))

  (defun my-org-set-one-of-options (opt val)
    "Set one of options named OPT in form #+TITLE to VAL."
    (my-org-set-options (list (cons opt val))))

  (defun my-org-set-options (alist)
    "Set #+OPTIONS to values and keys from ALIST.

For example: '((\"'\" . nil) (\"*\" . t)), set #+OPTIONS to
':nil *:t"
    (let ((options (my-org-get-options)))
      (->>
       (my-alist-union options alist 'string-equal)
       (--map (format "%s:%s" (car it) (cdr it)))
       (s-join " ")
       (my-org-set-option "OPTIONS"))))

  (defun my-org-get-options ()
    "Get value of #+OPTIONS as an alist.

For example with #+OPTIONS: ':nil *:t, return is '((\"'\" . nil) (\"*\" . t))"
    (let ((options-string (my-org-get-option-value "OPTIONS")))
      (-some->> options-string
        (s-split " ")
        (--map (s-split ":" it))
        (--map (cons (car it) (-second-item it))))))

  (defun my-org-get-option-value (option-name)
    "Get one of options #+OPTIONS."
    (save-excursion
      (when (my-org-goto-option option-name)
        (->>
         (just-text-at-line)
         (s-chop-prefix (s-concat "#+" option-name ":"))
         (s-trim)))))

  (defun my-org-goto-option (option)
    "Search `org-mode' OPTION with form as #+TITLE in `org-mode' buffer.

If not found return nil."
    (goto-char (point-min))
    (let ((init-pos (point))
          (search-result
           (search-forward (s-concat "#+" option) nil t)))
      (unless search-result (goto-char init-pos))
      search-result))

  (defun my-org-set-option (option value)
    "Set an option called OPTION sush as #+TITLE to VALUE."
    (interactive "sName of the option: \nsValue, please: ")
    (goto-char (point-min))
    (when (my-org-goto-option option)
      (delete-region (point-at-bol) (point-at-eol)))
    (insert "#+" option ": " value "\n")))

(leaf org-modern :ensure t :global-minor-mode global-org-modern-mode)

(leaf embrace
  :ensure t
  :hook (org-mode-hook . my-embrace-org-mode-hook)
  :config                             ;nofmt
  (defun my-embrace-org-mode-hook ()
    "Enable `embrace' specially for `org-mode'."
    (embrace-org-mode-hook)
    (setq-local embrace-show-help-p nil)))

(leaf rorg
  :load-path "~/projects/rorg/"
  :bind (:my-org-local-map
         :package org
         ("-" . rorg-splice-subtree)
         ("0" . rorg-wrap-region-or-current-heading)
         ("]" . rorg-forward-slurp-subtree)
         ("}" . rorg-backward-barf-subtree)
         ("{" . rorg-backward-slurp-subtree)
         ("[" . rorg-forward-barf-subtree)))

(provide 'my-org)
;;; my-org.el ends here
