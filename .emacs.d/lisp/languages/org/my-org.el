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
  :defun ((transient-define-prefix . transient))
  :custom ((org-refile-use-outline-path . nil)
           (org-fold-core-style  . 'overlays)
           (org-refile-targets   . '((org-agenda-files :maxlevel . 2)))
           (org-startup-folded   . t)
           (org-startup-indented . t)
           (org-startup-with-inline-images . t)
           (org-file-apps .
                          '(("\\.\\'" . default)
                            ("\\.pdf\\'" . "start %s")
                            ("\\.png\\'" . "start %s")
                            ("\\.jpg\\'" . "start %s"))))
  :major-mode-map (org (org-mode))
  :hook ((org-mode-hook . visual-fill)
         (org-mode-hook . visual-line-mode)
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
          ("SPC" . my-org-toggle-checkbox)
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
    :commands (my-org-clear-subtree
               my-org-clear-subtree
               my-org-cut
               my-org-indent-subtree
               my-org-indent-subtree
               my-org-insert-image
               my-org-schedule-to-today
               my-org-todo
               my-org-todo))

  (leaf xenops :ensure t :hook org-mode-hook)

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
    :defer-config                       ;nofmt
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

  (leaf my-org-do-tidy
    :bind (:my-org-local-map :package org ("k" . my-org-tidy)))

  (leaf my-org-options
    :bind (:my-org-local-map :package org ("." . my-org-options-transient)))

  (leaf embrace
    :ensure t
    :hook (org-mode-hook . my-embrace-org-mode-hook)
    :config                             ;nofmt
    (defun my-embrace-org-mode-hook ()
      "Enable `embrace' specially for `org-mode'."
      (embrace-org-mode-hook)
      (setq-local embrace-show-help-p nil)))

  (leaf org-table-sticky-header :ensure t :hook org-mode-hook)
  (leaf org-autolist :ensure t :hook org-mode-hook)

  (leaf rorg
    :load-path "~/projects/rorg/"
    :bind (:my-org-local-map
           :package org
           ("-" . rorg-splice-subtree)
           ("0" . rorg-wrap-region-or-current-heading)
           ("]" . rorg-forward-slurp-subtree)
           ("}" . rorg-backward-barf-subtree)
           ("{" . rorg-backward-slurp-subtree)
           ("[" . rorg-forward-barf-subtree))))

(provide 'my-org)
;;; my-org.el ends here
