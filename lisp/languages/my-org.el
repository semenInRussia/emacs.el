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

(require 'my-leaf)
(require 's)
(require 'just)
(require 'my-lib)
(require 'dash)
(require 'my-autoformat)

(declare-function meow-insert "meow-command.el")
(declare-function aas-set-snippets "aas.el")
(declare-function my-org-list-item-p "my-org.el")
(declare-function my-org-properties-end-p "my-org.el")
(declare-function my-org-heading-p "my-org.el")
(declare-function my-autoformat-bind-for-major-mode "my-autoformat.el")

(leaf org
  :ensure t
  :defun ((transient-define-prefix . transient)
          (my-org-keywords . my-org)
          (my-org-skip-backward-keyword . my-org)
          (embrace-org-mode-hook . my-org)
          org-current-level)
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
  :defvar (my-org-list-item-prefix-regexp
           my-org-keywords)
  :hook ((org-mode-hook . org-cdlatex-mode))
  :bind (("<f5>" . org-ctrl-c-ctrl-c)
         ("C-c c" . org-capture)
         (:org-mode-map
          ;; Insert anything
          ("C-c M-i"   . my-org-insert-image)
          ("C-c M-u"   . my-org-insert-img-at-url)

          ;; Manipulations with a subtree
          ("C-c C-w"   . my-org-cut)
          ;; heading => plain text
          ;; 8 is * without shift
          ("C-c C-M-w"   . my-org-clear-subtree)
          ([tab] . org-refile)
          ("C-c C-t"   . my-org-todo)
          ("C-c C-j"   . org-id-get-create)))
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
  (add-hook 'org-mode-hook 'visual-fill)
  (add-hook 'org-mode-hook 'visual-line-mode)
  (add-hook 'org-mode-hook 'aas-activate-for-major-mode)
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
       ((just-call-on-prev-line* (equal (pos-bol) (point-min)))
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
                   (meow-insert 0)))
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
    :bind (:org-mode-map
           :package org
           ("C-c M-{" . my-org-to-heading-start))
    :config                             ;nofmt
    (defun my-org-to-heading-start ()
      "Go to the beginning of the heading after activate insert mode."
      (interactive)
      (end-of-line)
      (search-backward-regexp "^\*" nil t)
      (meow-insert)
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

  (leaf consult
    :bind (:org-mode-map
           ([remap consult-imenu] . consult-outline)))

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
      :defun latex/compile-commands-until-done
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
    :bind (:org-mode-map
           :package org
           ("C-c M-q" . my-org-tidy)))

  (leaf my-org-options
    :bind (:org-mode-map
           :package org
           ("C-c C-." . my-org-options-transient)))

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

  ;; (leaf rorg
  ;;   :load-path "~/projects/rorg/"
  ;;   :bind (:org-mode-map
  ;;          :package org
  ;;          ("C-c C-x C-x" . rorg-splice-subtree)
  ;;          ("C-c C-0" . rorg-wrap-region-or-current-heading)
  ;;          ("C-c C-{" . rorg-forward-slurp-subtree)
  ;;          ("C-c C-}" . rorg-backward-barf-subtree)
  ;;          ("{" . rorg-backward-slurp-subtree)
  ;;          ("[" . rorg-forward-barf-subtree))
  ;;   )

  (leaf my-org-drag
    :defun ((add-right-dragger
             add-left-dragger
             add-down-dragger
             add-up-dragger)
            . my-drag)
    :commands (my-drag-org-right
               my-drag-org-left
               my-drag-org-down
               my-drag-org-up)
    :init
    (add-right-dragger 'my-drag-org-right)
    (add-left-dragger 'my-drag-org-left)
    (add-down-dragger 'my-drag-org-down)
    (add-up-dragger 'my-drag-org-up)))

(provide 'my-org)
;;; my-org.el ends here