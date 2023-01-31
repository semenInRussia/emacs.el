;;; my-latex.el --- My config for LaTeX

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

;; My config for LaTeX

;;; Code:

(defvar latex-documentclasses
  '("article"
    "reoport"
    "book"
    "proc"
    "minimal"
    "slides"
    "memoir"
    "letter"
    "beamer")
  "List of the names for built-in LaTeX documentclasses.")

(leaf latex
  :ensure auctex
  :mode ("\\.tex$" . latex-mode)
  :defun (((er/mark-LaTeX-math er/mark-LaTeX-inside-environment)
           . expand-region)
          (latex-complete-envnames . tex-mode)
          ((LaTeX-current-environment
            LaTeX-insert-item
            LaTeX-env-item
            LaTeX-find-matching-begin
            LaTeX-find-matching-end
            LaTeX-mark-section)
           . latex))
  :custom (TeX-master . nil)
  :hook ((LaTeX-mode-hook . my-latex-find-master-file)
         (LaTeX-mode-hook . my-latex-expansion-mode)
         (LaTeX-mode-hook . visual-fill)
         (LaTeX-mode-hook . turn-off-flycheck)
         (LaTeX-mode-hook . my-latex-disable-auto-fill))
  :major-mode-map (latex (latex-mode))
  :bind (:my-latex-local-map
         ("="  . my-calc-simplify-region-copy)
         ("f"  . my-calc-simplify-region-change)
         ("1"  . latex-split-block)
         ("6"  . my-latex-mark-inside-environment-or-math)
         ("\\" . my-latex-equation-to-split)
         ("x"  . my-latex-kill-section))
  :config                               ;nofmt
  (require 'calc-lang)
  (require 'font-latex)

  (defcustom my-latex-master-files-alist
    '(("~/zms/*/solutions/*.tex" . "../Solution.tex"))
    "Associated list, keys are wildcards, values are him master files."
    :type '(alist :key-type string :value-type string)
    :group 'my)

  (defun my-latex-find-master-file ()
    "Find auctex master file for the current buffer."
    (interactive)
    (setq-local TeX-master
                (and
                 (buffer-file-name)
                 (my-latex-lookup-master-file-of (buffer-file-name)))))

  (defun my-latex-lookup-master-file-of (filename)
    "Lookup a auctex master file for the file with FILENAME."
    (->>
     my-latex-master-files-alist
     (--find (my-latex-master-file-key-matches-with-p it filename))
     (cdr)))

  (defun my-latex-master-file-key-matches-with-p (master-file-key filename)
    "Return t, when master file key alist MASTER-FILE-KEY match with FILENAME."
    (->
     master-file-key
     (car)
     (f-full)
     (wildcard-to-regexp)
     (string-match-p filename)))

  (defun my-calc-simplify (expr)
    "Simplify EXPR via `calc' and return this."
    (calc-latex-language t)
    (calc-alg-entry expr)
    (with-temp-buffer
      (calc-copy-to-buffer 1)
      (delete-char -1)
      (buffer-string)))

  (defun my-calc-simplify-region-copy (beg end)
    "Take from BEG to END, simplify this via `calc' and copy as kill."
    (interactive "r")
    (let ((expr (my-calc-simplify (buffer-substring beg end))))
      (kill-new expr)
      (message "coppied: %s" (current-kill 0))))

  (defun my-calc-simplify-region-change (beg end)
    "Get from BEG to END change this via `calc' and yank instead of region."
    (interactive "r")
    (let* ((expr (buffer-substring beg end))
           (simplified (my-calc-simplify expr)))
      (goto-char beg)
      (delete-region beg end)
      (insert simplified)))

  (leaf xenops
    :hook LaTeX-mode-hook
    :ensure t
    :custom (xenops-math-image-scale-factor . 2))

  (leaf math-preview
    :ensure t
    :fast-exec ("Preview All Latex Fragments" 'math-preview-all)
    :ensure-system-package              ;nofmt
    `(math-preview .
                   ,(concat "npm install"
                            "-g git+"
                            "https://gitlab.com/matsievskiysv/math-preview"))
    :bind (:my-latex-local-map
           :package tex
           ("v" . my-latex-preview-in-other-window))
    :config                             ;nofmt
    (defun my-latex-preview-in-other-window ()
      "Preview fragment of LaTeX source at point in seperated window."
      (interactive)
      (let ((source
             (save-mark-and-excursion
               (xah-select-block)
               (buffer-substring (region-beginning) (region-end)))))
        (switch-to-buffer-other-window "*my-latex-preview*")
        (delete-region (point-min) (point-max))
        (LaTeX-mode)
        (insert source)
        (math-preview-region (point-min) (point-max)))))

  (leaf my-latex-insert
    :load-path* "lisp/languages/latex/"
    :hook (LaTeX-mode-hook . my-latex-expansion-mode))

  (leaf yasnippet
    :bind (:yas-keymap
           ("<tab>" . yas-next-field-or-cdlatex)
           ("TAB"   . yas-next-field-or-cdlatex))
    :config                             ;nofmt
    (defun cdlatex-in-yas-field ()
      (when-let* ((_ (overlayp yas--active-field-overlay))
                  (end (overlay-end yas--active-field-overlay)))
        (if (>= (point) end)
            (let ((s (thing-at-point 'sexp)))
              (unless (and s
                           (assoc
                            (substring-no-properties s)
                            cdlatex-command-alist-comb))
                (yas-next-field-or-maybe-expand)
                t))
          (let (cdlatex-tab-hook minp)
            (setq minp
                  (min
                   (save-excursion (cdlatex-tab) (point))
                   (overlay-end yas--active-field-overlay)))
            (goto-char minp)
            t))))

    (defun yas-next-field-or-cdlatex nil
      (interactive)
      "Jump to the next Yas field correctly with cdlatex active."
      (if (or
           (bound-and-true-p cdlatex-mode)
           (bound-and-true-p org-cdlatex-mode))
          (cdlatex-tab)
        (yas-next-field-or-maybe-expand))))

  (defun my-latex-mark-inside-environment-or-math ()
    "If the cursor place inside of the math environment mark that."
    (interactive)
    (if (texmathp)
        (er/mark-LaTeX-math)
      (er/mark-LaTeX-inside-environment)))

  (leaf laas
    :ensure t
    :hook (LaTeX-mode-hook . laas-mode)
    :aas (laas-mode
          :cond #'texmathp
          ;; Some Physics Units
          "As" "\\mathrm{А}"
          "Vs"  "\\mathrm{В}"
          "Oms"  "\\mathrm{Ом}"
          "cls" "^\\circ C"

          ;; Some Physics Sheet
          "eqv" "\\mathrm{Экв.}"

          ;; Some Cool Symbols
          "trg" "\\triangle"
          "agl" "\\angle"
          "grd" "^\\circ"))

  (leaf cdlatex
    :ensure t
    :hook ((cdlatex-tab-hook . yas-expand)
           (cdlatex-tab-hook . cdlatex-in-yas-field)
           (LaTeX-mode-hook  . turn-on-cdlatex))
    :bind (:cdlatex-mode-map
           ("<tab>" . cdlatex-tab)
           (";" . my-latex-dollar)
           ("(" .  self-insert-command)
           (")" .  self-insert-command)
           ("{" .  self-insert-command)
           ("}" .  self-insert-command)
           ("[" .  self-insert-command)
           ("]" .  self-insert-command)
           ("\"" . self-insert-command)
           ("\\" . self-insert-command))
    ;; fields
    :custom (cdlatex-math-modify-alist
             .
             '((?q "\\sqrt" nil t nil nil)
               (?u "\\breve" "\\uline" t nil nil)
               (?v "\\vec" nil t nil nil)))
    :config                             ;nofmt
    (defun my-latex-dollar ()
      "Insert dollars and turn input method into English."
      (interactive)
      ;; when current-input-method isn standard
      (if (not current-input-method)
          ;; then
          (insert ";")
        ;; else
        (toggle-input-method)
        (if (use-region-p)
            (sp-wrap-with-pair "$")
          (sp-insert-pair "$")))))

  (leaf embrace
    :after (embrace cdlatex)
    :hook ((LaTeX-mode-hook . embrace-LaTeX-mode-hook)
           (LaTeX-mode-hook . my-embrace-LaTeX-mode-hook))
    :config                             ;nofmt
    (defun my-embrace-LaTeX-mode-hook ()
      "My additional `embrace-LaTeX-mode-hook'."
      (interactive)
      (setq-local embrace-show-help-p nil)
      (--each
          (-concat cdlatex-math-modify-alist-default
                   cdlatex-math-modify-alist)
        (my-embrace-add-paren-of-cdlatex-math it))
      (my-embrace-add-paren-latex-command ?a "answer")
      (embrace-add-pair-regexp ?\\
                               (rx "\\"
                                   (1+ wordchar)
                                   (* space)
                                   (? "[" (*? any) "]" (* space))
                                   "{")
                               "}"
                               'my-embrace-with-latex-command
                               (embrace-build-help "\\name{" "}"))
      (embrace-add-pair-regexp ?d
                               "\\\\left."
                               "\\\\right."
                               'my-embrace-with-latex-left-right
                               (embrace-build-help
                                "\\left(" "\\right)"))
      (embrace-add-pair-regexp
       ?e
       "\\\\begin{\\(.*?\\)}\\(\\[.*?\\]\\)*"
       "\\\\end{\\(.*?\\)}"
       'my-embrace-with-latex-env
       (embrace-build-help "\\begin{name}" "\\end{name}")
       t))

    (defun my-embrace-add-paren-of-cdlatex-math (element)
      "Add an ELEMENT of the `cdlatex-math-modify-alist' to the `embrace' parens."
      (let* ((key (-first-item element))
             (cmd
              (s-chop-prefix
               "\\"
               (or (-third-item element) (-second-item element))))
             (type (-fourth-item element)))
        (if type
            (my-embrace-add-paren-latex-command key cmd)
          (my-embrace-add-paren-latex-style-command key cmd))))

    (defun my-embrace-add-paren-latex-command (key name)
      "Add paren at KEY for the LaTeX command with NAME in `embrace'."
      (embrace-add-pair-regexp
       key
       (my-latex-command-left-paren-regexp name)
       "}"
       (-const (cons (my-latex-command-left-paren name) "}"))
       (embrace-build-help (my-latex-command-left-paren name) "}")))

    (defun my-latex-command-left-paren (name)
      "Return paren right of the LaTeX command named NAME."
      (s-concat "\\" name "{"))

    (defun my-latex-command-left-paren-regexp (name)
      (rx "\\"
          (literal name)
          (* space)
          (? "[" (*? any) "]" (* space))
          "{"))

    (defun my-embrace-add-paren-latex-style-command (key name)
      "Add paren at KEY for the style LaTeX command with NAME in `embrace'."
      (embrace-add-pair-regexp key
                               (my-latex-style-command-left-paren-regexp name)
                               "}"
                               (-const
                                (cons
                                 (my-latex-style-command-left-paren name)
                                 "}"))
                               (embrace-build-help
                                (my-latex-style-command-left-paren name)
                                "}")))

    (defun my-latex-style-command-left-paren (name)
      "Return paren right of the LaTeX command named NAME."
      (s-concat "{\\" name " "))

    (defun my-latex-style-command-left-paren-regexp (name)
      (rx "{" (* space) "\\" (literal name) (* space)))

    (defun my-embrace-with-latex-command ()
      "Return pair from the left and right pair for a LaTeX command."
      (let ((name (read-string "Name of a LaTeX command, please: ")))
        (cons (s-concat "\\" name "{") "}")))

    (defun my-embrace-with-latex-left-right ()
      "Return pair from the left and right pair for the LaTeX command \\left."
      (cons
       (s-concat "\\left" (read-char "Left paren, please: "))
       (s-concat "\\right" (read-char "Right paren, please: "))))

    (defun my-embrace-with-latex-env ()
      "Return pair from the left and right pair for the LaTeX command \\left."
      (let ((env
             (read-string "Name of the environment, please: "
                          (latex-complete-envnames))))
        (cons
         (s-concat "\\begin{" env "}")
         (s-concat "\\end{" env "}")))))

  (leaf smartparens-latex
    :after smartparens
    :bind (:latex-mode-map :package tex-mode ("$" . self-insert-command))
    :config                             ;nofmt
    (sp-with-modes
        '(tex-mode plain-tex-mode latex-mode LaTeX-mode)
      (sp-local-pair " ``"
                     "''"
                     :trigger "\""
                     :unless '(sp-latex-point-after-backslash sp-in-math-p)
                     :post-handlers '(sp-latex-skip-double-quote))
      (sp-local-pair " \"<"
                     "\">"
                     :trigger "<"
                     :unless '(sp-latex-point-after-backslash sp-in-math-p)))

    (leaf my-latex-autoformat
      :config                             ;nofmt
      (my-autoformat-bind-for-major-mode
       'latex-mode
       'autoformat-latex-capitalize-special-commands
       'autoformat-latex-expand-to-list-item
       'autoformat-sentence-capitalization)

      (defvar autoformat-latex-capitalize-latex-commands
        '("author"
          "title"
          "date"
          "part"
          "subsection"
          "subsubsection"
          "section"
          "part"
          "chapter")
        "List of regexps which Emacs will automatically capitalize.")

      (defvar autoformat-latex-capitalize-regexps
        (--map
         (s-concat "\\\\" it "\\W*{.")
         autoformat-latex-capitalize-latex-commands)
        "List of regexps which Emacs will automatically capitalize.")

      (add-to-list 'autoformat-latex-capitalize-regexps "\\\\item\\W+.")

      (defun autoformat-latex-capitalize-special-commands ()
        "Capitalize last symbol, when its match on special regexp."
        (interactive)
        (when (-any #'looking-back autoformat-latex-capitalize-regexps)
          (undo-boundary)
          (capitalize-word -1)))

      (defun autoformat-latex-expand-to-list-item ()
        "Try expand fragments sush as 1. or - to LaTeX list items."
        (cond
         ((autoformat-latex-expand-to-enumerate-list-item-p)
          (autoformat-latex-expand-to-enumerate-list-item))
         ((autoformat-latex-expand-to-itemized-list-item-p)
          (autoformat-latex-expand-to-itemized-list-item))))

      (defcustom autoformat-latex-enumerate-list-items-triggers
        '("[0-9]*\\. ")
        "List of regepxs which should be expanded to LaTeX enumerate list item.

Will be expanded only on matching in empty line and not in math"
        :type '(repeat string)
        :group 'my)

      (defcustom autoformat-latex-itemized-list-items-triggers
        '("- " "\\* ")
        "List of regepxs which should be expanded to LaTeX itemized list item.

Will be expanded only on matching in empty line and not in math"
        :type '(repeat string)
        :group 'my)

      (defun autoformat-latex-expand-to-enumerate-list-item-p ()
        "Get t, when autoformat should expand text to the enumerate LaTeX list."
        (my-one-of-regexps-looking-back-on-bol
         autoformat-latex-enumerate-list-items-triggers))

      (defun autoformat-latex-expand-to-itemized-list-item-p ()
        "Get t, when autoformat should expand text to the itemized LaTeX list."
        (my-one-of-regexps-looking-back-on-bol
         autoformat-latex-itemized-list-items-triggers))

      (defun my-one-of-regexps-looking-back-on-bol (regexps)
        "Get t, when one of REGEXPS matchs with text from current point to bol."
        (->> regexps (--map (concat "^ *" it)) (-some 'looking-back)))

      (defun autoformat-latex-expand-to-enumerate-list-item ()
        "Expand, for example, 1. to the LaTeX enumerate list item."
        (clear-current-line)
        (if (string-equal (LaTeX-current-environment) "enumerate")
            (LaTeX-insert-item)
          (LaTeX-env-item "enumerate")))

      (defun autoformat-latex-expand-to-itemized-list-item ()
        "Expand, for example, 1. to the LaTeX itemized list item."
        (clear-current-line)
        (if (string-equal (LaTeX-current-environment) "itemize")
            (LaTeX-insert-item)
          (LaTeX-env-item "itemize")))

      (defun my-latex-env-beg-and-end ()
        "Return as cons beginning and end of current LaTeX environment."
        (save-excursion
          (LaTeX-find-matching-begin)
          (end-of-line)
          (forward-char)
          (push-mark nil nil t)
          (LaTeX-find-matching-end)
          (beginning-of-line)
          (forward-char -1)
          (cons (region-beginning) (region-end)))))

    (defun my-latex-env-beg ()
      "Return point at beginning of current LaTeX environment."
      (car (my-latex-env-beg-and-end)))

    (defun my-latex-env-end ()
      "Return point at end of current LaTeX environment."
      (cdr (my-latex-env-beg-and-end)))

    (defun my-latex-env-beg ()
      "Return point at beginning of current LaTeX environment."
      (car (my-latex-env-beg-and-end)))

    (defun my-latex-wrap-environment (beg end environment)
      "Wrap the region from BEG to END into ENVIRONMENT.

If the environment is not given, ask for it using completion."
      (just-mark-region beg end)
      (cdlatex-wrap-environment environment)
      (indent-region (region-beginning) (region-end)))

    (defun my-latex-kill-section ()
      "Kill a LaTeX section."
      (interactive)
      (LaTeX-mark-section)
      (kill-region (region-beginning) (region-end))))

  (leaf latex-extra
    :ensure t
    :hook ((LaTeX-mode-hook . latex-extra-mode)
           (LaTeX-mode-hook . visual-line-mode))
    :bind (:my-latex-local-map
           :package tex
           ("e" . latex/compile-commands-until-done)
           ("l" . latex/next-section-same-level)
           ("j" . latex/previous-section-same-level)))

  (leaf company-math
    :ensure t
    :hook (LaTeX-mode-hook . my-company-math-setup)
    :config                             ;nofmt
    (defun my-company-math-setup ()
      "Setup for `company-math'."
      (add-to-list 'company-backends 'company-math-symbols-latex)
      (add-to-list 'company-backends 'company-latex-commands)))

  (leaf company-auctex
    :ensure t
    :after auctex
    :config (company-auctex-init))

  (leaf my-latex-math-spaces :hook latex-mode)

  (defun my-latex-disable-auto-fill ()
    "Disable `auto-fill-mode'."
    (interactive)
    (auto-fill-mode 0)))

(provide 'my-latex)
;;; my-latex.el ends here
