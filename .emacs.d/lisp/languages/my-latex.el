;;; my-latex.el --- my-latex

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
(use-package tex
  :ensure auctex
  :hook ((latex-mode . my-latex-find-master-file)
	 (latex-mode . LaTeX-mode))
  :config (my-define-local-major-mode-map 'latex '(LaTeX-mode latex-mode)))

(defun my-latex-find-master-file ()
  "Find auctex master file for the current buffer."
  (interactive)
  (setq-local TeX-master
              (or
               (my-latex-lookup-master-file-of (buffer-file-name))
               t)))

(defcustom my-latex-master-files-alist
  '(("~/zms/*/solutions/*.tex" . "../Solution.tex"))
  "Associated list, keys are wildcards, values are him master files."
  :type '(alist :key-type string :value-type string))

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

(use-package latex
  :ensure auctex
  :bind ((:map my-latex-local-map)
	 ("="     . my-calc-simplify-region-copy)
	 ("f"     . my-calc-simplify-region-change))

  :config (require 'calc-lang)
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
      (insert simplified))))

(use-package xenops
  :ensure t
  ;; :hook
  ;; (LaTeX-mode . xenops-mode)
  :custom (xenops-math-image-scale-factor 2))

(use-package math-preview
  :ensure t
  :custom (math-preview-preprocess-functions
	   (list (lambda (s) (s-concat "{\\color{white}" s "}"))))
  :config (defun my-latex-preview-in-other-window
	      ()
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
	      (math-preview-region (point-min) (point-max))))
  (defun fast-exec-math-preview-keys ()
    "Get some useful keymaps of  `fast-exec' for math-preview."
    (fast-exec/some-commands
     ("Preview All Latex Fragments" 'math-preview-all)))

  (fast-exec/register-keymap-func 'fast-exec-math-preview-keys)
  (fast-exec/reload-functions-chain)
  :bind ((:map my-latex-local-map)
	 ("v" . 'my-latex-preview-in-other-window)))

(use-package magic-latex-buffer
  :ensure t
  :hook (LaTeX-mode . magic-latex-buffer))

(defvar my-latex-insert-at-start-arg-type 'optional
  "Type of argument (optional or required) which will be inserted at start.")

(defvar my-latex-insert-commands nil
  "Alist from the keys for LaTeX insertion, values - their insert functions.")

(defun my-latex-insert-command (name &rest args)
  "Insert a LaTeX command named NAME with required arguments ARGS.

Each arg of ARGS will be inserted as string, which will be taked with function
`format' and the argument string is \"%s\"

If one of ARGS is nil, then don't insert it.  If one of ARGS is `alist', then
insert it by the following form (see `my-latex-format-for-arg' for understand of
formating):

key1=val1,key2=val2

If you need to use optional arguments, add to ARGS the keyword `:optional' and
each element after it will be inserted as optional argument."
  (insert "\\" name)
  (my-latex--insert-args args)
  (indent-region (point-at-bol) (point-at-eol)))

(defun my-latex--insert-args (args)
  "Insert ARGS as LaTeX arguments.

See `my-latex-insert-command' for understand of ARGS"
  (-let
      (((required optional)
        (my-latex-optional-and-required-args args)))
    (case my-latex-insert-at-start-arg-type
      ((optional)
       (my-latex--insert-optional-args optional)
       (my-latex--insert-required-args required))
      ((required)
       (my-latex--insert-required-args required)
       (my-latex--insert-optional-args optional)))))

(defun my-latex--insert-optional-args (optional)
  "Insert list OPTIONAL as optional LaTeX arguments."
  (-each optional 'my-latex-insert-optional-arg))

(defun my-latex--insert-required-args (required)
  "Insert list REQUIRED as required LaTeX arguments."
  (-each required 'my-latex-insert-required-arg))

(defun my-latex-optional-and-required-args (args)
  "Split ARGS to 2 lists: required and optional arguments.

Optional arguments are elements of ARGS after the `:optional' keyword, required
are rest"
  (let* ((break-index (-elem-index :optional args)))
    (case break-index
      ((nil)
       (list args nil))
      ((0)
       (list nil (cdr args)))           ; here cdr ignore
      ;; the first element of args: :optional
      (t
       (list
        (-slice args 0 break-index)
        (-slice args (1+ break-index)))))))

(defun my-latex-insert-required-arg (val)
  "Insert VAL as an LaTeX required argument."
  (--when-let (my-latex-format-for-arg val) (insert "{" it "}")))

(defun my-latex-insert-optional-arg (val)
  "Insert VAL as an LaTeX optional argument."
  (--when-let (my-latex-format-for-arg val) (insert "[" it "]")))

(defun my-latex-format-for-arg (val)
  "Format VAL as string for inserting of an LaTeX argument."
  (cond
   ((my-alist-p val)
    (-some->>
        val
      (-filter 'cdr)
      (--map (format "%s=%s" (car it) (cdr it)))
      (s-join ",")))
   (val (format "%s" val))))

(defun my-alist-p (obj)
  "Return t, when OBJ is `alist'."
  (and
   (listp obj)
   (not (null obj))
   (consp (car obj))))

(defun my-latex-insert-single-line-command (command &rest args)
  "Insert the LaTeX command named COMMAND with ARGS.

This version of `my-latex-insert-command', so see `my-latex-insert-command'"
  (just-ensure-empty-line)
  (apply 'my-latex-insert-command command args))

(defun my-latex-insert-env (name &rest args)
  "Insert the LaTeX environment named NAME with ARGS.

See `my-latex-insert-command' for understand of use ARGS"
  (let ((beg (point))
	end)
    (my-latex-insert-single-line-command "begin" name)
    (my-latex--insert-args args)
    (newline)
    (insert "  ")
    (save-excursion
      (newline)
      (my-latex-insert-command "end" name)
      (setq end (point)))
    (run-hook-with-args LaTeX-after-insert-env-hook name beg end)))

(defun my-latex-in-env-p (expected)
  "Get non-nil, when the cursor placed in the LaTeX environment named EXPECTED."
  (let ((arg 1)
	(found nil)
	(actual (LaTeX-current-environment)))
    (while (and (not (string-equal actual "document")) (not found))
      (setq actual (LaTeX-current-environment arg))
      (setq found (string-equal expected actual))
      (incf arg))
    found))

(defun my-latex-expand-define-function (key fun)
  "Bind call of FUN at KEY in the LaTeX.

FUN will be called when the user press KEY and dot"
  (add-to-list 'my-latex-insert-commands (cons key fun))
  (aas-set-snippets 'latex-mode (s-concat key ".") fun))

(defmacro my-latex-expand-define (key name args &rest body)
  "Bind evaluation of BODY at KEY in LaTeX, define function with NAME and ARGS.

BODY will be evaluated when the user press KEY and dot"
  (declare (indent 3))
  `(progn
     (defun ,name ,args ,@body)
     (my-latex-expand-define-function ,key ',name)))

(defun my-latex-insert-commands-help ()
  "View help info about each of the LaTeX insert commands."
  (interactive)
  (let ((buffer (get-buffer-create "*Help for LaTeX insert*"))
	(longest-key-length
         (-max (--map (length (car it)) my-latex-insert-commands))))
    (switch-to-buffer-other-window buffer)
    (erase-buffer)
    (--each my-latex-insert-commands
      (my-latex-insert-command-insert-key it longest-key-length)
      (insert "   ")
      (my-latex-insert-command-insert-name it)
      (newline))))

(defun my-latex-insert-command-insert-key (command &optional min-len)
  "Insert the key of a LaTeX insert command.

COMMAND is pair from the key which will be activate that command, and function
which will be do insertion.

If MIN-LEN is lower then length of the key, then insert extra spaces to the
start of the key."
  (let ((key (car command)))
    (and min-len
         (> min-len (length key))
         (self-insert-command (- min-len (length key)) ? ))
    (insert key)))

(defun my-latex-insert-command-insert-name (command)
  "Insert the name of a LaTeX insert command.

COMMAND is pair from the key which will be activate that command, and function
which will be do insertion."
  (insert (symbol-name (cdr command))))

(define-minor-mode my-latex-expansion-mode
  "Which expands certain text fragments to LaTeX objects."
  :init-value nil
  (if my-latex-expansion-mode
      (progn (aas-mode +1) (aas-activate-keymap 'latex-mode))
    (aas-deactivate-keymap 'latex-mode)))

(use-package tex
  :ensure auctex
  :hook (LaTeX-mode . my-latex-expansion-mode))

(defun my-latex-ensure-has-graphicspath ()
  "Ensure that current LaTeX buffer has \\graphicspath{}.

If now current LaTeX buffer hasn't \\graphicspath{}, read new graphicspath from
the minibuffer."
  (unless (my-latex-current-graphicspathes)
    (my-latex-add-graphicspath
     (read-directory-name "Dirictory of the saved images, please: "))))

(defun my-latex-graphics-init ()
  "Add some stuffs for graphics in LaTeX."
  (interactive)
  (my-latex-use-package "graphicx")
  (my-latex-ensure-has-graphicspath)
  (--each (my-latex-current-graphicspathes) (f-mkdir (f-full it))))

(defun my-latex-add-graphicspath (path)
  "Add to list of current LaTeX file' s graphicpath PATH."
  (let ((path
         (s-chop-prefix (f-full default-directory) (f-full path))))
    (->>
     (my-latex-current-graphicspathes)
     (cons path)
     (my-latex-set-graphicspath))))

(defun my-latex-set-graphicspath (paths)
  "Set graphicpath (command \\graphicspath)of current LaTeX buffer to PATHS."
  (save-excursion
    (my-latex-goto-graphicspath)
    (forward-char -1)
    (sp-get
     (sp-get-enclosing-sexp)
     (delete-region :beg-in :end-in)
     (--each paths (insert "{" it "}")))))

(defun my-latex-current-graphicspathes ()
  "Parse from current LaTeX buffer value of \\graphicspath."
  (save-excursion
    (my-latex-goto-graphicspath)
    (forward-char -1)
    (sp-get
     (sp-get-enclosing-sexp)
     (->>
      (buffer-substring-no-properties :beg-in :end-in)
      (s-match-strings-all "{\\(.*\\)}")
      (-map '-last-item)))))

(defun my-latex-goto-graphicspath ()
  "Goto end of LaTeX command for set graphic paths, if isn't exit insert."
  (goto-char (point-min))
  (unless (search-forward-regexp "^\\\\graphicspath" nil t)
    (search-forward-regexp "\\usepackage.*{graphicspath}" nil t)
    (forward-line 1)
    (beginning-of-line)
    (my-latex-insert-command "graphicspath")
    (insert "{}")
    (newline)
    (forward-char -1))
  (end-of-line))

(defun my-latex-insert-includegraphics (filename &optional width)
  "Insert the \\includegraphics LaTeX command which include FILENAME.

If WIDTH is non-nil, then insert WIDTH as the optional argument width to the
\\includegraphics command."
  (let* ((filename filename)
	 (image-graphicspath
          (--find
           (s-prefix-p it filename)
           (my-latex-current-graphicspathes)))
	 (filename
          (if image-graphicspath
              (s-chop-prefix (f-full image-graphicspath) filename)
            filename)))
    (my-latex-insert-single-line-command "includegraphics" filename
                                         :optional `((width . ,width)))))

(defun my-latex-download-image-to-graphicspath (url &optional filename)
  "Download image at URL into graphicspath of current LaTeX buffer.
This file has name FILENAME.  Return nil when fail, otherwise return path of
downloaded file"
  (or filename (setq filename (my-uri-of-url url)))
  (let* ((graphicspath (-last-item (my-latex-current-graphicspathes)))
	 (dest (f-join graphicspath filename)))
    (url-copy-file url dest t)
    dest))

(defun my-uri-of-url (url)
  "Get the URI of URL."
  (->>
   url
   (s-split "/")
   (-last-item)
   (s-split "?")
   (-first-item)))

(defun my-latex-use-package (package &optional options)
  "Add \\usepackage for PACKAGE with OPTIONS to the current LaTeX buffer."
  (interactive
   (list
    (my-read-string-or-nil "Please, choose package which you need use: ")))
  (unless (my-latex-used-package-p package)
    (save-excursion
      (my-latex-goto-use-package-source)
      (beginning-of-line)
      (my-latex-insert-command "usepackage" package :optional options)
      (newline)
      (run-hooks LaTeX-after-usepackage-hook))))

(defun my-latex-goto-use-package-source ()
  "Go to the place of the LaTeX source code where shoud be inserted usepackage."
  (interactive)
  (goto-char (point-min))
  (unless (search-forward-regexp "^\\\\usepackage" nil t)
    (search-forward-regexp "^\\\\begin{document}" nil t))
  (beginning-of-line))

(defun my-latex-used-package-p (package)
  "Return t, when PACKAGE was used in current LaTeX buffer."
  (-contains-p (my-latex-used-packages) package))

(defun my-latex-used-packages ()
  "Parse from current LaTeX buffer, list of used packages."
  (->>
   (buffer-string)
   (s-lines)
   (--filter (s-prefix-p "\\usepackage" it))
   (--map
    (-last-item
     (s-match "\\\\usepackage\\(\\[.*\\]\\)?{\\(.*\\)}" it)))))

(my-latex-expand-define "wt" my-latex-insert-table ;nofmt
			(preamble wrapfig-pos width &optional placement centering-p)
  "Insert the LaTeX environment table to the current buffer with PREAMBLE.

If CENTERING-P is non-nil make table centered.  Pass to the environment
PLACEMENT"
  (interactive
   (list
    (my-latex-read-preamble)
    (my-latex-read-wrapfig-pos)
    (my-latex-read-width)
    (my-latex-read-placement)
    (my-latex-read-centering)))
  (my-latex-insert-env "wraptable" wrapfig-pos width)
  (my-latex--insert-inner-of-table preamble centering-p))

(my-latex-expand-define "t" my-latex-insert-table ;nofmt
			(preamble &optional placement centering-p)
  "Insert the LaTeX environment table to the current buffer with PREAMBLE.

If CENTERING-P is non-nil make table centered.  Pass to the environment
PLACEMENT"
  (interactive
   (list
    (my-latex-read-preamble)
    (my-latex-read-placement)
    (my-latex-read-centering)))
  (my-latex-insert-env "table" :optional placement)
  (my-latex--insert-inner-of-table preamble centering-p))

(defun my-latex--insert-inner-of-table (preamble centering-p)
  "Insert the inner of the LaTeX environment tables sush as table.

If CENTERING-P is non-nil make table centered.  Pass to the environment
PREAMBLE"
  (when centering-p (my-latex-insert-centering))
  (my-latex-insert-tabular preamble))

(defun my-latex-read-preamble ()
  "Read a LaTeX preamble for table from the minibuffer."
  (my-read-string-or-nil "Preamble, please: "))

(my-latex-expand-define "f" my-latex-insert-figure
			(&optional placement)
  "Insert the LaTeX environment figure into current buffer with PLACEMENT."
  (interactive (list (my-latex-read-placement)))
  (my-latex-insert-env "figure" :optional placement))

(my-latex-expand-define "cp" my-latex-insert-caption ;nofmt
			(&optional caption-string)
  "If CAPTION-STRING is non-nil, then insert a caption for with LaTeX syntax."
  (interactive (list (my-latex-read-caption)))
  (when caption-string
    (my-latex-insert-single-line-command "caption" caption-string)))

(my-latex-expand-define "cr" my-latex-insert-center
			()
  "Insert the LaTeX environment center."
  (interactive)
  (my-latex-insert-env "center"))

(my-latex-expand-define "tr" my-latex-insert-tabular
			(preamble)
  "Insert the latex environment tabular."
  (interactive (list (my-latex-read-preamble)))
  (my-latex-insert-env "tabular" preamble))

(my-latex-expand-define "ar" my-latex-insert-arabic
			(counter)
  "Insert the latex command arabic."
  (interactive (list (my-latex-read-counter-name)))
  (my-latex-insert-command "arabic" counter))

(my-latex-expand-define "rom" my-latex-insert-roman
			(counter)
  "Insert the latex command roman."
  (interactive (list (my-latex-read-counter-name)))
  (my-latex-insert-command "roman" counter))

(my-latex-expand-define "al" my-latex-insert-alph
			(counter)
  "Insert the latex command alph."
  (interactive (list (my-latex-read-counter-name)))
  (my-latex-insert-command "alph" counter))

(defcustom my-latex-default-counters
  '("part"
    "chapter"
    "section"
    "subsection"
    "subsubsection"
    "paragraph"
    "subparagraph"
    "page"
    "equation"
    "figure"
    "table"
    "footnote"
    "mpfootnote"
    "enumi"
    "enumii"
    "enumiii"
    "enumiv"
    "task")
  "List of the built-in LaTeX counters names."
  :type '(repeat string))

(defun my-latex-read-counter-name ()
  "Read name of a LaTeX counter from the minibuffer."
  (completing-read "Name of counter, please: "
                   (my-latex-get-counters-names)))

(defun my-latex-get-counters-names ()
  "Get list of all LaTeX counters names."
  (-concat
   my-latex-default-counters
   (my-latex-find-counters-names-in-buffer)))

(defun my-latex-find-counters-names-in-buffer ()
  "Find in the current buffer all LaTeX counters defnitions, return its names."
  (->>
   (buffer-string)
   (s-lines)
   (--keep
    (-second-item
     (s-match "\\newcounter\\(?:\[.*?\]\\)?{\\(.*\\)}" name)))))

(my-latex-expand-define "ncr" my-latex-insert-newcounter
			(counter &optional sub-counter)
  "Insert the LaTeX command newcounter with name COUNTER.

SUB-COUNTER is optional argument of that LaTeX command."
  (interactive
   (list
    (read-string "Name of the counter, please: ")
    (my-read-string-or-nil "Depends on counter: ")))
  (let ((my-latex-insert-at-start-arg-type 'required))
    (my-latex-insert-single-line-command "newcounter" counter
                                         :optional sub-counter)))

(my-latex-expand-define "atc" my-latex-insert-addtocounter
			(counter amount)
  "Insert the LaTeX command addtocounter with name COUNTER and AMOUNT."
  (interactive
   (list
    (my-latex-read-counter-name)
    (read-number "Amount for add to counter: ")))
  (my-latex-insert-command "addtocounter" counter amount))

(my-latex-expand-define "sc" my-latex-insert-setcounter
			(counter value)
  "Insert the LaTeX command setcounter with name COUNTER and VALUE."
  (interactive
   (list
    (my-latex-read-counter-name)
    (read-number "Value to set counter: ")))
  (my-latex-insert-command "setcounter" counter value))

(my-latex-expand-define "stc" my-latex-insert-stepcounter
			(counter)
  "Insert the LaTeX command stepcounter with name COUNTER and VALUE."
  (interactive (list (my-latex-read-counter-name)))
  (my-latex-insert-command "stepcounter" counter))

(my-latex-expand-define "au" my-latex-insert-author
			(name)
  "Insert the latex command author with NAME."
  (interactive
   (list (read-string "Name of the author, please: " user-full-name)))
  (my-latex-insert-single-line-command "author" name))

(my-latex-expand-define "ttl" my-latex-insert-title
			(title)
  "Insert the LaTeX command title with TITLE."
  (interactive "sTitle of the document: ")
  (my-latex-insert-single-line-command "title" title))

(my-latex-expand-define "dt" my-latex-insert-date
			(date)
  "Insert the LaTeX command date with DATE."
  (interactive "sDate of the document, please: ")
  (my-latex-insert-single-line-command "date" date))

(my-latex-expand-define "s" my-latex-insert-section
			(name &optional toc-name)
  "Insert the LaTeX command section with NAME and name for the TOC NAME-TOC."
  (interactive
   (list
    (read-string "Name of the new section: ")
    (my-read-string-or-nil "Name for the TOC of the section: ")))
  (my-latex-insert-command "section" name :optional toc-name))

(my-latex-expand-define "mc" my-latex-insert-multicolumn
			(cols text &optional preamble)
  "Insert the LaTeX environment multicolumn, if in table, otherwise command."
  (interactive
   (list
    (read-number "Number of the columns" 2)
    ""
    (and (my-latex-in-env-p "tabular") (my-latex-read-preamble))))
  (or
   (my-latex-maybe-insert-table-multicolumn cols preamble text)
   (my-latex-insert-text-multicolumn cols text)))

(defun my-latex-insert-text-multicolumn (cols text)
  "Insert the LaTeX environemnt multicolumn, pass to the command COLS and TEXT."
  (interactive)
  (my-latex-use-package "multicol")
  (my-latex-insert-env "multicols" cols text))

(defun my-latex-maybe-insert-table-multicolumn (cols preamble text)
  "Call the `my-latex-insert-table-multicolumn', when in the LaTeX table env.

Pass COLS, PREAMBLE and TEXT to the function."
  (when (my-latex-in-env-p "tabular")
    (my-latex-insert-table-multicolumn cols preamble text)))

(defun my-latex-insert-table-multicolumn (cols preamble text)
  "Insert the LaTeX command multicolumn.

Pass to the command COLS, PREAMBLE and TEXT."
  (my-latex-insert-command "multicols" cols preamble text))

(use-package cdlatex
  :ensure t
  :hook (LaTeX-mode . turn-on-cdlatex)
  :defer t
  :bind ((:map cdlatex-mode-map)
	 ("<tab>" . cdlatex-tab)))

;; fields
(use-package cdlatex
  :hook ((cdlatex-tab . yas-expand)
	 (cdlatex-tab . cdlatex-in-yas-field))
  :custom (cdlatex-math-modify-alist
	   '((?q "\\sqrt" nil t nil nil)
	     (?u "\\breve" "\\uline" t nil nil))))

(use-package yasnippet
  :bind ((:map yas-keymap)
	 ("<tab>" . yas-next-field-or-cdlatex)
	 ("TAB" . yas-next-field-or-cdlatex))
  :config ;nofmt
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

(use-package latex
  :bind ((:map my-latex-local-map)
	 ("1" . 'latex-split-block)
	 ("6" . 'my-latex-mark-inside-environment-or-math)))

(defun my-latex-mark-inside-environment-or-math ()
  "If the cursor place inside of the math environment mark that."
  (interactive)
  (if (texmathp)
      (er/mark-LaTeX-math)
    (er/mark-LaTeX-inside-environment)))

(use-package laas
  :ensure t
  :hook (LaTeX-mode . laas-mode)
  :config (aas-set-snippets
	   'laas-mode
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

(use-package embrace
  :ensure t
  :hook ((LaTeX-mode . embrace-LaTeX-mode-hook)
	 (LaTeX-mode . my-embrace-LaTeX-mode-hook)))

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
                           (embrace-build-help "\\left(" "\\right)"))
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
    (cons (s-concat "\\begin{" env "}") (s-concat "\\end{" env "}"))))

(use-package tex-mode
  :after cdlatex
  :bind ((:map cdlatex-mode-map)
	 ("(" .  self-insert-command)
	 (")" .  self-insert-command)
	 ("{" .  self-insert-command)
	 ("}" .  self-insert-command)
	 ("[" .  self-insert-command)
	 ("]" .  self-insert-command)
	 ("\"" . self-insert-command)
	 ("\\" . self-insert-command)))

(defun my-latex-dollar ()
  "Insert dollars and turn input method into English."
  (interactive)
  ;; when current-input-method isn standard
  (if (not current-input-method)
      ;; then
      (insert ";")
    ;; else
    (toggle-input-method)
    (if (use-region-p) (sp-wrap-with-pair "$") (sp-insert-pair "$"))))

(use-package cdlatex
  :ensure t
  :bind (:map cdlatex-mode-map)
  (";" . my-latex-dollar))

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

(use-package org-table)

(my-latex-expand-define "ot" my-latex-insert-orgtbl-table
			(name)
  "Insert table with `orgtbl-mode'.

It's insert two comments line and the LaTeX environment comment in which
you will edit your table in the `org-mode' syntax.  After you should press
C-c C-c and the source code will be replaced.

Each table should has certain NAME, so inserted table will has name NAME"
  (interactive "sName of the table, please: ")
  (let* ((e
          (cl-assoc-if #'derived-mode-p orgtbl-radio-table-templates))
	 (txt (nth 1 e))
	 name pos)
    (unless e
      (user-error "No radio table setup defined for %s" major-mode))
    (while (string-match "%n" txt)
      (setq txt (replace-match name t t txt)))
    (or (bolp) (insert "\n"))
    (setq pos (point))
    (insert txt)
    (goto-char pos)))

(eval
 `(my-use-autoformat-in-mode
   'LaTeX-mode
   ,@(append
      '(latex-capitalize-special-commands latex-expand-to-list-item)
      my-autoformat-all-functions)))

(defvar autoformat-latex-capitalize-latex-commands
  '("author" "title" "date" "part" "subsection" "section" "part" "chapter")
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
  :type '(repeat string))

(defcustom autoformat-latex-itemized-list-items-triggers
  '("- " "\\* ")
  "List of regepxs which should be expanded to LaTeX itemized list item.

Will be expanded only on matching in empty line and not in math"
  :type '(repeat string))

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

(defun my-latex-equation-to-split ()
  "Transform LaTeX equation environment to split environment."
  (interactive)
  (save-excursion
    (my-latex-wrap-environment
     (my-latex-env-beg)
     (my-latex-env-end)
     "split")
    (replace-string "=" "&=" nil
                    (my-latex-env-beg)
                    (my-latex-env-end))
    (just-for-each-line*
        (my-latex-env-beg)
        (my-latex-env-end)
      (when (just-call-on-next-line* (just-line-prefix-p "&=" nil t))
        (end-of-line)
        (insert "\\\\")))))

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
    (cons (region-beginning) (region-end))))

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

(bind-keys*
 :map my-latex-local-map
 ("\\" . my-latex-equation-to-split))

(defun my-latex-insert-image (filename  ;nofmt
			      &optional caption placement width centering)
  "Insert image with FILENAME with LaTeX syntax into current buffer.

By default, caption won't be inserted, but if CAPTION is a string image will
has caption CAPTION.

Image will be inserted with LaTeX environment figure and command
\\includegraphics, so you can change PLACEMENT and WIDTH of the inserted
image, PLACEMENT defaults to \"h\", WIDTH defaults to the original image
width.

If CENTERING is non-nil, then image will be centered via \\centering"
  (interactive (my--get-arguments-for-latex-insert-image))
  (my-latex-insert-figure placement)
  (my-latex--insert-inner-of-figure filename caption width centering))

(defun my-latex--insert-inner-of-figure (filename ;nofmt
					 &optional caption width centering)
  "Insert inner of the figure image environemnts sush as figure.

Insert \\includegraphics for image at FILENAME with WIDTH (if it's non-nil).

If CENTERING is non-nil make image centered.  By default, doesn't insert
CAPTION, but if CAPTION is non-nil, insert."
  (when centering ;nofmt
    (my-latex-insert-centering))
  (my-latex-insert-includegraphics filename width)
  (when caption (my-latex-insert-caption caption)))

(defun my-latex-insert-centering ()
  "Insert the LaTeX command \"centering\"."
  (interactive)
  (my-latex-insert-single-line-command "centering"))

(defun my--get-arguments-for-latex-insert-image ()
  "Read from the minibuffer arguments for the `my-latex-insert-image'."
  (my-latex-graphics-init)
  (list
   (my-latex-read-image-path-from-graphicspath)
   (my-latex-read-caption)
   (my-latex-read-width)
   (my-latex-read-placement)
   (my-latex-read-centering)))

(defun my-latex-read-image-path-from-graphicspath ()
  "Read from the minibuffer path to an image from the graphicspath."
  (let ((graphicspath
         (f-full (-last-item (my-latex-current-graphicspathes)))))
    (->>
     (read-file-name "Filename of image, please: " graphicspath)
     (s-chop-prefix graphicspath))))

(defun my-latex-read-caption ()
  "Read from the minibuffer a caption for the LaTeX.

  If the user typed nothing, then return nil"
  (read-string "Caption, please: "))

(defun my-latex-read-width (&optional for)
  "Read a LaTeX width of FOR from the minibuffer.

  If the user typed nothing, then return nil"
  (setq for (or for "the thing"))
  (my-read-string-or-nil "Width of %s, please: " for))

(defun my-latex-read-placement ()
  "Read a LaTeX placement from the minibuffer.

  If the user typed nothing, then return nil"
  (read-string "Placement, please (some chars of h t b p ! H): "))

(defun my-latex-read-centering ()
  "Return t, when the user need to centering of the LaTeX block."
  (y-or-n-p "Centering or no? "))

(defun my-latex-insert-image-at-url (url &optional filename caption placement width centering)
  "Download as FILENAME and insert image at URL to the current LaTeX buffer.

Image will be inserted with CAPTION (if CAPTION isn't nil), PLACEMENT (defaults
to \"h\"), width (defaults to origin width of the image at URL) and centering
if centering is non-nil.

If call that function interactively, then URL will be readed from either
the `kill-ring', the selected region or the minibuffer"
  (interactive (my--get-arguments-for-latex-insert-image-at-url))
  (my-latex-graphics-init)
  (my-latex-download-image-to-graphicspath url filename)
  (my-latex-insert-image filename caption placement width centering))

(defun my--get-arguments-for-latex-insert-image-at-url ()
  "Get arguments for `my-latex-insert-image-at-url'."
  (let ((url (my-read-image-url)))
    (list
     url
     (my-latex-read-new-filename-of-image-at-url url)
     (my-latex-read-caption)
     (my-latex-read-placement)
     (my-latex-read-width)
     (my-latex-read-centering))))

(defun my-latex-insert-wrapimage        ;nofmt
    (filename pos width &optional line-height caption image-width centering)
  "Insert an wraped image at FILENAME with the LaTeX syntax.

Pass to \\wrapfig POS (one of R L I O | r l i o), WIDTH and LINE-HEIGHT.

Insert caption for wrapfigure, if CAPTION is non-nil.  If CENTERING
is non-nil, then make wrapfigure centered.

Insert \\includegraphics for FILENAME with IMAGE-WIDTH (if is non-nil)."
  (interactive
   (list
    (my-latex-read-wrapfig-pos)
    (my-latex-read-width "wrapfigure")
    nil
    (my-latex-read-caption)
    (my-latex-read-centering)
    (my-latex-read-width "image")))
  (my-latex-insert-figure pos width line-height)
  (my-latex--insert-inner-of-figure filename caption image-width centering))

(defun my-latex-insert-wrapfigure (pos width &optional line-height)
  "Insert the LaTEX wrapfigure environment.

Pass to environment POS (one of R L I O | r l i o), WIDTH and LINE-HEIGHT."
  (my-latex-insert-env "wrapfigure" pos width :optional line-height))

(defun my-latex-read-wrapfig-pos ()
  "Read a position for the wrapfig environment from the minibuffer."
  (read-string "Position of the wrap figure, please (one of RLIO): "))

(defun my-latex-read-new-filename-of-image-at-url (url)
  "Read from the minibuffer new filename of the downloaded image at URL."
  (read-string "New filename of the downloaded image: "
               (my-uri-of-url url)))

(my-latex-expand-define-function "wi" 'my-latex-insert-wrapimage)

(bind-keys
 :map my-latex-local-map
 ("i" . my-latex-insert-image)
 ("u" . my-latex-insert-image-at-url))

(use-package tex-mode
  :ensure t
  :bind ((:map my-latex-local-map)
	 ("x" . 'my-latex-kill-section)))

(defun my-latex-kill-section ()
  "Kill a LaTeX section."
  (interactive)
  (LaTeX-mark-section)
  (kill-region (region-beginning) (region-end)))

(setq latex-documentclasses
      '("article" "reoport" "book" "proc" "minimal" "slides" "memoir" "letter" "beamer"))

(dolist (mode
	 (list 'TeX-mode-hook
               'tex-mode-hook
               'latex-mode-hook
               'LaTeX-mode-hook))
  (add-hook mode (lambda () (call-interactively 'visual-fill))))

(use-package latex-extra
  :ensure t
  :hook ((LaTeX-mode . latex-extra-mode)
	 (LaTeX-mode . auto-fill-mode))
  :bind ((:map my-latex-local-map)
	 ("e" . 'latex/compile-commands-until-done)
	 ("l" . 'latex/next-section-same-level)
	 ("j" . 'latex/previous-section-same-level)))

(use-package company-math
  :ensure t
  :init (defun my-company-math-setup
	    ()
	  "Setup for `company-math'."
	  (add-to-list 'company-backends 'company-math-symbols-latex)
	  (add-to-list 'company-backends 'company-latex-commands))
  (add-hook 'LaTeX-mode 'my-company-math-setup))

(use-package company-auctex :ensure t :config (company-auctex-init))

(defcustom my-latex-math-spaces-binary-ops
  nil
  "List of the regexps which indicates an binary operator of the LaTeX math.

Binary operator is a LaTeX command which no take arguments and needs to 2
one left number/expression and one rigth number/expression.  So, \\frac isn't
binary operator, because it takes arguments"
  :type '(repeat regexp))

(defcustom my-latex-math-spaces-parens
  '("(" ")" "\\\\left." "\\\\right." "|")
  "List of the regexps which indicates an paren of the LaTeX math.

In that list openning and closing parens should be added separately."
  :type '(repeat string))

(defvar my-latex-math-spaces-do-hook nil
  "Hooks which will be run when called `my-latex-math-spaces-do'.")

(define-minor-mode my-latex-math-spaces-mode
  "Minor mode which automatically insert spaces in the LaTeX math."
  :init-value t
  (if (not my-latex-math-spaces-mode)
      (remove-hook 'post-self-insert-hook 'my-latex-math-spaces-do)
    (add-hook 'post-self-insert-hook 'my-latex-math-spaces-do)))

(defun my-latex-math-spaces-do ()
  "Do insertion of the spaces for the LaTeX math syntax, if needed."
  (interactive)
  (when (and (eq major-mode 'latex-mode) (texmathp))
    (run-hooks 'my-latex-math-spaces-do-hook)))

(defun my-latex-math-spaces-for-binary-ops ()
  "Do insertion of the spaces for the last LaTeX binary operation, if needed."
  (interactive)
  (let ((start (point)))
    (when (my-latex-math-spaces-goto-binary-op-start)
      (goto-char (+ start (just-spaces-to-1)
                    ;; traveled distance
                    ))
      (just-spaces-to-1))))

(add-hook 'my-latex-math-spaces-do-hook 'my-latex-math-spaces-for-binary-ops)

(defun my-latex-math-spaces-goto-binary-op-start ()
  "Go to backward binary operation start when its looking back."
  (-any 'my-latex-math-spaces-skip-backward my-latex-math-spaces-binary-ops))

(defun my-latex-math-spaces-for-parens ()
  "Do insertion of the spaces for the last LaTeX parens commands, if needed."
  (-when-let
      (paren (my-latex-math-spaces-goto-parens-start))
    (just-spaces-to-1)
    (my-latex-math-spaces-goto-binary-op-end)
    (when (> (length paren) 1) (just-spaces-to-1))))

(add-hook 'my-latex-math-spaces-do-hook 'my-latex-math-spaces-for-binary-ops)

(defun my-latex-math-spaces-goto-parens-start ()
  "Go to the start of the LaTeX paren command, if it placed backward of point.

Return a matched paren or nil if paren isn't found."
  (-any 'my-latex-math-spaces-skip-backward my-latex-math-spaces-parens))

(defun my-latex-math-space-for-backslash ()
  "Do insertion of the spaces for the backward from point char \ , if needed."
  (save-excursion
    (skip-chars-backward " ")
    (when (= (char-before) ?\\)
      (forward-char -1)
      (just-spaces-to-1))))

(add-hook 'my-latex-math-spaces-do-hook 'my-latex-math-space-for-backslash)

(defun my-latex-math-spaces-skip-backward (regexp &optional ignore-spaces)
  "Skip REGEXP looking back, if regexps match return point, else return nil.

If IGNORE-SPACES is non-nil, then ignore backward-spaces"
  (when (search-backward-regexp (s-concat regexp " *" "\\=") nil t)
    (goto-char (match-beginning 0))
    (match-string 0)))

(defun my-latex-math-spaces-skip-forward (regexp)
  "Skip REGEXP looking back, if regexps match return point, else return nil."
  (search-forward-regexp (s-concat "\\=" regexp) nil t))

(defun my-latex-declare-bin-op (&rest ops)
  "Define LaTeX binary OPS for auto insertion of the spaces in math."
  (--each ops (add-to-list 'my-latex-math-spaces-binary-ops it)))

(defun my-latex-declare-parens (&rest parens)
  "Define LaTeX PARENS for auto insertion of the spaces in math."
  (--each ops (add-to-list 'my-latex-math-spaces-parens it)))

(use-package tex
  :ensure auctex
  :hook (LaTeX-mode . my-latex-math-spaces-mode)
  :config (my-latex-declare-bin-op
	   "\\+"
	   "-"
	   "\\\\cdot"
	   "\\\\times"
	   "="
	   "\\\\neq"
	   "&="
	   "\\\\mapsto"
	   "\\\\pm"
	   "\\\\mp"
	   "\\\\to"
	   "\\\\ll"
	   "\\\\leq"
	   "\\\\diamond"
	   "\\\\impliedby"
	   "\\\\implies"
	   "\\\\geq"
	   "\\\\gg"
	   "\\\\in"
	   "\\\\models"
	   "\\\\mid"
	   "\\\\approx"
	   "\\\\sim"))

(provide 'my-latex)
;;; my-latex.el ends here
