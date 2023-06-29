

;;;### (autoloads nil "agnifize" "agnifize.el" (0 0 0 0))
;;; Generated autoloads from agnifize.el

(autoload 'agnifize-dwim "agnifize" "\
Agnifize that you mean.

By default agnifize the current buffer.  If a region is selected and active
then agnifize source code between its bounds.  If do in a `dired' buffer, then
try agnifize marked files." t)

(register-definition-prefixes "agnifize" '("agn"))

;;;***

;;;### (autoloads nil "my-dired-commands" "my-dired-commands.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from my-dired-commands.el

(autoload 'my-dired-mark-all-files "my-dired-commands" "\
Mark all file in `dired'." t)
(autoload 'my-dired-rename "my-dired-commands")

(autoload 'my-dired-move "my-dired-commands" "\
Move file of current directory of `dired' at the point." t)
(autoload 'my-dired-delete "my-dired-commands")

(autoload 'my-dired-goto-parent-dir "my-dired-commands" "\
Navigate to parent directory of current dired directory." t)

(autoload 'my-dired-new-file "my-dired-commands" "\
Create file with FILENAME in the directory which opened in the Dired buffer.

\(fn FILENAME)" t)

(autoload 'my-dired-delete-all-files "my-dired-commands" "\
Delete all files from the directory of the `dired' buffer." t)

(autoload 'dired-avy "my-dired-commands" "\
Version of `avy' for the `dired'." t)
(autoload 'my-dired-duplicate "my-dired-commands")

(autoload 'my-dired-jump-to-home "my-dired-commands" "\
Open a `dired' buffer of the home directory." t)

(register-definition-prefixes "my-dired-commands" '("my-"))

;;;***

;;;### (autoloads nil "my-doom-modeline-segments" "my-doom-modeline-segments.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from my-doom-modeline-segments.el

(register-definition-prefixes "my-doom-modeline-segments" '("durand-buffer-name-max" "eglot--spinner" "my-"))

;;;***

;;;### (autoloads nil "my-elisp-class-fields" "my-elisp-class-fields.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from my-elisp-class-fields.el

(autoload 'my-elisp-new-field-of-class "my-elisp-class-fields" "\
Insert new field of Lisp class.
Only when in class defnition." t)

(register-definition-prefixes "my-elisp-class-fields" '("my-"))

;;;***

;;;### (autoloads nil "my-elisp-embrace" "my-elisp-embrace.el" (0
;;;;;;  0 0 0))
;;; Generated autoloads from my-elisp-embrace.el

(autoload 'my-embrace-emacs-lisp-mode-hook "my-elisp-embrace" "\
Add some parens for the Emacs-Lisp embrace.")

(register-definition-prefixes "my-elisp-embrace" '("my-embrace-emacs-lisp-with-"))

;;;***

;;;### (autoloads nil "my-latex-drag" "my-latex-drag.el" (0 0 0 0))
;;; Generated autoloads from my-latex-drag.el

(autoload 'my-latex-try-drag-right-list-item "my-latex-drag" "\
If the dragger for LaTeX list item should be work, drag that to right." t)

(autoload 'my-latex-try-drag-left-list-item "my-latex-drag" "\
If the dragger for LaTeX list item should be work, drag that to left." t)

(register-definition-prefixes "my-latex-drag" '("my-latex-"))

;;;***

;;;### (autoloads nil "my-latex-insert" "my-latex-insert.el" (0 0
;;;;;;  0 0))
;;; Generated autoloads from my-latex-insert.el

(autoload 'my-latex-expansion-mode "my-latex-insert" "\
Minor mode which complete text snippets for LaTeX after press dot.

List of the \"snippets\" you can learn using
function `my-latex-insert-commands-help'

This is a minor mode.  If called interactively, toggle the
`My-Latex-Expansion mode' mode.  If the prefix argument is
positive, enable the mode, and if it is zero or negative, disable
the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `my-latex-expansion-mode'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\(fn &optional ARG)" t)

(register-definition-prefixes "my-latex-insert" '("my-"))

;;;***

;;;### (autoloads nil "my-latex-math-spaces" "my-latex-math-spaces.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from my-latex-math-spaces.el

(defvar my-latex-math-spaces-do-hook nil "\
Hooks which will be run when called `my-latex-math-spaces-do'.")

(register-definition-prefixes "my-latex-math-spaces" '("my-latex-"))

;;;***

;;;### (autoloads nil "my-mipt" "my-mipt.el" (0 0 0 0))
;;; Generated autoloads from my-mipt.el

(autoload 'my-mipt-visit-last-task "my-mipt" "\
Visit last opened task searched via `my-mipt-last-task'." t)

(autoload 'my-mipt-next-task "my-mipt" "\
Return the next MIPT task after the current MIPT task.

If IS-VISIT is t, then also visit the next mipt task

\(fn &optional IS-VISIT)" t)

(autoload 'my-mipt-prev-task "my-mipt" "\
Return previous task, before the last found task.

If IS-VISIT is t, then also visit the next mipt task.

\(fn &optional IS-VISIT)" t)

(autoload 'my-mipt-task-visit "my-mipt" "\
Visit file of the TASK's solution.

\(fn TASK)" t)

(autoload 'my-compress-latex-source "my-mipt" "\
Take SOURCE and return compressed variant.

\(fn SOURCE)")

(register-definition-prefixes "my-mipt" '("my-"))

;;;***

;;;### (autoloads nil "my-org-do-tidy" "my-org-do-tidy.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from my-org-do-tidy.el

(autoload 'my-org-tidy "my-org-do-tidy" "\
Use each of rules tidy org." t)

(register-definition-prefixes "my-org-do-tidy" '("my-org-"))

;;;***

;;;### (autoloads nil "my-org-drag" "my-org-drag.el" (0 0 0 0))
;;; Generated autoloads from my-org-drag.el

(register-definition-prefixes "my-org-drag" '("my-"))

;;;***

;;;### (autoloads nil "my-org-editing" "my-org-editing.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from my-org-editing.el

(autoload 'my-org-clear-subtree "my-org-editing" "\
Kill subtree at the position, and activate insertion mode.")

(defvar my-org-table-cut-map '(keymap (46 . org-cut-special) (99 . org-table-delete-column) (114 . my-kill-line-or-region)))

(autoload 'my-org-cut "my-org-editing" "\
Cut any `org' thing.

If in a table ask from the user: either kill column, kill cell or kill row, if
in a src block cut it, otherwise kill heading" t)

(autoload 'my-org-schedule-to-today "my-org-editing" "\
Scheduale a `org-mode' heading to today." t)

(autoload 'my-org-indent-subtree "my-org-editing" "\
Indent current the `org-mode' subtree at current position." t)

(autoload 'my-org-todo "my-org-editing" "\
My version of the `org-todo'.

Different with the original functions is that this function can be repeated by
pressing of the previous last pressed char.  So if functions is bound to
\"SPC l 1\", that after pressing that user can press \"1\" and this command will
be repeated" t)

(autoload 'my-org-insert-image "my-org-editing" "\
Insert a image with FILENAME.

By default, caption for the image don't inserts, but if CAPTION is a
string, then define caption of the image to the CAPTION.

In the interactive, If the region is active, the FILENAME will be text
in the region.

\(fn FILENAME &optional CAPTION)" t)

(register-definition-prefixes "my-org-editing" '("my-"))

;;;***

;;;### (autoloads nil "my-org-options" "my-org-options.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from my-org-options.el
(autoload 'my-org-options-transient "my-org-options")

(register-definition-prefixes "my-org-options" '("my-org-"))

;;;***

;;;### (autoloads nil "my-organization-commands" "my-organization-commands.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from my-organization-commands.el

(autoload 'my-agenda-plan-new-day "my-organization-commands" "\
Switch to the new day in my organization system." t)

(autoload 'my-org-archive-done-and-saw-headings "my-organization-commands" "\
Archieve all `org-mode' headings which has the label done.")

(autoload 'my-open-main-agenda-file "my-organization-commands" "\
Open agenda.org." t)

;;;***


;;; Generated autoloads from my-doom-modeline-segments.el

(register-definition-prefixes "my-doom-modeline-segments" '("durand-buffer-name-max" "eglot--spinner" "my-"))

;;; Generated autoloads from my-org-editing.el

(autoload 'my-org-clear-subtree "my-org-editing" "\
Kill subtree at the position, and activate insertion mode.")
(defvar my-org-table-cut-map '(keymap (46 . org-cut-special) (99 . org-table-delete-column) (114 . my-kill-line-or-region)))
(autoload 'my-org-cut "my-org-editing" "\
Cut any `org' thing.

If in a table ask from the user: either kill column, kill cell or kill row, if
in a src block cut it, otherwise kill heading" t)
(autoload 'my-org-schedule-to-today "my-org-editing" "\
Scheduale a `org-mode' heading to today." t)
(autoload 'my-org-indent-subtree "my-org-editing" "\
Indent current the `org-mode' subtree at current position." t)
(autoload 'my-org-todo "my-org-editing" "\
My version of the `org-todo'.

Different with the original functions is that this function can be repeated by
pressing of the previous last pressed char.  So if functions is bound to
\"SPC l 1\", that after pressing that user can press \"1\" and this command will
be repeated" t)
(autoload 'my-org-insert-image "my-org-editing" "\
Insert a image with FILENAME.

By default, caption for the image don't inserts, but if CAPTION is a
string, then define caption of the image to the CAPTION.

In the interactive, If the region is active, the FILENAME will be text
in the region.

(fn FILENAME &optional CAPTION)" t)
(register-definition-prefixes "my-org-editing" '("my-"))



;;; Generated autoloads from agnifize.el

(autoload 'agnifize-dwim "agnifize" "\
Agnifize that you mean.

By default agnifize the current buffer.  If a region is selected and active
then agnifize source code between its bounds.  If do in a `dired' buffer, then
try agnifize marked files." t)
(register-definition-prefixes "agnifize" '("agn"))



;;; Generated autoloads from my-latex-insert.el

(autoload 'my-latex-expansion-mode "my-latex-insert" "\
Minor mode which complete text snippets for LaTeX after press dot.

List of the \"snippets\" you can learn using
function `my-latex-insert-commands-help'

This is a minor mode.  If called interactively, toggle the
`My-Latex-Expansion mode' mode.  If the prefix argument is
positive, enable the mode, and if it is zero or negative, disable
the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `my-latex-expansion-mode'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

(fn &optional ARG)" t)
(register-definition-prefixes "my-latex-insert" '("my-"))


;;; Generated autoloads from my-latex-autoformat.el

(add-hook 'LaTeX-mode-hook '(lambda () (require 'my-latex-autoformat)))
(register-definition-prefixes "my-latex-autoformat" '("autoformat-latex-" "my-"))



;;; Generated autoloads from my-latex-embrace.el

(autoload 'my-embrace-LaTeX-mode-hook "my-latex-embrace" "\
My additional `embrace-LaTeX-mode-hook'." t)
(register-definition-prefixes "my-latex-embrace" '("my-"))


;;; Generated autoloads from my-latex-drag.el

(autoload 'my-latex-try-drag-right-list-item "my-latex-drag" "\
If the dragger for LaTeX list item should be work, drag that to right." t)
(autoload 'my-latex-try-drag-left-list-item "my-latex-drag" "\
If the dragger for LaTeX list item should be work, drag that to left." t)
(register-definition-prefixes "my-latex-drag" '("my-latex-"))


;;; Generated autoloads from my-meow-structural.el

(register-definition-prefixes "my-meow-structural" '("my-meow-structural-"))


;;; Generated autoloads from my-dired-commands.el

(autoload 'my-dired-mark-all-files "my-dired-commands" "\
Mark all file in `dired'." t)
(autoload 'my-dired-rename "my-dired-commands")
(autoload 'my-dired-move "my-dired-commands" "\
Move file of current directory of `dired' at the point." t)
(autoload 'my-dired-delete "my-dired-commands")
(autoload 'my-dired-goto-parent-dir "my-dired-commands" "\
Navigate to parent directory of current dired directory." t)
(autoload 'my-dired-new-file "my-dired-commands" "\
Create file with FILENAME in the directory which opened in the Dired buffer.

(fn FILENAME)" t)
(autoload 'my-dired-delete-all-files "my-dired-commands" "\
Delete all files from the directory of the `dired' buffer." t)
(autoload 'dired-avy "my-dired-commands" "\
Version of `avy' for the `dired'." t)
(autoload 'my-dired-duplicate "my-dired-commands")
(autoload 'my-dired-jump-to-home "my-dired-commands" "\
Open a `dired' buffer of the home directory." t)
(register-definition-prefixes "my-dired-commands" '("my-"))
