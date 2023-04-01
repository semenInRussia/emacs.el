;;; my-autoload.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:


;;;### (autoloads nil "my-dired-commands" "my-dired-commands.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from my-dired-commands.el

(autoload 'my-dired-mark-all-files "my-dired-commands" "\
Mark all file in `dired'." t nil)
(autoload 'my-dired-rename "my-dired-commands")

(autoload 'my-dired-move "my-dired-commands" "\
Move file of current directory of `dired' at the point." t nil)
(autoload 'my-dired-delete "my-dired-commands")

(autoload 'my-dired-goto-parent-dir "my-dired-commands" "\
Navigate to parent directory of current dired directory." t nil)

(autoload 'my-dired-new-file "my-dired-commands" "\
Create file with FILENAME in the directory which opened in the Dired buffer.

\(fn FILENAME)" t nil)

(autoload 'my-dired-delete-all-files "my-dired-commands" "\
Delete all files from the directory of the `dired' buffer." t nil)

(autoload 'dired-avy "my-dired-commands" "\
Version of `avy' for the `dired'." t nil)
(autoload 'my-dired-duplicate "my-dired-commands")

(autoload 'my-dired-jump-to-home "my-dired-commands" "\
Open a `dired' buffer of the home directory." t nil)

(register-definition-prefixes "my-dired-commands" '("my-"))

;;;***

;;;### (autoloads nil "my-elisp-class-fields" "my-elisp-class-fields.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from my-elisp-class-fields.el

(autoload 'my-elisp-new-field-of-class "my-elisp-class-fields" "\
Insert new field of Lisp class.
Only when in class defnition." t nil)

(register-definition-prefixes "my-elisp-class-fields" '("my-"))

;;;***

;;;### (autoloads nil "my-elisp-embrace" "my-elisp-embrace.el" (0
;;;;;;  0 0 0))
;;; Generated autoloads from my-elisp-embrace.el

(autoload 'my-embrace-emacs-lisp-mode-hook "my-elisp-embrace" "\
Add some parens for the Emacs-Lisp embrace." nil nil)

(register-definition-prefixes "my-elisp-embrace"
                              '("my-embrace-emacs-lisp-with-"))

;;;***

;;;### (autoloads nil "my-films" "my-films.el" (0 0 0 0))
;;; Generated autoloads from my-films.el

(autoload 'my-films-add "my-films" "\
Add FILM to current org file, this file is db of films.

\(fn FILM)" t nil)

(autoload 'my-films-format-as-org-heading "my-films" "\
Format an `kinopoisk-film' readed from the minibuffer as an org entry." nil nil)

(register-definition-prefixes "my-films" '("my-"))

;;;***

;;;### (autoloads nil "my-helm-gitignore" "my-helm-gitignore.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from my-helm-gitignore.el

(autoload 'helm-gitignore "my-helm-gitignore" "\
Helm to generate .gitignore using gitignore.io." t nil)

(register-definition-prefixes "my-helm-gitignore"
                              '("helm-gitignore--"))

;;;***

;;;### (autoloads nil "my-mipt" "my-mipt.el" (0 0 0 0))
;;; Generated autoloads from my-mipt.el

(register-definition-prefixes "my-mipt" '("my-"))

;;;***

;;;### (autoloads nil "my-org-do-tidy" "my-org-do-tidy.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from my-org-do-tidy.el

(autoload 'my-org-tidy "my-org-do-tidy" "\
Use each of rules tidy org." t nil)

(register-definition-prefixes "my-org-do-tidy" '("my-org-"))

;;;***

;;;### (autoloads nil "my-org-editing" "my-org-editing.el" (25610
;;;;;;  5732 0 0))
;;; Generated autoloads from my-org-editing.el

(autoload 'my-org-clear-subtree "my-org-editing" "\
Kill subtree at the position, and activate insertion mode." nil nil)

(defvar my-org-table-cut-map
  '(keymap
    (46 . org-cut-special)
    (99 . org-table-delete-column)
    (114 . my-kill-line-or-region)))

(autoload 'my-org-cut "my-org-editing" "\
Cut any `org' thing.

If in a table ask from the user: either kill column, kill cell or kill row, if
in a src block cut it, otherwise kill heading" t nil)

(autoload 'my-org-schedule-to-today "my-org-editing" "\
Scheduale a `org-mode' heading to today." t nil)

(autoload 'my-org-indent-subtree "my-org-editing" "\
Indent current the `org-mode' subtree at current position." t nil)

(autoload 'my-org-todo "my-org-editing" "\
My version of the `org-todo'.

Different with the original functions is that this function can be repeated by
pressing of the previous last pressed char.  So if functions is bound to
\"SPC l 1\", that after pressing that user can press \"1\" and this command will
be repeated" t nil)

(autoload 'my-org-insert-image "my-org-editing" "\
Insert a image with FILENAME.

By default, caption for the image don't inserts, but if CAPTION is a
string, then define caption of the image to the CAPTION.

In the interactive, If the region is active, the FILENAME will be text
in the region.

\(fn FILENAME &optional CAPTION)" t nil)

(register-definition-prefixes "my-org-editing" '("my-"))

;;;***

;;;### (autoloads nil "my-org-options" "my-org-options.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from my-org-options.el
(autoload 'my-org-options-transient "my-org-options")

(register-definition-prefixes "my-org-options" '("my-org-"))

;;;***

;;;### (autoloads nil "my-project" "my-project.el" (0 0 0 0))
;;; Generated autoloads from my-project.el

(autoload 'my-project-root "my-project" "\
Root of the project at DIR.

\(fn &optional DIR)" nil nil)

(autoload 'my-projectile-project-files "my-project" "\
Return filenames list of the project at ROOT, with caching.

If RELATIEVE-PATHS is non-nil, instead of the returns path relatieve to the
ROOT

\(fn ROOT &optional RELATIEVE-PATHS)" nil nil)

(autoload 'projectile-project-files-clear-cache "my-project" "\
Function `projectile-project-files' is cached, clear this cache for ROOT.

\(fn ROOT)" t nil)

(autoload 'my-projectile-files-with-string "my-project" "\
Return a list of all files containing STRING in DIRECTORY.

\(fn STRING DIRECTORY &optional FILE-EXT)" nil nil)

(register-definition-prefixes "my-project" '("my-"))

;;;***

(provide 'my-autoload)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; my-autoload.el ends here
