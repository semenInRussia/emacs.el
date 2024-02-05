;;; init.el --- Load my config
;;; Commentary:
;;; Code:

(require 'cl-lib)
(require 'subr-x)  ; for `string-remove-prefix'

;; every custom variable of my config have the following group
(defgroup my nil "Group for all my config files." :group 'tools)

;; Increase how much is read from processes in a single chunk (default is 4kb).
;; This is further increased elsewhere, where needed (like our LSP module).
(setq read-process-output-max (* 64 1024))  ; 64kb

;; PERF: Line numbers are pretty slow all around. The performance boost of disabling
;; them outweighs the utility of always keeping them on.
;;(setq display-line-numbers-type nil)

;; change Emacs config directory depends on init file
;;
;; after this config you can easily run Emacs with "emacs -l init.el"
;; not only when init.el inside ~/.emacs.d
(setq user-emacs-directory
      (file-name-directory (or load-file-name
                               (buffer-file-name))))

;;; Handle some CLI options
;; byte-compile local-projects and generate autoloads
(when (member "--local-projects" command-line-args)
  ;; generate autoloads
  (loaddefs-generate (locate-user-emacs-file "lisp/local-projects")
                     (locate-user-emacs-file "lisp/local-projects/my-autoload.el"))
  ;; byte-compile every file from the "local-projects" dir (including autoloads
  ;; file)
  (dolist (file (directory-files (locate-user-emacs-file "lisp/local-projects/")
                                 'full
                                 ".*\\.el$"))
    (byte-compile-file file)))

;;; Local Projects
;; It is my own small "packages" which aren't so big to create real packages
(eval-and-compile
  ;; add some files into the `load-path' that config files can require
  ;; them and byte-compiler will be happy
  (add-to-list 'load-path (locate-user-emacs-file "lisp/"))
  (add-to-list 'load-path (locate-user-emacs-file "lisp/package-management/"))
  (add-to-list 'load-path (locate-user-emacs-file "lisp/local-projects"))
  (load (locate-user-emacs-file "lisp/local-projects/my-autoload") :noerror :nomessage))

(require 'my-bench)

;;; add to `load-path' all installed packages
;;
;; I'm use `pam' which is built over straight.
;;
;; if packages was already installed, then every package was loaded
;; from the `pam' directory, it's more faster than load packages from
;; package specific directories (what do `straight') because in 2nd
;; case `load-path' will contain about 200+(*) directories and to load
;; one package with `require' Emacs will checks all these dirs.  When
;; all packages files located inside the `pam' dir Emacs checks only
;; `pam' dir (instead of 200 other dirs).
;;
;; O(n) vs O(1) to load one package (n is amount of packages)
;;
;; This optimization matters for Windows, where slow IO
;;
;; NOTE: amount of the directories in the `load-path' depends on amount of the
;;   packages and their dependencies (if you use straight)
(require 'pam)
(pam-activate)

;; generate and byte-compile my-modules.el
;;
;; NOTE: I do it after local-projects, because `my-build-config' is a local
;; project too
(when (member "--modules" command-line-args)
  (require 'my-build-config)
  (my-build-config))

;; don't use init.el for custom.el which I don't use
;;
;; in the most of configurations, after it Emacs load custom.el, but I fount it
;; a bit useless.  I prefer `setq' over `custom'
;;(setq custom-dont-initialize t)
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

;; some useful macros
(require 'my-macros)

(declare-function nano-agenda "nano-agenda")
(declare-function my-require-times "init")
(declare-function my-build-config "my-build-config.el")
(declare-function org-roam-node-find "org-roam")

(add-to-list!
 'command-line-functions
 ;; --kill
 (defun my-kill-cli-handle-arg ()
   "Handle --kill command-line argument.

Argument was named --kill, because it kill Emacs after Emacs is load.  It
useful, if you needed in only install packages, byte compile config and other
these things.

This is function for `command-line-functions'."
   (when (string-equal argi "--kill")
     ;; it handled above
     (kill-emacs))

   ;; --modules
   (defun my-modules-cli-handle-arg ()
     "Handle --modules command-line argument.

Argument was named --modules, because it build my-modules.el file (or
`my-modules-el-file').  So when this argument is specified, then build
my-modules.el file with the `my-build-config' function

This is function for `command-line-functions'."
     (when (string-equal argi "--modules")
       ;; it handled above
       t)))

 ;; --local-projects
 (defun my-local-projects-cli-handle-arg ()
   "Handle --local-projects command-line argument.

Byte-compile every file of local-projects and generate autoloads file"
   (when (string-equal argi "--local-projects")
     ;; it handled above
     t))

 ;; --show-bench
 (defun my-show-bench-cli-handle-arg ()
   "Handle --install command-line argument."
   (when (string-equal argi "--show-bench")
     (prog1 t
       ;; `my-require-times' is defined in init.el
       (my-require-times))))

 ;; --zettel
 (defun my-handle-cli-zettel ()
   "Handle --zettel command line ARG.

When you apply this command line argument after init Emacs open one of the
Zettelkasten node"
   (when (string-equal argi "--zettel")
     (prog1 t
       (message "zettel")
       (org-roam-node-find))))

 ;; --agenda
 (defun my-handle-cli-agenda ()
   "Handle --agenda command line ARG.

When you apply this command line argument after init Emacs open the my agenda"
   (when (string-equal argi "--agenda")
     (prog1 t
       (nano-agenda)))))


;; don't load anything useless at the startup (like `emacs-lisp-mode' for
;; *Scratch* or `dashboard')
(setq initial-major-mode 'fundamental-mode
      initial-scratch-message "Good Luck!\n: you can start")

;; PERF,UX: Remove "For information about GNU Emacs..." message at startup.
;;   It's redundant with our dashboard and incurs a premature redraw.
(advice-add #'display-startup-echo-area-message :override #'ignore)
;; PERF: Suppress the vanilla startup screen completely. We've disabled it
;;   with `inhibit-startup-screen', but it would still initialize anyway.
;;   This involves some file IO and/or bitmap work (depending on the frame
;;   type) that we can no-op for a free 50-100ms boost in startup time.
(advice-add #'display-startup-screen :override #'ignore)

;;; Load all config files

;; the most part of the config located inside "~/.emacs.d/lisp" I join all .el
;; files into the my-modules file for fast start up
(defvar my-modules-el-file (locate-user-emacs-file "dist/my-modules.el"))

(unless (file-exists-p (file-name-directory my-modules-el-file))
  (user-error "File \"my-modules.el\" didn't created, suggest use --modules option"))

(add-to-list 'load-path (file-name-directory my-modules-el-file))


(let ((file-name-handler-alist nil)
      (load-suffixes '(".elc" ".el"))
      (load-rep-load-file-rep-suffixes '(""))
      )
  (require 'my-modules))

(provide 'init)
;;; init.el ends here
