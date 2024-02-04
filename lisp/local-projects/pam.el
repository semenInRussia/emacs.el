;;; pam.el --- A fast Emacs package manager which is built over `straight' -*- lexical-binding: t; -*-

;; Copyright (C) 2023 semenInRussia

;; Author: semenInRussia <hrams205@gmail.com>
;; Version: 0.0.1
;; Package-Requires: (straight)

;;; Commentary:

;; A fast Emacs package manager which is built over `straight'.

;; When use a package instead of `straight-use-package' just add to the load
;; path only ONE directory, load autoloads.  `straight' here add about 190~
;; paths to the load path and load the same amount of autoloads files.  The
;; `pam' version is more simple and lightweight, but the cons is that to install
;; packages (don't activate) it still uses straight, if your packages didn't
;; installed, you should add the --install option when run Emacs in your
;; terminal, it replaces the default behaviour with the default `straight' way.
;; Also if you need change the `pam' behaviour inside of a running Emacs session
;; you can use the `pam-install-everything-mode' minor mode.

;; The main function which you should use in 90% of cases when you use `pam' is
;; `pam-use' it's like `straight-use-package', but don't add any path to the
;; `load-path', don't explore TeXinfo documents, don't check if the package must
;; be byte-compiled.  All these things you can do once when call
;; `pam-load-packages', it have the complexity O(1) (don't depend on the amount
;; of packages) and looks like one call of `straight-use-package'.  Instead
;; `pam-use' just install package if you enabled the respective mode (or used
;; --install flag when call Emacs) or do NOTHING.  In real cases you install the
;; config on other computer, run "emacs --install" and all packages will be
;; installed with `straight' and `pam', so you can use Emacs in other session
;; with just call "emacs" without flags and forgot about `straight' ever.

;; The most the package functions are aliases to their `straight' alternatives,
;; the idea is forgot about the fact that `pam' is built over `straight' and just
;; use pam-<one>, pam-<anotherone> and other package manager functions and don't
;; think where function is define in `straight' or `pam'.  The "real" code in
;; this package placed in little doses.

;;; Code:

;;; autoload `straight' variables and functions.
;;
;; NOTE: here I don't wrote (require \\='straight), because in the most of Emacs
;;   sessions `straight' is extra dependency of `pam', `straight' will be loaded
;;   only when it's really imported.  It saves a little bit of time
(autoload 'straight--build-dir "straight")
(autoload 'straight-fetch-package "straight")
(autoload 'straight-get-recipe "straight")
(autoload 'straight-pull-recipe-repositories "straight")
(autoload 'straight-rebuild-all "straight")
(autoload 'straight-rebuild-package "straight")
(autoload 'straight-use-package "straight")

(eval-and-compile
  (defvar straight-use-package-post-build-functions)
  (defvar straight--repo-cache)
  (require 'subr-x))


(defgroup pam nil
  "My simple package manager which is built over `straight'."
  :group 'emacs)

(defcustom pam-need-to-install-pkgs-p (member "--install" command-line-args)
  "It is a non-nil value if `pam' should install packages."
  :group 'pam
  :type 'hook)

(defcustom pam-build-dir (locate-user-emacs-file "pam/")
  "Path where all package files (and autoloads) are saved.

Note that if you need in access the autoloads packages file use
`pam--build-dir' function instead of this variable."
  :group 'pam
  :type 'string)

(defcustom pam-autoloads-filename "my-packages-autoloads.el"
  "Name of the file in which located all packages autoloads.

Note that if you need in access the autoloads packages file use
`pam--autoloads-file' function instead of this variable, the function return the
full path, instead of filename.

By default, file with this name is located inside inside `pam-build-dir', but
you can redefine `pam--autoloads-file' with `advice'"
  :group 'pam
  :type 'string)

;; TODO: ability to choose copy or move `straight' files
(defcustom pam-post-build-functions
  '(pam--copy-straight-files
    pam--save-pkg-autoloads)
  "Abnormal hook run after building a package with `pam-use-package'.

Each hook function is called with the name of the package as a
string.  For forward compatibility, it should accept and ignore
additional arguments.

Defaults to 2 functions:
1. that copies `straight' files into the `pam' build directory.
2. that append autoloads into the `pam' autloads file"
  :group 'pam
  :type 'hook)

(defvar pam-straight-already-loaded-p nil
  "Determine that straight.el was already loaded.

Note that `featurep' here isn't work, because we also should do some
other things (load source code + some other things, see sources of
straight.el)")

;;; Macros

;; (macros defined before the public part, because they affect on byte-compiler)

(defmacro pam--with-straight-hooks (&rest body)
  "Evaluate the BODY with changed hooks for `straight'."
  (declare (indent 0))
  `(progn
     (pam--load-straight)
     (let ((straight-use-package-post-build-functions
            (append
             straight-use-package-post-build-functions
             ;; add specific functions which will be runned after build.  Here I
             ;; don't use `add-hook' to the `straight' hooks, because the user
             ;; should can to choose `straight-use-package' or `pam-use-package'
             ;; and they should have the different behaviours.
             pam-post-build-functions)))
       ,@body)))

;;; Public

(define-minor-mode pam-install-everything-mode
  "Toggle should or shouldn't `pam' install new packages.

With enabled this minor mode, `pam-use-package' will use `straight-use-package'
to install a package while the default behaviour is do nothing because already
all is installed"
  :global t
  :init-value nil
  :variable pam-need-to-install-pkgs-p
  :group 'pam)

(defun pam-use-package (melpa-style-recipe &optional no-clone no-build)
  "Activate the package and MAYBE install it.

The package is defined with MELPA-STYLE-RECIPE (see the `straight'
documentation).  The behaviour depends on the value of the
`pam-need-to-install-pkgs-p' variable.  If packages must be installed then this
function install them using `straight' with passing NO-CLONE and NO-BUILD to
`straight-use-package' (see below details), otherwise (packages already
installed) do nothing, notice that here activation is extra, because in `pam'
you activate all packages only once.

The result value of this function is either nil, that tells package wasn't
installed or activated or non-nil value otherwise.

The paragraphs below tells about arguments which are used if package should be
installed:

First, the package recipe is registered with straight.el.  If NO-CLONE is a
function, then it is called with two arguments: the package name as a string,
and a boolean value indicating whether the local repository for the package is
available.  In that case, the return value of the function is used as the value
of NO-CLONE instead.  In any case, if NO-CLONE is non-nil, then processing stops
here.

Otherwise, the repository is cloned, if it is missing.  If NO-BUILD is a
function, then it is called with one argument: the package name as a string.  In
that case, the return value of the function is used as the value of NO-BUILD
instead.  In any case, if NO-BUILD is non-nil, then processing halts here.
Otherwise, the package is built and activated.  Note that if the package recipe
has a nil `:build' entry, then NO-BUILD is ignored and processing always stops
before building and activation occurs."
  (interactive (list
                (if (not pam-need-to-install-pkgs-p)
                    (user-error "Sorry, you can't install a pkg with `pam' when the `pam-install-everything-mode' is disabled")
                  (straight-get-recipe
                   (when current-prefix-arg 'interactive) nil
                   (let ((installed nil))
                     ;; Cache keys are :local-repo. We want to compare :package.
                     (maphash (lambda (_ v) (push (plist-get v :package) installed))
                              straight--repo-cache)
                     (lambda (pkg) (not (member pkg installed))))))))
  (if (not pam-need-to-install-pkgs-p)
      t
    (pam--with-straight-hooks
      (straight-use-package melpa-style-recipe no-clone no-build))))

(defun pam-fetch-package (package)
  "Fetch the source of a PACKAGE and save it in the `pam' directory.

You can call this function update.  The difference with
`straight-fetch-package' is that files will be synced with the `pam'
directory and autoloads file with `pam-autoloads-filename'."
  (interactive (list
                (completing-read "Which recipe to update: "
                                 (pam--straight-packages))))
  (pam--with-straight-hooks
    (straight-fetch-package package 'from-upstream)))

(defun pam-rebuild-package (package &optional recursive)
  "Rebuild the PACKAGE.

The package is defined with PAKCAGE (see the `straight'
documentation).  The difference with `straight-rebuild-package' is that after
build files will be copied into the `pam' directory and autoloads file will be
updated.  In other it is the same function PACKAGE and RECURSIVE will
be passed to `straight-rebuild-package'

Notice that while `pam-use-package' check the mode (install or only activate a
package), but `pam-rebuild-package' don't it, because rebuild is a more concrete
command."
  (interactive (list
                (completing-read "Which recipe to rebuild: "
                                 (pam--straight-packages))))
  (pam--with-straight-hooks
    (straight-rebuild-package package recursive)))

(defun pam-rebuild-all ()
  "Rebuild all installed packages.

Notice that while `pam-use-package' check the mode (install or only activate a
package), but `pam-rebuild-all' don't it, because rebuild is a more concrete
command."
  (interactive)
  (pam--with-straight-hooks
    (straight-rebuild-all)))

(defun pam-activate ()
  "Activate all installed `pam' packages.

After that you can load any installed package with `require', `M-x' will show
all commands of these packages, TeXinfo will be included in the manual."
  (interactive)
  (add-to-list 'load-path (pam--build-dir))
  (add-to-list 'Info-default-directory-list (pam--build-dir))
  (pam-create-files)
  (load (string-trim-right (pam--autoloads-file) ".el")
        :noerror
        :nomessage))

(defun pam-delete-package (pkg &optional update-autoloads)
  "Remove the PKG from the `pam' directory.

If UPDATE-AUTOLOADS is non-nil, then update my-packages-autoloads.el, NOTE that
is a heavy function which can take a time, because it update autoloads for EVERY
package"
  (interactive
   (list
    (completing-read "Which package? " (pam--straight-packages))
    'interactive))
  (let* ((default-directory (pam--build-dir))
         (build-dir (straight--build-dir pkg)))
    (thread-last
      build-dir
      (directory-files)
      ;; `cddr' skips "." and ".."
      (cddr)
      (mapc #'delete-file))
    (when (file-exists-p build-dir)
      (delete-directory build-dir :recursive))
    ;; update the autoloads file for EVERY package, because delete only the part
    ;; of my-package-autoloads is hard, if you should delete some `pam'
    ;; packages, call `pam-delete-package' some times and only after manually
    ;; call `pam-update-all-packages-autoloads'
    (when update-autoloads
      (pam-update-all-packages-autoloads))))

(defun pam-sync-with-straight ()
  "Copy all `straight' packages files into the `pam' dir, make autoloads file.

Notice that it can take a long time."
  (interactive)
  (delete-file (pam--autoloads-file))
  (dolist (pkg (pam--straight-packages))
    (pam--sync-straight-package pkg)))

(defun pam-update-all-packages-autoloads ()
  "Add autoloads of every package to my-packages-autoloads.el."
  (delete-file (pam--autoloads-file))
  (let ((pkgs (pam--straight-packages)))
    (while pkgs
      (pam--save-pkg-autoloads (car pkgs))
      (setq pkgs (cdr pkgs)))))

(defun pam-create-files ()
  "Make the `pam' build directory and touch the autoloads file."
  (unless (file-exists-p (pam--build-dir))
    (make-directory (pam--build-dir))))

;;; Internal

(defun pam--build-dir ()
  "Return the path to the directory where `pam' store all packages files."
  ;; Yes, here I just return the value of a variable.
  ;; It's better than a variable, because `straight' use variables to get paths
  ;; and `pam' must use function `pam--autoloads-file' that return the path,
  ;; user shouldn't guess: Is it function or variable?  Choice already happened:
  ;; everywhere - function
  pam-build-dir)

(defun pam--autoloads-file ()
  "Return the path to the directory where `pam' store all packages files."
  (file-name-concat pam-build-dir
                    pam-autoloads-filename))

(defun pam--straight-packages ()
  "Get the list of the packages names that were installed with `straight'."
  (cddr  ;; here `cddr' skips "." and ".." from the front list
   (directory-files (straight--build-dir))))

(defun pam--load-straight ()
  "Load straight.el for `pam' things."
  (unless pam-straight-already-loaded-p
    (defvar bootstrap-version)
    (let ((bootstrap-file
           (expand-file-name
            "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
          (bootstrap-version 6))
      (unless (file-exists-p bootstrap-file)
        (with-current-buffer (url-retrieve-synchronously
                              "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
                              'silent 'inhibit-cookies)
          (goto-char (point-max))
          (eval-print-last-sexp)))
      (load bootstrap-file nil 'nomessage))
    (require 'straight)
    ;; (straight-pull-recipe-repositories)
    (setq pam-straight-already-loaded-p t)))

(defun pam--sync-straight-package (pkg &rest _ignore)
  "Sync with `pam' PKG which have already installed and built with `straight'.

It can be hook to `straight-use-package-post-build-functions' instead of other
`pam' functions"
  (run-hook-with-args 'pam-post-build-functions
                      pkg))

(defun pam--copy-straight-files (pkg &optional _ignore)
  "Copy the files of `straight' PKG into the `pam-build-dir'.

Notice that this function expect that PKG was already built, otherwise this
function can do really strange things.

This is a hook for `straight-use-package-post-build-functions', but it still can
be useful in other cases"
  (pam--copy-files
   (straight--build-dir pkg)
   (pam--build-dir)))

(defun pam--copy-files (src dst)
  "Copy all files and directories inside SRC to the DST directory.

The difference with just copy directory, is that it tries to merge conflicted
directories, instead of just replacing one with other"
  (let ((default-directory src))
    (dolist (file (cddr (directory-files src)))
      ;; copy files: SRC -> DST
      (cond
       ;; make a directory in DST and move all SRC files into it
       ((file-directory-p file)
        (unless (file-exists-p (file-name-concat dst file))
          (make-directory (file-name-concat dst file)))
        (pam--copy-files (expand-file-name file)
                         (file-name-concat dst file)))
       ;; delete a file from DST and copy from SRC
       (t
        (ignore-errors
          (delete-file (file-name-concat dst file)))
        (copy-file file (file-name-concat dst file) 'ok-if-already-exists))))))

(defun pam--save-pkg-autoloads (pkg &optional _ignore)
  "Save all autoloads of `straight' built PKG into `pam' autoloads file.

Notice that this function expect that PKG was already built, otherwise this
function can do really strange things.

Also this function doesn't GENERATE autoloads for PKG, expected that `straight'
already done it, here just save them.

This is a hook for `straight-use-package-post-build-functions', but it still can
be useful in other cases."
  (with-temp-buffer
    ;; insert all autoloads files contents
    (dolist (file (cddr (directory-files (straight--build-dir pkg) 'full)))
      (when (string-suffix-p "-autoloads.el" file)
        (insert-file-contents file)))
    ;; write it to the end of autoloads file
    (write-region (point-min) (point-max)
                  (pam--autoloads-file)
                  'append)))

(defun pam-byte-compile-pkg-autoloads ()
  "Byte compile `pam' package autoloads file.

This is a file where `pam' stored all autoloads of every package."
  (interactive)
  (setq byte-compile-last-logged-file nil)
  (let* ((filename (pam--autoloads-file))
         (byte-compile-current-file filename)
         (byte-compile-current-group nil)
         (set-auto-coding-for-load t)
         (byte-compile--seen-defvars nil)
         (byte-compiler-error-flag nil)
         (byte-compile--known-dynamic-vars
          (byte-compile--load-dynvars (getenv "EMACS_DYNVARS_FILE")))
         target-file input-buffer output-buffer
         byte-compile-dest-file)
    (setq target-file (byte-compile-dest-file filename))
    (setq byte-compile-dest-file target-file)
    (with-current-buffer
        ;; It would be cleaner to use a temp buffer, but if there was
        ;; an error, we leave this buffer around for diagnostics.
        ;; Its name is documented in the lispref.
        (setq input-buffer (get-buffer-create
                            (concat " *Compiler Input*"
                                    (if (zerop byte-compile-level) ""
                                      (format "-%s" byte-compile-level)))))
      (erase-buffer)
      (setq buffer-file-coding-system nil)
      ;; Always compile an Emacs Lisp file as multibyte
      ;; unless the file itself forces unibyte with -*-coding: raw-text;-*-
      (set-buffer-multibyte t)
      (insert-file-contents filename)
      ;; Mimic the way after-insert-file-set-coding can make the
      ;; buffer unibyte when visiting this file.
      (when (or (eq last-coding-system-used 'no-conversion)
                (eq (coding-system-type last-coding-system-used) 5))
        ;; For coding systems no-conversion and raw-text...,
        ;; edit the buffer as unibyte.
        (set-buffer-multibyte nil))
      ;; Run hooks including the uncompression hook.
      ;; If they change the file name, then change it for the output also.
      (let ((buffer-file-name filename)
            (dmm (default-value 'major-mode))
            ;; Ignore unsafe local variables.
            ;; We only care about a few of them for our purposes.
            (enable-local-variables :safe)
            (enable-local-eval nil))
        (unwind-protect
            (progn
              (setq-default major-mode 'emacs-lisp-mode)
              ;; Arg of t means don't alter enable-local-variables.
              (delay-mode-hooks (normal-mode t)))
          (setq-default major-mode dmm))
        ;; There may be a file local variable setting (bug#10419).
        (setq buffer-read-only nil
              filename buffer-file-name))
      ;; Don't inherit lexical-binding from caller (bug#12938).
      (unless (local-variable-p 'lexical-binding)
        (setq-local lexical-binding nil))
      ;; Set the default directory, in case an eval-when-compile uses it.
      (setq default-directory (file-name-directory filename)))
    (when byte-compile-verbose
      (message "Compiling %s..." filename))
    ;; It is important that input-buffer not be current at this call,
    ;; so that the value of point set in input-buffer
    ;; within byte-compile-from-buffer lingers in that buffer.
    (setq output-buffer
          (save-current-buffer
            (let ((byte-compile-level (1+ byte-compile-level)))
              (byte-compile-from-buffer input-buffer))))
    (if byte-compiler-error-flag
        nil
      (when byte-compile-verbose
        (message "Compiling %s...done" filename))
      (kill-buffer input-buffer)
      (with-current-buffer output-buffer
        (when (and target-file
                   (or (not byte-native-compiling)
                       (and byte-native-compiling byte+native-compile)))
          (goto-char (point-max))
          (insert "\n")			; aaah, unix.
          (cond
           ((and (file-writable-p target-file)
                 ;; We attempt to create a temporary file in the
                 ;; target directory, so the target directory must be
                 ;; writable.
                 (file-writable-p
                  (file-name-directory
                   ;; Need to expand in case TARGET-FILE doesn't
                   ;; include a directory (Bug#45287).
                   (expand-file-name target-file))))
            (if byte-native-compiling
                ;; Defer elc production.
                (setf byte-to-native-output-buffer-file
                      (cons (current-buffer) target-file))
              (byte-write-target-file (current-buffer) target-file))
            (or noninteractive
                byte-native-compiling
                (message "Wrote %s" target-file)))
           ((file-writable-p target-file)
            ;; In case the target directory isn't writable (see e.g. Bug#44631),
            ;; try writing to the output file directly.  We must disable any
            ;; code conversion here.
            (let ((coding-system-for-write 'no-conversion))
              (with-file-modes (logand (default-file-modes) #o666)
                (write-region (point-min) (point-max) target-file nil 1)))
            (or noninteractive (message "Wrote %s" target-file)))
           (t
            ;; This is just to give a better error message than write-region
            (let ((exists (file-exists-p target-file)))
              (signal (if exists 'file-error 'file-missing)
                      (list "Opening output file"
                            (if exists
                                "Cannot overwrite file"
                              "Directory not writable or nonexistent")
                            target-file))))))
        (unless byte-native-compiling
          (kill-buffer (current-buffer))))
      (if (and byte-compile-generate-call-tree
               (or (eq t byte-compile-generate-call-tree)
                   (y-or-n-p (format "Report call tree for %s? "
                                     filename))))
          (save-excursion
            (display-call-tree filename)))
      (let ((gen-dynvars (getenv "EMACS_GENERATE_DYNVARS")))
        (when (and gen-dynvars (not (equal gen-dynvars ""))
                   byte-compile--seen-defvars)
          (let ((dynvar-file (concat target-file ".dynvars")))
            (message "Generating %s" dynvar-file)
            (with-temp-buffer
              (dolist (var (delete-dups byte-compile--seen-defvars))
                (insert (format "%S\n" (cons var filename))))
              (write-region (point-min) (point-max) dynvar-file)))))
      t)))

(provide 'pam)
;;; pam.el ends here
