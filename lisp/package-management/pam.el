;;; pam.el --- A fast Emacs package manager which is built over `straight' -*- lexical-binding: t; -*-

;; Copyright (C) 2023 semenInRussia

;; Author: semenInRussia <hrams205@gmail.com>
;; Version: 0.0.1
;; Package-Requires: ((dash "2.18.0") (s "1.12.0") straight)

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
(autoload 'straight-use-package "straight")
(autoload 'straight--build-dir "straight")
(autoload 'straight-rebuild-package "straight")
(eval-and-compile
  (defvar straight-use-package-post-build-functions))


(defgroup pam nil
  "My simple package manager which is built over `straight'."
  :group 'emacs)

(defcustom pam-need-to-install-pkgs-p (member "--install" command-line-args)
  "It is a non-nil value if `pam' should install packages."
  :group 'pam
  :type 'hook)

(defcustom pam-build-dir "~/.emacs.d/pam/"
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

;;; Macros

;; (macros defined before the public part, because they affect on byte-compiler)

(defmacro pam--with-straight-hooks (&rest body)
  "Evaluate the BODY with changed hooks for `straight'."
  (declare (indent 0))
  `(let ((straight-use-package-post-build-functions
          (append
           straight-use-package-post-build-functions
           ;; add specific functions which will be runned after build.  Here I
           ;; don't use `add-hook' to the `straight' hooks, because the user
           ;; should can to choose `straight-use-package' or `pam-use-package'
           ;; and they should have the different behaviours.
           pam-post-build-functions)))
     ,@body))

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
  (interactive (read (read-string "Which recipe: ")))
  (if (not pam-need-to-install-pkgs-p)
      t
    (pam--with-straight-hooks
      (straight-use-package melpa-style-recipe no-clone no-build))))

(defun pam-rebuild-package (melpa-style-recipe &optional recursive)
  "Rebuild the package.

The package is defined with MELPA-STYLE-RECIPE (see the `straight'
documentation).  The difference with `straight-rebuild-package' is that after
build files will be copied into the `pam' directory and autoloads file will be
updated.  In other it is the same function MELPA-STYLE-RECIPE and RECURSIVE will
be passed to `straight-rebuild-package'

Notice that while `pam-use-package' check the mode (install or only activate a
package), but `pam-rebuild-package' don't it, because rebuild is a more concrete
command."
  (interactive (read (read-string "Which recipe to rebuild: ")))
  (pam--with-straight-hooks
    (straight-rebuild-package melpa-style-recipe recursive)))

(defun pam-activate ()
  "Activate all installed `pam' packages.

After that you can load any installed package with `require', `M-x' will show
all commands of these packages, TeXinfo will be included in the manual."
  (interactive)
  (add-to-list 'load-path (pam--build-dir))
  (add-to-list 'Info-default-directory-list (pam--build-dir))
  (load (pam--autoloads-file)))

(defun pam-sync-with-straight ()
  "Copy all `straight' packages files into the `pam' dir, make autoloads file.

Notice that it can take a long time."
  (interactive)
  (delete-file (pam--autoloads-file))
  (dolist (pkg (pam--straight-packages))
    (pam--sync-straight-package pkg)))

(defun pam-create-files ()
  "Make the `pam' build directory and touch the autoloads file."
  (make-directory (pam--build-dir)))

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

(defun pam--sync-straight-package (pkg &rest _ignore)
  "Sync with `pam' PKG which have already installed and built with `straight'.

It can be hook to `straight-use-package-post-build-functions' instead of other
`pam' functions"
  (run-hook-with-args pam-post-build-functions
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
       ((file-directory-p file)
        ;; make a directory in DST and move all SRC files into it
        (unless (file-exists-p (file-name-concat dst file))
          (make-directory (file-name-concat dst file)))
        (pam--copy-files (expand-file-name file)
                         (file-name-concat dst file)))
       (t
        ;; delete a file from DST and copy from SRC
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

(provide 'pam)
;;; pam.el ends here
