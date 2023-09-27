;;; test-pam.el --- Tests of `pam': my PAckage Manager which is built over `straight' -*- lexical-binding: t -*-

;;; Commentary:

;; Tests of `pam': my PAckage Manager which is built over `straight'.

;;; Code:

(require 'pam)
(eval-and-compile
  (require 'ert))

;;; Initialize the tests (pam needs in a test directory):

(defvar test-pam-path
  (file-name-concat
   (directory-file-name
    (file-name-directory (or load-file-name (buffer-file-name))))
   "sandbox")
  "Path to tests directory.")

(defmacro pam--with-sandbox (&rest body)
  "Evaluate the BODY inside a sandbox directory for `pam'.

NOTE that it changes the `default-directory', you can don't use full paths."
  (declare (indent 0))
  `(progn
     (let ((default-directory test-pam-path)
           ;; setup `pam'
           (pam-build-dir (file-name-concat test-pam-path "pam"))
           (pam-autoloads-filename "my-packages-autoloads.el")
           ;; setup `straight'
           (straight-build-cache-fixed-name (make-hash-table :test 'equal))
           (straight--recipe-cache (make-hash-table :test 'equal))
           (straight--repo-cache (make-hash-table :test 'equal))
           (straight--profile-cache (make-hash-table :test 'equal))
           (straight--recipe-lookup-cache (make-hash-table :test 'equal))
           (straight--profile-cache-valid (make-hash-table :test 'equal))
           (straight-cache-autoloads (make-hash-table :test 'equal))
           (straight--cached-package-modifications (make-hash-table :test 'equal))
           (straight--success-cache (make-hash-table :test 'equal))
           (straight-base-dir test-pam-path)
           (straight--build-cache (make-hash-table :test 'equal))
           (straight--autoloads-cache (make-hash-table :test 'equal))
           (straight-recipe-repositories '(melpa)))
       (and
        (file-exists-p pam-build-dir)
        (delete-directory pam-build-dir t))
       (and
        (file-exists-p (straight--build-dir))
        (delete-directory (straight--build-dir) t))
       (unless (file-exists-p test-pam-path)
         (make-directory test-pam-path))
       (pam-create-files)
       ,@body)))

;;; Tests

(ert-deftest test-pam-use-package ()
  "Test `pam-use-package'."
  (pam--with-sandbox
    (pam-install-everything-mode 0)
    (should
     ;; don't install anything when `pam-install-everything-mode' is disabled.
     ;; errors not expected
     (pam-use-package 'abracadabrak))
    (pam-install-everything-mode t)
    (should-error
     ;; when `pam-install-everything-mode' is enabled, install unknown packages
     ;; should raise errors
     (should-not
      ;; and return nil values
      (pam-use-package 'abracadabrak)))
    ;; install the most popular package on MELPA to ensure that packages are
    ;; installed right if `pam-install-everything-mode' is enabled
    (pam-use-package 'dash)
    (should (file-exists-p "pam/dash.el"))
    (should (file-exists-p "pam/dash.elc"))
    (should (file-exists-p "pam/dash.texi"))
    (should (file-exists-p "pam/my-packages-autoloads.el"))))

(ert-deftest test-pam-delete-package ()
  "Test `pam-delete-package'."
  (pam--with-sandbox
    ;; install a package
    (pam-install-everything-mode t)
    (pam-use-package 'dash)
    ;; ensure: it was installed
    (should (file-exists-p "pam/dash.el"))
    (should (file-exists-p "pam/dash.elc"))
    (should (file-exists-p "pam/dash.texi"))
    (should (file-exists-p "pam/my-packages-autoloads.el"))
    ;; delete
    (pam-delete-package "dash")
    ;; ensure: it was deleted
    (should-not (file-exists-p "pam/dash.el"))
    (should-not (file-exists-p "pam/dash.elc"))
    (should-not (file-exists-p "pam/dash.texi"))))

(ert-deftest test-pam-sync-with-straight ()
  "Test `pam-sync-with-straight'."
  (pam--with-sandbox
    (straight-pull-recipe-repositories
     '(org-elpa
       melpa
       gnu-elpa-mirror
       nongnu-elpa
       el-get
       emacsmirror-mirror))
    ;; install packages with `straight'
    (straight-use-package 'dash)
    (straight-use-package 's)
    ;; ensure: packages haven't installed in `pam' yet
    (should-not (file-exists-p "pam/dash.el"))
    (should-not (file-exists-p "pam/s.el"))
    ;; do sync
    (pam-sync-with-straight)
    ;; check
    (should (file-exists-p "pam/dash.el"))
    (should (file-exists-p "pam/s.el"))))

(ert-deftest test-pam-update-all-packages-autoloads ()
  "Test `pam-update-all-packages-autoloads'."
  (pam--with-sandbox
    ;; install 2 packages
    (pam-use-package 'dash)
    (pam-use-package 's)
    ;; ensure: they have autoloads
    (should (file-exists-p "pam/dash-autoloads.el"))
    (should (file-exists-p "pam/s-autoloads.el"))
    ;; and ensure: pam/my-package-autoloads.el isn't empty
    (with-temp-buffer
      (insert-file-contents "pam/my-packages-autoloads.el")
      (should-not (equal (point-max) 0)))
    ;; remove them, but without update autoloads
    (pam-delete-package "s")
    (pam-delete-package "dash")
    ;; update-autoloads
    (pam-update-all-packages-autoloads)
    ;; and ensure: pam/my-package-autoloads.el is not exists, because there no autoloads
    (should-not (file-exists-p "pam/my-package-autoloads.el"))))

(ert-deftest test-pam-rebuild-package ()
  "Test `pam-rebuild-package'."
  (pam--with-sandbox
    ;; install `dash'
    (pam-use-package 'dash)
    ;; ensure: `dash' have byte-compiled file
    (should (file-exists-p "pam/dash.elc"))
    ;; delete it
    (delete-file "pam/dash.elc")
    ;; rebuild `dash'
    (pam-rebuild-package "dash")
    ;; ensure: byte-compiled file come back
    (should (file-exists-p "pam/dash.elc"))))

;;; test-pam.el ends here
