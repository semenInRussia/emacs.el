;;; my-build-config.el --- Join all my config files into one init.el -*- lexical-binding: t; -*-

;; Copyright (C) 2023 semenInRussia

;; Author: semenInRussia <hrams205@gmail.com>

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Join all my config files into one init.el.  I use this script to make one
;; big init.el file which has more fast startup time.

;; `build-config' command joins all modules files from the .emacs.d/lisp into one
;; dist/my-modules.el and compile it.  When I start Emacs init.el files just
;; require my-modules.el, my-autoloads.el and custom.el which have already
;; byte-compiled (or even better NATIVE-compiled).  One big file instead of a lot
;; of small is better in load time, because every `require' statement also take
;; a bit of time.

;;; Code:

;; NOTE that here I don't use external libraries like `dash' or `s', because I
;; sometimes need to build configuration when old configuration was broken and
;; `dash'/`s' haven't been loaded
(require 'cl-lib)

(defvar my-modules-el-file (locate-user-emacs-file "dist/my-modules.el"))
(defvar my-config-modules-prefix (locate-user-emacs-file "lisp/"))


(defvar my-modules-order
  (list
   "package-management/my-straight.el"
   "package-management/my-leaf.el"
   "package-management"
   "my-libs.el"
   "my-lib.el"
   "editing"
   "languages/lisps/my-lisp.el"
   "languages"
   "env"
   "misc"
   "ui")
  "Names of the directories and files that define an order to load.")

(defvar my-modules-files-ignore-regexps
  '("/local-projects/" "/test/" "/features/" ".*-step\\.el" "/site-lisp/" "/sandbox/")
  "List of the regexps that indicates that a file to load shouldn't be loaded.")

;;;###autoload
(defun my-build-config ()
  "Build my config."
  (interactive)
  (let ((default-directory (file-name-directory my-modules-el-file)))
    (my-join-modules-into-modules.el)
    (byte-compile-file my-modules-el-file)
    (native-compile-async (list my-modules-el-file))
    (byte-compile-file (locate-user-emacs-file "lisp/local-projects/my-autoload.el"))))

(defun my-file-igored-as-module-p (filename)
  "Return non-nil if a module at FILENAME can't be a configuration module."
  (let ((base (file-name-base filename)))
    (or
     ;; ignore auto-save or backup files
     (string-prefix-p "#" base)
     (string-prefix-p "." base)
     ;; ignore not Elisp files
     (not (string-suffix-p ".el" filename))
     ;; ignore files that matches with one of ignoring regexps
     (cl-some
      (lambda (regexp) (string-match-p regexp filename))
      my-modules-files-ignore-regexps))))

(defmacro my-remove-from (var elt)
  "Remove an ELT from the list at VAR.
The same to
\(setq var (remove elt var))"
  `(setq ,var (remove ,elt ,var)))

(defun my-all-modules-files ()
  "Return list of all modules filenames using `my-modules-order'."
  (let ((order (mapcar
                (lambda (it) (concat my-config-modules-prefix it))
                my-modules-order))
        order-item
        (files (cl-remove-if
                #'my-file-igored-as-module-p
                (directory-files-recursively (locate-user-emacs-file "lisp") "my-.*\\.el$" nil)))
        f
        sorted)
    (while order
      (setq order-item (car order))
      (setq order (cdr order))
      (cond
       ((file-directory-p order-item)
        (setq sorted
              (append sorted (cl-remove-if
                              (lambda (f)
                                (message "= %s" f)
                                (or (my-file-igored-as-module-p f)
                                    (member f sorted)))
                              (directory-files-recursively order-item ".el$")))))
       (t
        (message "+ %s" order-item)
        (setq sorted (append sorted (list order-item))))))
    (while files
      (setq f (car files))
      (setq files (cdr files))
      (when (and (not (member f sorted)) (not (my-file-igored-as-module-p f)))
        (message "! %s" f)
        (setq sorted (append sorted (list f)))))
    sorted))

(defun my-join-modules-into-modules.el ()
  "Join all configuration modules into one my-modules.el file."
  (my-join-modules my-modules-el-file))

(defun my-join-modules (dest)
  "Join all configuration modules into one file with DEST filename."
  (with-temp-buffer
    (mapc 'insert-file-contents (nreverse (my-all-modules-files)))
    (goto-char (point-max))
    (replace-regexp-in-region "^(provide 'my-[a-zA-Z-]*?)" "\n"
                              (point-min)
                              (point-max))
    (replace-regexp-in-region "^(require 'my-[a-zA-Z-]*?)" "\n"
                              (point-min)
                              (point-max))
    (insert "\n(provide 'my-modules)")
    (delete-file dest)
    (write-region (point-min) (point-max) dest)))

(provide 'my-build-config)
;;; my-build-config.el ends here
