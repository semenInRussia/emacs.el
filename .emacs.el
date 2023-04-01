(require 'cl-lib)

(add-to-list 'load-path
             (locate-user-emacs-file "lisp/local-projects"))

(setq custom-file
      (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)
(add-to-list 'load-path "~/.emacs.d/lisp")

(require 'my-autoload)
(setq initial-buffer-choice "~/Start.org")

(defvar my-modules-order
  (list
   "packagement/my-straight.el"
   "packagement/my-leaf.el"
   "package-management"
   "my-libs.el"
   "my-lib.el"
   "ui/my-all-the-icons.el"
   "ui/my-doom-modeline.el"
   "editing/my-xah.el"
   "editing"
   "languages/lisps/my-lisp.el"
   "languages/my-autoformat.el"
   "languages"
   "env"
   "ui")
  "Names of the directories and files that define an order to load.")

(defvar my-modules-files-ignore-regexps
  '("/local-projects/" "/test/" "/features/" ".*-step\\.el" "/site-lisp/")
  "List of the regexps that indicates that a file to load shouldn't be loaded.")

(defun my-file-igored-as-module-p (filename)
  "Return non-nil if a module at FILENAME can't be a configuration module."
  (cl-some
   (lambda (regexp) (string-match-p regexp filename))
   my-modules-files-ignore-regexps))

(setq my-modules-files
      (cl-remove-if
       #'my-file-igored-as-module-p
       (directory-files-recursively "~/.emacs.d/lisp" ".el$" nil)))

(let ((dirs (directory-files-recursively "~/.emacs.d/lisp" ".*" t)))
  (while dirs
    (when (file-directory-p (car dirs))
      (add-to-list 'load-path (car dirs)))
    (setq dirs (cdr dirs))))

(defvar my-load-modules-all (length my-modules-files))

(defun my-require-or-debug-file (filename)
  "Require the module in FILENAME, if catch errors, debug it."
  (my-require-or-debug (intern (file-name-base filename))))

(defun my-require-or-debug (module)
  "Require MODULE, if has any errors, then debug that."
  (unless (featurep module)
    (let ((start-time (current-time)))
      (ignore-errors (require module nil t))
      (message "`%s' module took %ssec"
               module
               (float-time (time-since start-time))))))

(defvar my-config-modules-prefix "~/.emacs.d/lisp/")

(defmacro my-extend (var lst)
  "Extend a variable called VAR with type list with LST.
The same to
\(setq var (append var lst))"
  `(setq ,var (append ,var ,lst)))

(defmacro my-remove-from (var elt)
  "Remove an ELT from the list at VAR.
The same to
\(setq var (remove elt var))"
  `(setq ,var (remove ,elt ,var)))

(defmacro my-mapc (lst elt &rest body)
  "Evaluate a BODY with variable called ELT setted to element of a LST."
  `(cl-do
       ((lst (cdr ,lst) (cdr lst))
        (,elt
         (car ,lst)
         (car lst)))
       ((null ,elt))
     ,@body))

(defun my-load-all-config-modules ()
  "Load all configuration modules."
  (cl-do*
      ((order nil)
       (sketch-order (cdr my-modules-order) (cdr sketch-order))
       (it
        (concat my-config-modules-prefix (car sketch-order))
        (and
         sketch-order
         (concat my-config-modules-prefix (car sketch-order)))))
      ((null it))
    (if (file-directory-p it)
        (my-mapc my-modules-files file
                 (and
                  (string-prefix-p it file)
                  (not (string-equal it file))
                  (my-remove-from my-modules-files it)
                  (my-require-or-debug-file file)))
      (my-require-or-debug-file it)
      (my-remove-from my-modules-files it)))
  (my-mapc my-modules-files file (my-require-or-debug-file file)))

(my-load-all-config-modules)
(my-load-all-config-modules)

(defgroup my nil "Group for all my config files." :group 'tools)

(defun my-bench ()
  "Show bench analysis."
  (interactive)
  (require 'dash)
  (require 's)
  (require 'inspector)
  (switch-to-buffer "*Messages*")
  (->>
   (buffer-substring-no-properties (point-min) (point-max))
   (s-lines)
   (--keep
    (-when-let
        ((_ module duration)
         (s-match "‘\\(.*?\\)’ module took \\(.*?\\)sec" it))             ;nofmt
      (cons module (string-to-number duration))))
   (--sort (> (cdr it) (cdr other)))
   (inspector-inspect)))

(defun my-do-autoload-for-local-projects-files ()
  "If the opened file is a \"local projects\", make the directory autoloads."
  (interactive)
  (when (string-prefix-p
         (f-full "~/.emacs.d/lisp/local-projects/")
         (f-full (buffer-file-name)))
    (make-directory-autoloads "~/.emacs.d/lisp/local-projects/"
                              "~/.emacs.d/lisp/local-projects/my-autoload.el")))

(add-hook 'after-save-hook 'my-do-autoload-for-local-projects-files)
