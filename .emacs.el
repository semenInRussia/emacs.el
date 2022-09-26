(setq initial-buffer-choice "~/Start.org")

(defun if-Emacs-org-then-org-babel-tangle ()
    "If current open file is Emacs.org, then `org-babel-tangle`."
    (interactive)
    (when (s-equals? (f-filename buffer-file-name) "Emacs.org")
        (org-babel-tangle)))

(add-hook 'after-save-hook 'if-Emacs-org-then-org-babel-tangle)

(defun my-file-base (file)
    "Return base of the FILE: just name of file without extension."
    (car
     (split-string (car (last (split-string file "/"))) "\\.")))

(add-to-list 'load-path "~/.emacs.d/lisp")

(let ((dirs (directory-files-recursively "~/.emacs.d/lisp" ".*" t)))
    (while dirs
        (and
         (file-directory-p (car dirs))
         (add-to-list 'load-path (car dirs)))
        (setq dirs (cdr dirs))))

(defvar my-modules-files
  (directory-files-recursively "~/.emacs.d/lisp" ".el$"))

(defvar my-load-modules-all (length my-modules-files))

(defun my-reload-modules (&rest modules)
    "Unload and then load each of MODULES.

Each of MODULES is either string (indicates loading each of directory),
symbol indicates load only one module or t, indicates load other"
    (my-unload-modules modules)
    (my-load-modules modules))

(defun my-load-modules (&rest modules)
    "Load each of MODULES.

Each of MODULES is either string (indicates loading each of directory),
symbol indicates load only one module or t, indicates load other"
    (my-for-each-config-module 'require modules))

(defun my-unload-modules (&rest modules)
    "UnLoad each of MODULES.

Each of MODULES is either string (indicates loading each of directory),
symbol indicates load only one module or t, indicates load other"
    (my-for-each-config-module
     (lambda (module)
         (and (memq module features) (unload-feature module)))
     modules))

(defun my-for-each-config-module (f modules &optional start)
    "Call F for each of the my Emacs configuration MODULES.

Each of MODULES is either string (indicates loading each of directory),
symbol indicates load only one module or t, indicates load other.

After each call of F, print progress starting from the START."
    (let ((count (or start 0))
	      module)
        (while modules
            (setq module (car modules))
            (setq modules (cdr modules))
            (incf count)
            (cond
              ((and (symbolp module) (not (eq module t)))
               (message "Call `%s' with %s (%s/%s)"
                        f
                        module
                        count
                        my-load-modules-all)
               (funcall f module))
              ((stringp module)
               (incf count
	                 (1-
                      (my-for-each-module-of-config-dir f module
                                                        (1- count)))))
              (t
               (print "AGAIN!")
               (my-for-each-module-of-config-dir f "."))))))

(defun my-for-each-module-of-config-dir (f dir &optional start)
    "Call F for each Emacs Lisp file of the the configuration subdir DIR.

After each call of F, print progress starts with START.

Return number of modules on which was call F."
    (let ((modules
           (mapcar
            (lambda (file) (intern (my-file-base file)))
            (directory-files-recursively
             (concat "~/.emacs.d/lisp/" dir)
	         "\\.el$"))))
        (my-for-each-config-module f modules start)
        (length modules)))

(defun my-load-modules-of-dir (dir)
    "Load each Emacs Lisp file of the DIR."
    (apply
     'my-load-modules
     (mapcar
      (lambda (file) (intern (my-file-base file)))
      (directory-files-recursively dir ".el$"))))

(defun my-load-all-config-modules ()
    "Load each of the my Emacs configuration Elisp files."
    (defvar my-html-local-map (make-sparse-keymap))
    (defvar my-latex-local-map (make-sparse-keymap))
    (my-load-modules
     'my-libs
     'my-lib
     "package-management"
     'my-info
     'my-xah
     'my-fast-exec
     'my-aas
     'my-smartparens
     'my-lisp
     'my-autoformat
     "languages"
     "editing"
     "env"
     "ui"
     t))

(my-load-all-config-modules)
