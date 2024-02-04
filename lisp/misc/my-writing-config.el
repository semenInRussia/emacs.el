;;; my-writing-config.el --- My configuration for the writing other configuration -*- lexical-binding: t; -*-

;; Copyright (C) 2022 semenInRussia

;; Author: semenInRussia <hrams205@gmail.com>
;; Version: 0.1
;; Homepage: https://github.com/semeninrussia/emacs.el

;;; Commentary:

;; My configuration for the writing other configuration.

;;; Code:

(require 'my-leaf)

(require 'dash)
(require 'f)
(require 's)

(declare-function inspector-inspect "inspector.el")

(defun my-new-config-module (module-name &optional directory)
  "Create a new configuration file named MODULE-NAME in the DIRECTORY.

DIRECTORY defaults to ~/.emacs.d/lisp/"
  (interactive "sName of the configuration module: \nDDirectory: ")
  (setq directory (or directory user-emacs-directory))
  (->>
   module-name
   (s-append ".el")
   (s-prepend "my-")
   (s-prepend directory)
   (find-file))
  (insert
   (s-replace
    "writing-config"
    module-name
    (format
     ";;; my-writing-config.el --- My configuration of writing-config -*- lexical-binding: t; -*-

;; Copyright (C) %s semenInRussia

;; Author: semenInRussia <hrams205@gmail.com>
;; Version: 0.1
;; Homepage: https://github.com/semeninrussia/emacs.el

;;; Commentary:

;; My configuration of writing-config.

;;; Code:

(require 'leaf)


(leaf writing-config)

(provide 'my-writing-config)
;;; my-writing-config.el ends here"
     (format-time-string "%Y"))))
  (search-backward "(leaf "))

(with-eval-after-load 'fast-exec
  (eval
   '(fast-exec-bind 'writing-config
      (fast-exec-make-some-commands
       ("New Config Module" 'my-new-config-module)))))

(leaf ecukes
  :ensure t
  :bind (:feature-mode-map
         :package feature-mode
         ("C-c C-e" . ecukes))
  :hook (ecukes-mode-hook . my-feature-mode-hook)
  :config (leaf espuds :ensure t :require t))

(leaf leaf
  :bind ("C-x M-f" . 'leaf-find))

(defun my-bench ()
  "Show bench analysis for Emacs startup."
  (interactive)
  (switch-to-buffer "*Messages*")
  (->>
   (buffer-substring-no-properties (point-min) (point-max))
   (s-lines)
   (--keep
    (-when-let
        ((_ module duration)
         (s-match "‘\\(.*?\\)’ module took \\(.*?\\)sec" it))
      (cons module (string-to-number duration))))
   (--sort (> (cdr it) (cdr other)))
   (inspector-inspect))
  (rename-buffer "*My Bench*"))

(defvar my-local-project-was-updated nil
  "Variable is non-nil if one of my \"local-project\" was edited.

If before exit Emacs this variable is non-nil byte-compile
local-projects autoloads.

Will be changed automatically if you use
`my-do-autoload-for-local-projects-files'")

(defun my-do-autoload-for-local-projects-files ()
  "If the opened file is a \"local projects\", make the directory autoloads."
  (interactive)
  (let ((dir (f-full (locate-user-emacs-file "lisp/local-projects/")))
        (out (f-full (locate-user-emacs-file "lisp/local-projects/my-autoload.el")))
        flycheck-files)
    (when (string-prefix-p dir (f-full (buffer-file-name)))
      (setq my-local-project-was-updated t)
      (->>
       dir
       (f-files)
       (--filter (or
                  (s-prefix-p "flycheck_" (f-base it))
                  (s-suffix-p ".elc" it)))
       (setq flycheck-files))
      (loaddefs-generate dir
                         out
                         (cons out flycheck-files)))))

(defun my-byte-compile-local-projects-autoloads (&optional filename)
  "Byte-compile autoloads Elisp file with FILENAME."
  (or filename
      (setq filename (f-full (locate-user-emacs-file "lisp/local-projects/my-autoload.el"))))
  (setq byte-compile-last-logged-file nil)
  (let ((byte-compile-current-file filename)
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

(add-hook 'after-save-hook 'my-do-autoload-for-local-projects-files)
(add-hook 'kill-emacs-hook
          (defun my-maybe-byte-compile-local-projects-autoloads ()
            (when my-local-project-was-updated
              (my-byte-compile-local-projects-autoloads))))


(eval-when-compile
  (require 'dash)
  (require 's)
  (require 'f))

(defun my-copy-files (files dest)
  "Copy all FILES into the directory DEST."
  (--each files
    (if (f-dir-p it)
        (ignore-errors
          (copy-directory it
                          (f-join dest (f-filename it))))
      (copy-file it
                 (f-join dest (f-filename it))
                 'ok-if-exists))))

(provide 'my-writing-config)
;;; my-writing-config.el ends here
