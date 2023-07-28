;;; init.el --- Initialize Emacs Lisp code for my Emacs -*- lexical-binding: t; -*-

;; Copyright (C) 2023

;; Author: semenInRussia <hrams205@gmail.com>

;;; Commentary:

;; Initialize Elisp code for my Emacs.

;;; Code:

(require 'cl-lib)
(require 'subr-x)  ;; for `string-remove-prefix'

;; every custom variable of my config have the following group
(defgroup my nil "Group for all my config files." :group 'tools)

;; add some files into the `load-path' that config files can require theme and
;; byte-compiler will be happy
(add-to-list 'load-path "~/.emacs.d/lisp/package-management/")
(add-to-list 'load-path "~/.emacs.d/lisp/")

;;; Local Projects
;; It is my own small "packages" which aren't so big to create real packages
(add-to-list 'load-path (locate-user-emacs-file "lisp/local-projects"))
(load "~/.emacs.d/lisp/local-projects/my-autoload")

;;; add to `load-path' all installed packages
;;
;; I'm use `pam' which is built over straight.
;;
;; if packages was already installed, then every package was loaded from the
;; `pam' directory, it's more faster than load packages from package specific directories
;; because in 2nd case `load-path' will contain about 200+(*) directories and to load one package
;; with `require' Emacs will checks all these dirs.  When all packages files located inside the `pam' dir
;; Emacs checks only `pam' dir (instead of 200 other dirs)
;;
;;
;; NOTE: amount of the directories in the `load-path' depends on amount of the
;;   packages and their dependencies
(defvar my-straight-packages-already-installed-p t)
(require 'pam)
(pam-activate)

;; don't use .emacs.d for custom.el which I don't use
;;
;; in the most of configurations, after it Emacs load custom.el, but I fount it
;; a bit useless.  I prefer `setq' over `custom'
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

;;; Benchmarking

(defun my-time-subtract-millis (b a)
  "Subtract two time structutres: A and B and return milliseconds."
  (* 1000.0 (float-time (time-subtract b a))))

(defvar my-require-times nil
  "A list of (FEATURE LOAD-START-TIME LOAD-DURATION).
LOAD-DURATION is the time taken in milliseconds to load FEATURE.")

(defun my-require-times-wrapper (orig feature &rest args)
  "Note in `my-require-times' the time taken to require each feature.

Pass FEATURE with ARGS to `require'.  ORIG is the original `require' function"
  (let* ((already-loaded (memq feature features))
         (require-start-time (and (not already-loaded) (current-time))))
    (prog1
        (apply orig feature args)
      (when (and (not already-loaded) (memq feature features))
        (let ((time (my-time-subtract-millis (current-time) require-start-time)))
          (add-to-list 'my-require-times
                       (list feature require-start-time time)
                       t))))))

(advice-add 'require :around 'my-require-times-wrapper)

(define-derived-mode my-require-times-mode tabulated-list-mode "Require-Times"
  "Show times taken to `require' packages."
  (setq tabulated-list-format
        [("Start time (ms)" 20 my-require-times-sort-by-start-time-pred)
         ("Feature" 30 t)
         ("Time (ms)" 12 my-require-times-sort-by-load-time-pred)])
  (setq tabulated-list-sort-key (cons "Start time (ms)" nil))
  (setq tabulated-list-padding 2)
  (setq tabulated-list-entries #'my-require-times-tabulated-list-entries)
  (tabulated-list-init-header)
  (when (fboundp 'tablist-minor-mode)
    (tablist-minor-mode)))

(defun my-require-times-sort-by-start-time-pred (entry1 entry2)
  (< (string-to-number (elt (nth 1 entry1) 0))
     (string-to-number (elt (nth 1 entry2) 0))))

(defun my-require-times-sort-by-load-time-pred (entry1 entry2)
  (> (string-to-number (elt (nth 1 entry1) 2))
     (string-to-number (elt (nth 1 entry2) 2))))

(defun my-require-times-tabulated-list-entries ()
  (cl-loop for (feature start-time millis) in my-require-times
           with order = 0
           do (cl-incf order)
           collect (list order
                         (vector
                          (format "%.3f" (my-time-subtract-millis start-time before-init-time))
                          (symbol-name feature)
                          (format "%.3f" millis)))))

(defun my-require-times ()
  "Show a tabular view of how long various libraries took to load."
  (interactive)
  (with-current-buffer (get-buffer-create "*Require Times*")
    (my-require-times-mode)
    (tabulated-list-revert)
    (display-buffer (current-buffer))))

;;; Load all config files

;; the most part of the config located inside "~/.emacs.d/lisp" I join all .el
;; files into the my-modules file for fast start up
(defvar my-modules-el-file "~/.emacs.d/dist/my-modules.el")

(unless (file-exists-p (file-name-directory my-modules-el-file))
  (user-error "File \"my-modules.el\" didn't created, suggest use --modules option"))

(add-to-list 'load-path (file-name-directory my-modules-el-file))

(let ((file-name-handler-alist nil))
  (require 'my-modules))

(provide 'init)
;;; init.el ends here
