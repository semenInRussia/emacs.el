;;; my-pam.el --- My configuration of pam -*- lexical-binding: t; -*-

;; Copyright (C) 2024 semenInRussia

;; Author: semenInRussia <hrams205@gmail.com>
;; Version: 0.1
;; Homepage: https://github.com/semeninrussia/emacs.el

;;; Commentary:

;; My configuration of `pam': my own small package manager built over
;; `straight'.

;;; Code:

(require 'pam)

;; Some packages can depended on the packages which already installed
;; into Emacs.  (see `consult-eglot')
;;
;; When `straight' and `pam' install this packages it can install
;; `eglot' second time.  In this file I fix this problem

(eval-and-compile
  (defvar my-built-in-packages '(eldoc
                                 eglot
                                 external-completion
                                 flymake
                                 imenu
                                 jsonrpc
                                 org
                                 project
                                 seq
                                 ;; transient  ; `magit' require not built-in version
                                 xref)
    "List of packages which are already built-in Emacs.")

  (defun my-pam-mark-built-ins ()
    "My mark all built-in packages as built-in for `straight' and `pam'."
    (let ((xs my-built-in-packages))
      (while xs
        (message "Mark the package %s" (car xs))
        (pam-use-package `(,(car xs)
                           :type built-in))
        (setq xs (cdr xs)))))

  (with-eval-after-load 'pam
    (advice-add 'pam--load-straight
                :around
                (defun my-pam-maybe-mark-built-ins (&rest args)
                  (let ((need-p (not pam-straight-already-loaded-p)))
                    (apply args)
                    (and need-p
                         (my-pam-mark-built-ins)))))))

;;; my-pam.el ends here
(provide 'my-pam)
