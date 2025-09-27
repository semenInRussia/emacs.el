;;; my-project.el --- My configration for project.el -*- lexical-binding: t -*-

;; Copyright (C) 2022-2025 semenInRussia

;;; Commentary:

;; My configration for project.el that built-in Emacs by default.

;;; Code:

(require 'my-leaf)
(eval-and-compile
  (require 'my-macros))


(leaf project
  :ensure (project :type built-in)
  :bind (:project-prefix-map
         ;; at start `project-dired' is bound with C-x p D, but
         ;; `dired-jump' (open `dired' in the current directory) is
         ;; bound with C-x C-j, I love the following convention:

         ;; C-x C-f - find a file in the current directory,
         ;; C-x p f - find a file in the current project

         ;; C-x C-b - switch to a opened buffer (or also bookmark or
         ;;   register,   because `consult')
         ;; C-x p b - switch to a project buffer,

         ;; so I also do it with:
         ;; - (j) `project-dired' / `dired-jump'
         ;; - (%) `project-query-replace-regexp' / `vr/query-replace'
         ;; - (s) `consult-ripgrep' / `consult-ripgrep'
         ("j" . project-dired)
         ("s" . consult-ripgrep)
         ("%" . project-query-replace-regexp))
  :defvar project-switch-commands
  :config
  (remove-from-list! project-switch-commands
                     ;; remove `vc-dir' from the commands which will be shown
                     ;; when project switched i prefer `magit', sorry
                     '(project-vc-dir "VC-Dir")
                     ;; remove "Find directory" from these commands
                     '(project-find-dir "Find directory"))
  ;; config for `consult' located inside `my-consult'
  ;; config of `magit' located inside `my-git' leaf `magit'
  )

;; SPC p f => C-x p f, when use `meow'
(with-eval-after-load 'meow-helpers
  (declare-function meow-leader-define-key "meow-helpers")
  (meow-leader-define-key (cons "p" project-prefix-map)))

;; `embark': for file map `project-remember-projects-under'
(with-eval-after-load 'embark
  (keymap-set embark-file-map "p" #'project-remember-projects-under))

(defvar project-vc-extra-root-markers nil)
(add-to-list 'project-vc-extra-root-markers ".dir-locals.el")

(provide 'my-project)
;;; my-project.el ends here
