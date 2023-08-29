;;; my-project.el --- My configration for project.el

;; Copyright (C) 2022-2023 Semen Khramtsov

;;; Commentary:

;; My configration for project.el that built-in Emacs by default.

;;; Code:

(require 'my-leaf)
(eval-and-compile
  (require 'my-macros))


(leaf project
  :hook (after-init-hook . my-bind--project)
  :bind (:project-prefix-map
         ;; at start `project-dired' is bound with C-x p D, but `dired-jump'
         ;; (open `dired' in the current directory) is bound with C-x C-j, I
         ;; love the following convention:

         ;; C-x C-f - find a file in the current directory,
         ;; C-x p f - find a file in the current project

         ;; C-x C-b - switch to a opened buffer (or also bookmark or register,
         ;; because `consult')
         ;; C-x p b - switch to a project buffer,

         ;; so I also do it with:
         ;; - (j) `project-dired' / `dired-jump'
         ;; - (%) `project-query-replace-regexp' / `vr/query-replace'
         ;; - (s) `consult-ripgrep' / `consult-ripgrep'
         ("j" . project-dired)
         ("s" . consult-ripgrep)
         ("%" . project-query-replace-regexp))
  :config (defun my-bind--project ()
            (global-set-key (kbd "C-c p") project-prefix-map))
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

(provide 'my-project)
;;; my-project.el ends here
