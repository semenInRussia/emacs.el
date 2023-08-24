;;; my-project.el --- My configration for project.el

;; Copyright (C) 2022-2023 Semen Khramtsov

;;; Commentary:

;; My configration for project.el that built-in Emacs by default.

;;; Code:

(require 'my-leaf)


(leaf project
  :bind-keymap ("C-c p" . project-prefix-map)
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
