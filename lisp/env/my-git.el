;;; my-git.el --- My config for the Git: the most popular version control

;; Copyright (C) 2022-2024 Semen Khramtsov

;;; Commentary:

;; My config for the Git: the most popular version control tool.

;;; Code:

(require 'my-leaf)
(require 'dash)


(leaf magit-section
  :ensure t)

(leaf with-editor
  :ensure t)

(leaf git-commit
  :ensure t)

(leaf magit
  :ensure (magit :repo "magit/magit"
                 :host github)
  :bind (:magit-mode-map
         ("D" . magit-file-delete))
  :custom ((magit-refresh-status-buffer . t)
           (magit-disabled-section-inserters
            . '(magit-insert-diff-filter-header
                magit-insert-tags-header)))
  :init
  (with-eval-after-load 'project
    (add-to-list 'project-switch-commands '(magit-project-status "Magit") t)
    (keymap-set project-prefix-map "m" 'magit-project-status))
  :config
  (add-hook 'magit-mode-hook #'hl-line-mode))

(leaf git-timemachine
  :ensure (git-timemachine :repo "pidu/git-timemachine" :host gitlab)
  :fast-exec ("Git Timemachine" 'git-timemachine))

(leaf git-modes
  :ensure (git-modes :repo "magit/git-modes" :host github))

(leaf gitignore-templates
  :ensure t
  :fast-exec ("Insert Git Ignore" 'gitignore-templates-insert))

(leaf github-clone
  :ensure (github-clone :repo "dgtized/github-clone.el" :host github)
  :custom (github-clone-directory . "~/projects")
  :fast-exec ("Clone a GitHub Project" 'github-clone))

(leaf line-reminder
  :ensure (line-reminder :repo "emacs-vs/line-reminder" :host github)
  :commands line-reminder-mode
  :custom ((line-reminder-bitmap . 'filled-rectangle)
           (line-reminder-show-option . 'indicators)))

(provide 'my-git)
;;; my-git.el ends here
