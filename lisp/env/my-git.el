;;; my-git.el --- My config for the Git: the most popular version control

;; Copyright (C) 2022-2025 semenInRussia

;;; Commentary:

;; My config for the Git: the most popular version control tool.

;;; Code:

(require 'my-leaf)
(require 'dash)


(leaf magit
  :ensure (magit :repo "magit/magit"
                 :host github)
  :bind (:magit-mode-map
         ("D" . magit-file-delete))
  :defvar project-switch-commands
  :custom ((magit-refresh-status-buffer . t)
           (magit-disabled-section-inserters
            . '(magit-insert-diff-filter-header
                magit-insert-tags-header)))
  :init
  (fset 'magit-version #'ignore)
  (advice-add 'transient--post-command
              :around
              (defun my-oref-for-nils (&rest r)
                (ignore-errors
                  (apply r))))

  (with-eval-after-load 'project
    (add-to-list 'project-switch-commands '(magit-project-status "Magit") t)
    (keymap-set project-prefix-map "m" 'magit-project-status)))

(leaf git-timemachine
  :ensure (git-timemachine :repo "pidu/git-timemachine" :host gitlab))

(leaf git-modes
  :ensure t)

(leaf gitignore-templates
  :ensure t)

(provide 'my-git)
;;; my-git.el ends here
