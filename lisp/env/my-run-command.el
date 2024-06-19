;;; my-run-command.el --- My configuration for `run-command'
;; Copyright (C) 2022-2024 semenInRussia

;;; Commentary:
;; My configuration for `run-command'

;;; Code:
(require 'my-leaf)
(declare-function my-run-command--run--set-last-recipe "my-run-command-funcs.el")

(leaf run-command
  :ensure (run-command
           :host github
           :repo "bard/emacs-run-command"
           :branch "develop")
  :defun run-command-runner-compile run-command-core-run
  :custom (run-command-default-runner . #'run-command-runner-compile)
  :bind (("<f5>" . run-command)
         ("S-<f5>" .  my-run-last-command)
         ("M-!" . compile))
  :config
  (advice-add 'run-command-core-run :before #'my-run-command--run--set-last-recipe)

  (leaf run-command-recipes
    :ensure t
    :require t
    :commands run-command-recipes-use-all
    :config (run-command-recipes-use-all)))

(leaf compile
  :bind (:compilation-mode-map
         ("t" . #'my-compilation-toggle-hide-details)))

(provide 'my-run-command)
;;; my-run-command.el ends here
