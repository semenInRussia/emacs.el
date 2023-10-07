;;; my-run-command.el --- My configuration for `run-command'

;; Copyright (C) 2022-2023 Semen Khramtsov

;;; Commentary:

;; My configuration for `run-command'

;;; Code:

(require 'my-leaf)


(leaf run-command
  :ensure (run-command
           :host github
           :repo "bard/emacs-run-command"
           :branch "develop")
  :defun (run-command-runner-compile
          run-command-core-run)
  :custom (run-command-default-runner . #'run-command-runner-compile)
  :bind ("<f5>" . run-command)
  :config
  (advice-add 'run-command-core-run :before #'my-run-command--run--set-last-recipe)

  (leaf run-command-recipes
    :ensure t
    :require t
    :commands run-command-recipes-use-all
    :config (run-command-recipes-use-all)))

(global-set-key (kbd "S-<f5>")  #'my-run-last-command)
(global-set-key (kbd "M-!")     #'compile)

(defvar run-command-last-recipe nil
  "Last runned recipe of `run-command'.")

(defun my-run-command--run--set-last-recipe (recipe)
  "Set `run-command-last-recipe' to a given RECIPE."
  (setq-local run-command-last-recipe recipe))

(defun my-run-last-command ()
  "Run command which was runned last, if commands wasn't run do nothing."
  (interactive)
  (if run-command-last-recipe
      (run-command-core-run run-command-last-recipe)
    (message "NOT FOUND!")))

;;; hide details inside `compile' buffer

(with-eval-after-load 'compile
  (defvar compilation-mode-map)
  (define-key compilation-mode-map "t" #'my-compilation-toggle-hide-details))


(defun my-compilation-toggle-hide-details ()
  "Hide or show details inside of `compile' buffer."
  (interactive)
  (if my-compilation-hide-details-p
      (my-compilation-show-details)
    (my-compilation-hide-details))
  (setq-local my-compilation-hide-details-p (not my-compilation-hide-details-p)))

(defvar-local my-compilation-hide-details-p nil
  "Variable is non-nil if details of `compile' buffer was hiden.")

(defun my-compilation-hide-details ()
  "Hide details like time at compile start inside `compile' buffer."
  (interactive)
  (narrow-to-region (progn
                      (goto-char (point-min))
                      (forward-line 4)
                      (point))
                    (progn
                      (goto-char (point-max))
                      (forward-line -2)
                      (point)))
  (goto-char (point-min)))

(defalias 'my-compilation-show-details #'widen
  "Show details like compile command start inside `compile' buffer.")

(provide 'my-run-command)
;;; my-run-command.el ends here
