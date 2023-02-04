;;; my-run-command.el --- My configuration for `run-command'

;; Copyright (C) 2022 Semen Khramtsov

;; Author: Semen Khramtsov <hrams205@gmail.com>
;; Version: 0.1
;; URL: https://github.com/semenInRussia/emacs.el

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; My configuration for `run-command'

;;; Code:

(leaf run-command
  :ensure (run-command
           :type git
           :host github
           :repo "bard/emacs-run-command"
           :branch "develop")
  :after helm
  :defun ((run-command--run--set-last-recipe . my-run-command)
          run-command--run)
  :custom ((run-command-completion-method . 'helm)
           (run-command-default-runner . #'run-command-runner-compile))
  :bind (:xah-fly-command-map
         :package xah-fly-keys
         ("SPC , c" . run-command)
         ("S-<f5>"  . my-run-last-command))
  :config                               ;nofmt
  (leaf run-command-recipes
    :load-path "~/projects/emacs-run-command-recipes"
    :require t
    :defun run-command-recipes-use-all
    :config (run-command-recipes-use-all))

  (defvar run-command-last-recipe nil
    "Last runned recipe of `run-command'.")

  (defun run-command--run--set-last-recipe (recipe)
    "Set `run-command-last-recipe'."
    (setq-local run-command-last-recipe recipe))

  (advice-add 'run-command--run :before #'run-command--run--set-last-recipe)

  (defun my-run-last-command ()
    "Run command which was runned last, if commands wasn't run do nothing."
    (interactive)
    (if run-command-last-recipe
        (run-command--run run-command-last-recipe)
      (message "NOT FOUND!"))))

(provide 'my-run-command)
;;; my-run-command.el ends here
