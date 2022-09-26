;;; my-run-command.el --- my-run-command

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

;;; Code:
(use-package run-command
    :ensure t
    :custom
    (run-command-completion-method 'helm)
    :bind ((:map xah-fly-command-map)
           ("SPC , c" . 'run-command)
           ("S-<f5>"  . 'my-run-last-command))
    :config

    (defun run-command--run--set-last-recipe (recipe)
      "Set `run-command-last-recipe'."
      (setq-local run-command-last-recipe recipe))

    (advice-add 'run-command--run :before #'run-command--run--set-last-recipe)

    (defun my-run-last-command ()
      "Run command which was runned last, if commands wasn't run do nothing."
      (interactive)
      (if run-command-last-recipe
          (run-command--run run-command-last-recipe)
        (message "NOT FOUND!")))

    (defvar run-command-last-recipe nil
      "Last runned recipe of `run-command'."))

(use-package run-command-recipes
    :ensure t
    :config
    (run-command-recipes-use-all))

(provide 'my-run-command)
;;; my-run-command.el ends here