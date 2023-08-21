;;; my-git.el --- My config for the Git: the most popular version control

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
;; along with this program.  If not, see <http://www.gnu.org/licenses/>

;;; Commentary:

;; My config for the Git: the most popular version control.

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
  :custom ((magit-refresh-status-buffer . nil)
           (magit-disabled-section-inserters
            . '(magit-insert-push-branch-header
                magit-insert-tags-header
                magit-insert-unpushed-to-upstream-or-recent
                magit-insert-unpulled-from-upstream)))
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
