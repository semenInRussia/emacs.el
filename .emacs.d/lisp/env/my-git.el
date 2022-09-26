;;; my-git.el --- My config of `git'

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
(use-package git-gutter
    :ensure t
    :hook
    (prog-mode . git-gutter-mode))

(use-package magit :ensure t)

(use-package blamer
    :ensure t
    :defer 20
    :custom
    (blamer-idle-time 0.3)
    (blamer-min-offset 70)
    :custom-face
    (blamer-face ((t :foreground "#7a88cf"
                     :background nil
                     :height 140
                     :italic t))))

(use-package git-undo
    :commands git-undo
    :init
    (defun fast-exec-define-git-undo-keymaps ()
      "Bind `git-undo' and `fast-exec'."
      (fast-exec/some-commands
       ("Undo via Git" 'git-undo)))
    (fast-exec/register-keymap-func 'fast-exec-define-git-undo-keymaps)
    (fast-exec/reload-functions-chain))

(use-package git-modes
    :ensure t)

(use-package helm-gitignore
    :load-path "site-lisp"
    :init
    (defun fast-exec-helm-gitignore-keys ()
      "Bind of `helm-gitignore' and `fast-exec'."
      (fast-exec/some-commands
       ("Generate Gitignore" 'helm-gitignore)))
    (fast-exec/register-keymap-func 'fast-exec-helm-gitignore-keys)
    (fast-exec/reload-functions-chain))

(provide 'my-git)
;;; my-git.el ends here
