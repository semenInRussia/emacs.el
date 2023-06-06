;;; my-projectile.el --- My configration of `projectile'

;; Copyright (C) 2022-2023 Semen Khramtsov

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

;; My configration of `projectile'

;;; Code:

(require 'my-leaf)
(require 'fast-exec)

(require 'dash)
(require 'f)
(require 's)

(declare-function projectile-project-files-clear-cache "my-project.el")

(leaf projectile
  :ensure t
  :defun (projectile-acquire-root
          projectile-project-p
          projectile-current-project-files
          projectile-project-root
          projectile-completing-read
          projectile-project-files
          projectile-maybe-invalidate-cache)
  :defvar (projectile-known-projects projectile-project-root)
  :custom ((projectile-project-root-functions .
                                              '(projectile-root-local
                                                my-project-root))
           (projectile-enable-caching . nil)
           (projectile-auto-discover . nil))
  :fast-exec ("Projectile Clear Cache" 'projectile-project-files-clear-cache)
  :global-minor-mode projectile-mode
  :config
  (leaf consult-projectile
    :bind (:xah-fly-command-map
           :package xah-fly-keys
           ("SPC j"  . consult-projectile-find-file))))

(advice-add
 'projectile-project-files
 :override
 'my-projectile-project-files)
(advice-add
 'projectile-project-root
 :override
 'my-project-root)
(advice-add
 'projectile-root-local
 :override
 'my-projectile-root-local)
(advice-add
 'projectile-files-with-string
 :override
 'my-projectile-files-with-string)

(provide 'my-projectile)
;;; my-projectile.el ends here
