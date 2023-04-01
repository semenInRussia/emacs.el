;;; my-fast-exec.el --- My configuration of the `fast-exec'

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

;; My configuration of the `fast-exec'

;;; Code:
(leaf fast-exec
  :load-path "~/projects/fast-exec.el/"
  :defun fast-exec-use
  :bind (:xah-fly-command-map           ;nofmt
         :package xah-fly-keys
         ("=" . fast-exec-exec))
  :config

  (require 'fast-exec-initial-keymaps)

  (fast-exec-use
   deadgrep
   devdocs
   flycheck
   format-all
   magit
   package
   projectile
   skeletor
   suggest
   wikinforg
   yasnippet
   haskell-mode
   helm-wikipedia)
  (leaf straight :fast-exec ("Use Package" 'straight-use-package))
  (leaf my-mipt
    :fast-exec (("Next MIPT Task" 'my-mipt-next-task)
                ("Previous MIPT Task" 'my-mipt-prev-task)
                ("Open Last MIPT Task" 'my-mipt-visit-last-task)
                ("Find MIPT Task" 'my-mipt-task-visit)
                ("Open MIPT Task in Web Browser" 'my-mipt-task-browse-course-url))
    :bind (:my-latex-local-map
           :package tex
           ("c" . my-copy-buffer-content-as-mipt-solution))))

(provide 'my-fast-exec)
;;; my-fast-exec.el ends here
