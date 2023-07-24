;;; my-fast-exec-misc.el --- Define `fast-exec' keybindings for miscellaneous packages -*- lexical-binding: t; -*-

;; Copyright (C) 2023 $6

;; Author: $6 <hrams205@gmail.com>
;; Version: 0.0.1
;; Package-Requires: ((dash "2.18.0") (s "1.12.0"))
;; Homepage: https://github.com/semenInRussia/emacs.el

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Define `fast-exec' keybindings for miscellaneous packages.

;;; Code:

(require 'my-leaf)
(require 'fast-exec-initial-keymaps)

(fast-exec-use deadgrep
               devdocs
               flycheck
               magit
               package
               skeletor
               suggest
               wikinforg
               yasnippet
               haskell-mode)

(leaf straight
  :fast-exec ("Use Package" 'straight-use-package))

(leaf my-mipt
  :fast-exec (("Next MIPT Task" 'my-mipt-next-task)
              ("Previous MIPT Task" 'my-mipt-prev-task)
              ("Open Last MIPT Task" 'my-mipt-visit-last-task)
              ("Find MIPT Task" 'my-mipt-task-visit)
              ("Open MIPT Task in Web Browser" 'my-mipt-task-browse-course-url))
  :bind (:latex-mode-map
         :package tex-mode
         ("C-c M-w" . 'my-copy-buffer-content-as-mipt-solution)))

(provide 'my-fast-exec-misc)
;;; my-fast-exec-misc.el ends here
