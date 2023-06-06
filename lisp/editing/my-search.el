;;; my-search.el --- My configuration of the search

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

;; My configuration of the search

;;; Code:
(require 'my-leaf)

(leaf ctrlf
  :ensure t
  :bind (("C-s" . ctrlf-forward-default)
         (:xah-fly-command-map
          :package xah-fly-keys
          ("'" . ctrlf-forward-default))))

(leaf visual-regexp
  :ensure t
  :bind (:xah-fly-command-map
         :package xah-fly-keys
         ("SPC r" . vr/query-replace)))

(leaf deadgrep
  :ensure t
  :defun ((projectile-acquire-root . projectile))
  :custom (deadgrep-project-root-function . #'projectile-acquire-root)
  :bind (:xah-fly-command-map :package xah-fly-keys
                              ("SPC SPC '" . deadgrep)))

(leaf consult-projectile
  :ensure t
  :bind (:xah-fly-command-map
         :package xah-fly-keys
         ("SPC SPC r" . consult-projectile)))

(provide 'my-search)
;;; my-search.el ends here
