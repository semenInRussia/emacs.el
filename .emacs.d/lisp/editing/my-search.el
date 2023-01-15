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
(leaf helm-swoop
  :ensure t
  :after helm
  :bind ((:xah-fly-command-map
          :package xah-fly-keys
          ("'"       . helm-swoop)
          ("SPC k '" . helm-multi-swoop-current-mode))
         (:helm-swoop-map              ;nofmt
          ("M-j"     . helm-swoop-edit))
         (:helm-swoop-edit-map         ;nofmt
          ([remap save-buffer] . helm-swoop--edit-complete))))

(leaf deadgrep
  :ensure t
  :bind (:xah-fly-command-map :package xah-fly-keys ("SPC '" . deadgrep)))

(leaf projectile
  :bind (:xah-fly-command-map
         :package xah-fly-keys
         ("SPC SPC r" . projectile-replace)))

(leaf visual-regexp
  :ensure t
  :bind (:xah-fly-command-map
         :package xah-fly-keys
         ("SPC r" . vr/query-replace)))

(provide 'my-search)
;;; my-search.el ends here
