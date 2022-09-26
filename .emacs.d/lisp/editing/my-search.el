;;; my-search.el --- my-search

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
(use-package helm-swoop
    :ensure t
    :bind
    ((:map xah-fly-command-map)
     ("'"       . helm-swoop)
     ("SPC k '" . helm-multi-swoop-current-mode))
    :bind
    ((:map helm-swoop-map)
     ("M-j" . 'helm-swoop-edit)
     (:map helm-swoop-edit-map)
     ([remap save-buffer] . 'helm-swoop--edit-complete)))

(use-package deadgrep
    :ensure t
    :bind
    ((:map xah-fly-command-map)
     ("SPC '" . deadgrep)))

(use-package projectile
    :ensure t
    :bind
    ((:map xah-fly-command-map)
     ("SPC SPC r" . projectile-replace)))

(use-package visual-regexp
    :ensure t
    :bind
    ((:map xah-fly-command-map)
     ("SPC r" . vr/query-replace)))

(provide 'my-search)
;;; my-search.el ends here