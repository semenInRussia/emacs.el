;;; my-dumb-jump.el --- My configuration of the `dumb-jump'

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

;; My configuration of the `dumb-jump'

;;; Code:

(require 'my-leaf)

(leaf rg
  :ensure t)

(leaf dumb-jump
  :ensure t
  :custom ((dumb-jump-prefer-searcher dumb-jump-force-searcher)
           . 'rg)
  :bind (("M-," . xref-go-back)
         ("M-." . xref-find-definitions))
  :hook (xref-backend-functions . dumb-jump-xref-activate))

(provide 'my-dumb-jump)
;;; my-dumb-jump.el ends here
