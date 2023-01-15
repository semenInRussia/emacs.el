;;; my-helm-github-stars.el --- My config of `helm-github-stars'

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

;; My config of `helm-github-stars'

;;; Code:
(leaf helm-github-stars
  :ensure t
  :after helm
  :fast-exec ("View Github Stars" 'helm-github-stars-fetch)
  :custom (helm-github-stars-username . "semeninrussia")
  :commands helm-github-stars-fetch)

(provide 'my-helm-github-stars)
;;; my-helm-github-stars.el ends here
