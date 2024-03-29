;;; my-comment-dwim-2.el --- My configuration for the `comment-dwim-2'

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

;; My configuration for the `comment-dwim-2'

;;; Code:
(require 'my-leaf)

(leaf comment-dwim-2
  :ensure (comment-dwim-2 :repo "remyferre/comment-dwim-2"
                          :host github)
  :bind ("M-;" . comment-dwim-2))

(provide 'my-comment-dwim-2)
;;; my-comment-dwim-2.el ends here
