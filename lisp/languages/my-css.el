;;; my-css.el --- My configuration for `css'

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

;; My configuration for `css'

;;; Code:

(require 'my-leaf)
(require 'dash)


(leaf facemenu
  :fast-exec ("Display All Colors" #'list-colors-display))

(leaf css-mode
  :config                               ;nofmt
  (leaf css-eldoc
    :ensure (css-eldoc :repo "zenozeng/css-eldoc" :host github)
    :hook (((css-mode-hook web-mode-hook)
            . css-eldoc-enable))))

(provide 'my-css)
;;; my-css.el ends here
