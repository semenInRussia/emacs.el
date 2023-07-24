;;; my-load-theme --- Load the current theme

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
;; See `doom-themes' (list of themes at starting comments)

;; List of my favorite themes:
;; - `doom-1337'.  The best one, I think
;; - `gruber-darker'.  Cool, but `org-mode' and `vertico' are bad
;; - `doom-monokai-classic'.  Cool
;; - `solarized'
;; - `flatland-theme'

(require 'my-leaf)

(setq font-lock-maximum-decoration t
      truncate-partial-width-windows nil)

(leaf doom-themes
  :ensure t)

(leaf gruber-darker-theme
  :ensure t)

(leaf monokai-theme
  :ensure t)

(require 'doom-themes-autoloads)
(load-theme 'doom-1337 t)

;; (custom-set-faces
;;  `(region
;;    ((t (:background "white")))))

(setq line-spacing 0.2)

(provide 'my-load-theme)
;;; my-load-theme.el ends here
