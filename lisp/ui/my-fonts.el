;;; my-fonts.el --- My configuration for fonts

;; Copyright (C) 2022, 2023 Semen Khramtsov

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

;; My configuration for fonts

;;; Code:
(require 'my-leaf)
(require 'dash)

;; you can install this font, usin `nerd-fonts' (search in the `github')
(defcustom my-fonts-main
  "Hack Nerd Font Mono"
  "Name of the main font to display all."
  :group 'my
  :type 'string)

(set-face-attribute 'default nil :font my-fonts-main :height 210)

;; (leaf unicode-fonts
;;   :ensure t
;;   :defun (unicode-fonts-setup unicode-fonts--instructions)
;;   :defvar (unicode-fonts
;;            unicode-fonts--instructions
;;            ...)
;;   :require t
;;   :init (defvar ... nil)
;;   :config (setq unicode-fonts--instructions
;;                 (-remove-item '... unicode-fonts--instructions))
;;   (unicode-fonts-setup))

(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-language-environment 'utf-8)
(set-selection-coding-system 'utf-8)

(provide 'my-fonts)
;;; my-fonts.el ends here
