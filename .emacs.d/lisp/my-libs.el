;;; my-libs.el --- Some libraries

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

;; Some libraries

;;; Code:
(leaf s :ensure t :require t)

(leaf f :ensure t :require t)

(leaf dash :ensure t :global-minor-mode global-dash-fontify-mode :require t)

(leaf just                              ;nofmt
  :ensure (just :host github :repo "semenInRussia/just.el")
  :require t)

(leaf queue :ensure t)

(leaf request :ensure t)

(provide 'my-libs)
;;; my-libs.el ends here
