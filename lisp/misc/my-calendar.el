;;; my-calendar.el --- My configuration of `calendar' -*- lexical-binding: t; -*-

;; Copyright (C) 2022 semenInRussia
;; Author: semenInRussia <hrams205@gmail.com>
;; Version: 0.1
;; Homepage: https://github.com/semeninrussia/emacs.el

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; My configuration of `calendar'.

;;; Code:

(require 'my-leaf)

(leaf calendar
  :ensure t
  :defvar calendar-holidays
  :bind ("C-c C" . calendar)
  :config                               ;nofmt
  (leaf russian-holidays
    :ensure t
    :require t
    :defvar russian-holidays
    :config (setq calendar-holidays russian-holidays)))

(provide 'my-calendar)
;;; my-calendar.el ends here
