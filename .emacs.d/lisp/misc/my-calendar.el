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

(leaf calendar
  :ensure t
  :bind ((:xah-fly-command-map ("SPC i c" . 'calendar))
         (:calendar-mode-map
          ("l" . 'calendar-forward-day)
          ("j" . 'calendar-backward-day)
          ("i" . 'calendar-backward-week)
          ("k" . 'calendar-forward-week)
          (";" . 'calendar-forward-month)
          ("h" . 'calendar-backward-month)
          ("H" . 'calendar-cursor-holidays)
          ("]" . 'calendar-forward-year)
          ("[" . 'calendar-backward-year)
          ("=" . 'calendar-count-days-region)))
  :config                               ;nofmt
  (leaf russian-holidays
    :ensure t
    :require t
    :config (setq calendar-holidays russian-holidays)))

(provide 'my-calendar)
;;; my-calendar.el ends here
