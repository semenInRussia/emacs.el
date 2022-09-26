;;; my-buffer-navigation.el --- my-buffer-navigation

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
(defun my-visit-last-opened-buffer ()
  "Visit buffer which was opened recently."
  (interactive)
  (switch-to-buffer (my-last-opened-buffer)))

(defun my-last-opened-buffer ()
  "Get buffer which was visited most recently."
  (--find (not (my--visit-last-opened-buffer-ignore-p it))
          (cdr (buffer-list))))

(defun my--visit-last-opened-buffer-ignore-p (buffer)
  "Take object of BUFFER and return nil when don't need visit its."
  (->>
   buffer
   (buffer-name)
   (s-trim)
   (s-prefix-p "*Minibuf")))

(bind-keys
 :map xah-fly-command-map
 ("SPC 0" . my-visit-last-opened-buffer))

(provide 'my-buffer-navigation)
;;; my-buffer-navigation.el ends here