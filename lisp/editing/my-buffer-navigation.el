;;; my-buffer-navigation.el --- My config for navigation beetween buffers

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

;; My config for navigation beetween buffers

;;; Code:
(require 'my-leaf)

(require 'dash)
(require 's)

(leaf ace-window
  :ensure t
  :bind ("M-o" . ace-window))

(defun my-visit-last-opened-buffer ()
  "Visit buffer which was opened recently."
  (interactive)
  (switch-to-buffer (my-last-opened-buffer)))

(defun my-last-opened-buffer ()
  "Get buffer which was visited most recently."
  (--find
   (not (my--visit-last-opened-buffer-ignore-p it))
   (cdr (buffer-list))))

(defun my--visit-last-opened-buffer-ignore-p (buffer)
  "Take object of BUFFER and return nil when don't need visit its."
  (->> buffer (buffer-name) (s-trim) (s-prefix-p "*Minibuf")))

(defun my-kill-current-buffer ()
  "Kill current opened buffer."
  (interactive)
  (kill-buffer (current-buffer)))

(leaf-keys
 ("C-TAB" . 'my-visit-last-opened-buffer)
 ("C-x k" . 'my-kill-current-buffer))

(provide 'my-buffer-navigation)
;;; my-buffer-navigation.el ends here
