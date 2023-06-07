;;; my-misc.el --- My some little miscellaneous feautures

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

;; My some little miscellaneous feautures

;;; Code:
(require 'my-leaf)
(require 'dash)
(require 'f)

(setq make-backup-files         nil)
(setq auto-save-list-file-name  nil)
(defalias 'yes-or-no-p 'y-or-n-p)
(toggle-truncate-lines t)

(add-to-list 'load-path (f-join (-last-item load-path) ; path to share/lisp
                                ".."
                                "leim"))
(add-to-list 'load-path (f-join (-last-item load-path) ; path to share/lisp
                                ".."
                                "leim/quail"))

(setq default-input-method "russian-computer")
(setq default-file-name-coding-system 'utf-8)
(setq default-keyboard-coding-system 'utf-8)
(setq buffer-file-coding-system 'utf-8)

(defun my-new-fake-pptx-file ()
  "Make this buffer, fake presentation with format (.pptx)."
  (interactive)
  (->> "~/broken.pptx" (f-read) (insert))
  (text-mode))

(require 'fast-exec)
(eval-after-load 'fast-exec
  '(progn
     (fast-exec-bind 'pptx
       (fast-exec-make-some-commands
        ("New Fake PPTX File" 'my-new-fake-pptx-file)))))

;; Startup time
(defun my-display-startup-time ()
  (message
   "Emacs loaded in %s with %d garbage collections."
   (format
    "%.2f seconds"
    (float-time (time-subtract after-init-time before-init-time)))
   gcs-done))

(add-hook 'emacs-startup-hook #'my-display-startup-time)

(provide 'my-misc)
;;; my-misc.el ends here
