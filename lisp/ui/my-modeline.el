;;; my-modeline.el --- My configuration for modeline

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

;; My configuration for `modeline'

;;; Code:

(defvar flycheck-mode-line)

(require 'my-leaf)
(require 'dash)


(leaf moody
  :ensure t
  :require t
  :custom (flycheck-mode-line-prefix . "")
  :defun (moody-replace-vc-mode
          moody-replace-mode-line-buffer-identification)
  :config
  (setq x-underline-at-descent-line t)
  (setq-default mode-line-format
                '(" " (:eval (meow-indicator))
                  mode-line-modified
                  mode-line-client
                  mode-line-frame-identification
                  mode-line-buffer-identification
                  " " (:eval (my-modeline-pomidor))
                  " " (:eval (my-display-time-string))
                  " " (vc-mode vc-mode)
                  (multiple-cursors-mode mc/mode-line)
                  (lsp-bridge-mode mc/mode-line)
                  flycheck-mode-line
                  mode-line-end-spaces))
  (moody-replace-mode-line-buffer-identification)
  (moody-replace-vc-mode))


(declare-function pomidor--overwork-duration "pomidor.el")
(declare-function pomidor--work-duration "pomidor.el")
(declare-function pomidor-overwork-p "pomidor.el")
(declare-function pomidor--break-duration "pomidor.el")
(declare-function pomidor--current-state "pomidor.el")


(defcustom my-modeline-time-segment-format-string " [%H-%M]"
  "By this format string will draw time in modeline.

See `format-time-string' for see what format string"
  :type 'string
  :group 'my)

(defface my-modeline-time-morning-face
  '((t (:foreground "#ff4500" :weight bold)))
  "Face for view of the time in modeline in the morning."
  :group 'my)

(defface my-modeline-time-evening-face
  '((t (:foreground "#dcdcdc" :weight bold)))
  "Face for view of the time in modeline in the evening."
  :group 'my)

(defun my-display-time-string ()
  "Return the string that tell current time in the modeline."
  (let* ((hour (string-to-number (format-time-string "%H"))))
    (propertize
     (format-time-string my-modeline-time-segment-format-string)
     'face
     (if (< 4 hour 19)
         'my-modeline-time-morning-face 'my-modeline-time-evening-face))))

(defun my-modeline-pomidor ()
  "Return format string for `pomidor', view remainders minuts for break/work."
  (and
   (featurep 'pomidor)
   (--some (equal (buffer-name it) "*pomidor*") (buffer-list))
   (my-pomidor-format-remaining-time)))

(defun my-pomidor-kind ()
  "Return kind of curent `pomidor' state, either break, work or overwork."
  (cond
   ((plist-get (pomidor--current-state) :break)
    'break)
   ((pomidor-overwork-p)
    'overwork)
   ((plist-get (pomidor--current-state) :started)
    'work)))

(defface my-modeline-pomidor-break-face
  '((t :foreground "#ff4500" :underline t :weight bold))
  "Face showing in the mode line at time when `pomidor' has status break."
  :group 'my)

(defface my-modeline-pomidor-overwork-face
  '((t :foreground "#Ffa500" :underline t :weight bold))
  "Face showing in the mode line at time when `pomidor' has status overwork."
  :group 'my)

(defface my-modeline-pomidor-work-face
  '((t :foreground "#7cfc00" :underline t :weight bold))
  "Face showing in the mode line at time when `pomidor' has work status."
  :group 'my)

(defun my-pomidor-face ()
  "Return face for the current status of the current `pomidor' state."
  (cl-case
      (my-pomidor-kind)
    ((break)
     'my-modeline-pomidor-break-face)
    ((work)
     'my-modeline-pomidor-work-face)
    ((overwork)
     'my-modeline-pomidor-overwork-face)))

(defun my-pomidor-remaining-time ()
  "Return remaining time to the end of the pomidor work or break period.

Format of time is the list form the hours, minutes, seconds and zero?"
  (cl-case
      (my-pomidor-kind)
    ((work)
     (pomidor--work-duration (pomidor--current-state)))
    ((overwork)
     (pomidor--overwork-duration (pomidor--current-state)))
    ((break)
     (pomidor--break-duration (pomidor--current-state)))))

(defcustom my-pomidor-modeline-time-format "%M min"
  "String defining format of string viewing pomodoro time at the modeline."
  :group 'my
  :type 'string)

(defun my-pomidor-format-remaining-time ()
  "Format remaining time to the end of the pomidor work or break period."
  (propertize
   (format-time-string my-pomidor-modeline-time-format
                       (my-pomidor-remaining-time))
   'face
   (my-pomidor-face)))

(provide 'my-modeline)
;;; my-modeline.el ends here
