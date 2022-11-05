;;; my-doom-modeline.el --- My configuration for `doom-modeline'

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

;; My configuration for `doom-modeline'

;;; Code:

(require 's)

(leaf doom-modeline
  :ensure t
  :require t
  :init (size-indication-mode t)
  :defun ((doom-modeline--active
           doom-modeline-def-segment
           doom-modeline--macro-recording
           doom-modeline--multiple-cursors
           doom-modeline--buffer-size
           doom-modeline-segment--buffer-info
           doom-modeline-def-modeline
           doom-modeline-set-modeline)
          (my-drag-p . my-drag))
  :custom ((doom-modeline-buffer-file-name-style . 'buffer-name)
           (doom-modeline . 'buffer-name)
           (doom-modeline-icon                   . nil)
           (xah-fly-insert-state-p               . nil))
  :config                               ;nofmt
  (doom-modeline-def-segment drag
    (when (my-drag-p)
      (propertize
       " DRG "
       'face
       (if (doom-modeline--active)
           'doom-modeline-panel
         'mode-line-inactive))))

  (doom-modeline-def-segment my-matches
    "Display `macro-recoring', `multiple-cursors' and `buffer-size'."
    (let ((meta
           (concat
            (doom-modeline--macro-recording)
            (doom-modeline--multiple-cursors))))
      (or
       (and (not (equal meta "")) meta)
       (doom-modeline--buffer-size))))

  (defcustom my-modeline-time-segment-format-string " [%H-%M]"
    "By this format string will draw time in `doom-modeline'.
See `format-time-string' for see what format string"
    :type 'string
    :group 'my)

  (defface my-modeline-time-morning-face
    '((t (:foreground "#ff4500" :weight bold)))
    "Face for view of the time in modeline in the morning."
    :group 'my)

  (defface my-modeline-time-evening-face
    '((t (:foreground "#dcdcdc" :weight bold)))
    "Face for view of the time in modeline. in the evening"
    :group 'my)

  (doom-modeline-def-segment time
    (let* ((hour (string-to-number (format-time-string "%H"))))
      (propertize
       (format-time-string my-modeline-time-segment-format-string)
       'face
       (if (< 4 hour 19)
           'my-modeline-time-morning-face 'my-modeline-time-evening-face))))

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
    '((t :foreground "#7cfc00" :weight bold))
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

  (doom-modeline-def-segment pomidor
    ()
    "Return format string for `pomidor', view remainders minuts for break/work."
    (when (featurep 'pomidor)
      (format " %s " (my-pomidor-format-remaining-time))))

  (defvar durand-buffer-name-max 20
    "The maximal length of the buffer name in modeline.")

  (doom-modeline-def-segment buffer-info-durand
    ()
    (declare (pure t) (side-effect-free t))
    (let* ((buffer-info
            (format-mode-line
             (s-truncate
              durand-buffer-name-max
              (doom-modeline-segment--buffer-info)))))
      (concat (s-truncate durand-buffer-name-max buffer-info))))

  (setq flycheck-mode-line nil)

  (defvar my-modeline-ignored-modes '(company-mode))
  (display-time-mode t)
  (doom-modeline-def-modeline 'main
    '(bar
      my-matches
      drag
      buffer-info-durand
      time
      pomidor
      word-count
      selection-info)
    '(objed-state
      persp-name
      grip
      irc
      gnus
      github
      debug
      repl
      input-method
      indent-info
      buffer-encoding
      major-mode
      process
      vcs
      checker))
  (doom-modeline-set-modeline 'main t))

(provide 'my-doom-modeline)
;;; my-doom-modeline.el ends here
