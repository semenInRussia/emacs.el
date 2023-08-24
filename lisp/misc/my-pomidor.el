;;; my-pomidor.el --- My configuration for `pomidor': pomodoro techniques
;; Copyright (C) 2022-2023 Semen Khramtsov

;;; Commentary:

;; My configuration for `pomidor': pomodoro techniques

;;; Code:

(require 'my-leaf)
(require 'dash)


(leaf pomidor
  :ensure t
  :bind (("<f12>" . pomidor)
         (:pomidor-mode-map
          ("Q" . kill-buffer)))
  :custom ((pomidor-sound-tack        . nil)
           (pomidor-seconds           . 1800) ; 30min
           (pomidor-sound-tick        . nil)
           (pomidor-confirm-end-break . nil)))

(declare-function pomidor--break-duration "pomidor.el")
(declare-function pomidor--current-state "pomidor.el")
(declare-function pomidor--overwork-duration "pomidor.el")
(declare-function pomidor--work-duration "pomidor.el")
(declare-function pomidor-overwork-p "pomidor.el")

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

(provide 'my-pomidor)
;;; my-pomidor.el ends here
