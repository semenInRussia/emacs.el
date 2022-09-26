;;; my-doom-modeline.el --- my-doom-modeline

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
(use-package doom-modeline :ensure t)

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
  (let ((meta (concat (doom-modeline--macro-recording)
                      (doom-modeline--multiple-cursors))))
    (or (and (not (equal meta "")) meta)
        (doom-modeline--buffer-size))))

(defcustom my-modeline-time-segment-format-string " [%H-%M]"
  "By this format string will draw time in `doom-modeline'.
See `format-time-string' for see what format string"
  :type 'string)

(doom-modeline-def-segment time
    (let* ((hour (string-to-number (format-time-string "%H"))))
      (propertize
       (format-time-string my-modeline-time-segment-format-string)
       'face
       (if (< 4 hour 19)
           'hi-red-b
         'outline-1))))

(doom-modeline-def-segment pomidor
    ()
  "Return header."
  (when (featurep 'pomidor)
    (let* ((state (pomidor--current-state))
           (break (pomidor--break-duration state))
           (overwork (pomidor--overwork-duration state))
           (work (pomidor--work-duration state))
           (face (cond
                   (break 'pomidor-break-face)
                   (overwork 'pomidor-overwork-face)
                   (work 'pomidor-work-face)))
           (pomidor-time-format " Pom %-Mm")
           (time (-first 'identity (list break overwork work))))
      (propertize (pomidor--format-time time) 'face face))))

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
    (concat
     (s-truncate
      durand-buffer-name-max
      buffer-info))))

(setq flycheck-mode-line nil)

(defvar my-modeline-ignored-modes '(company-mode))

(use-package doom-modeline
    :ensure t
    :defer 0.1
    :init
    (size-indication-mode t)
    :custom
    (doom-modeline-buffer-file-name-style 'buffer-name)
    (doom-modeline-icon nil)
    (xah-fly-insert-state-p nil)
    :config
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