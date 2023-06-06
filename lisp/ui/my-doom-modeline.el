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
(defvar flycheck-mode-line)

(require 'my-leaf)
(require 'dash)

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
          ((my-pomidor-format-remaining-time
            my-pomidor-remaining-time
            my-pomidor-face
            my-modeline-time-segment-format-string
            my-pomidor-kind)
           . my-doom-modeline)
          (my-drag-p . my-drag))
  :defvar (my-modeline-time-segment-format-string
           my-pomidor-modeline-time-format)
  :custom ((doom-modeline-buffer-file-name-style . 'buffer-name)
           (doom-modeline . 'buffer-name)
           (doom-modeline-icon . t)
           (xah-fly-insert-state-p . nil))
  :config                               ;nofmt
  (require 'my-doom-modeline-segments)
  (doom-modeline-set-modeline 'main t))

(provide 'my-doom-modeline)
;;; my-doom-modeline.el ends here
