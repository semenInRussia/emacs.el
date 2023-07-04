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

(require 'my-leaf)


(leaf doom-modeline
  :ensure t
  :custom (;; it looks like more nice
           (doom-modeline-height . 60)
           ;; enconding not useful I think.
           (doom-modeline-buffer-encoding . nil)
           ;; don't show directory names in `doom-modeline'
           (doom-modeline-project-detection . 'project)
           (doom-modeline-buffer-file-name-style . 'buffer-name))
  :hook after-init-hook
  :config
  ;; I use Emacs in fullscreen mode, so I don't see time that provided
  ;; by OS, so I need time in modeline.  EMACS IS MY OS!!!
  ;; I need only to time (not date) in 24hour format
  (defvar display-time-format)  ;; make compile happy
  (setq display-time-format "%H:%M")
  (display-time-mode 1)

  ;; disable show line and column numbers in modeline, because it only
  ;; take off extra place
  (column-number-mode 0)
  (line-number-mode 0))

(provide 'my-modeline)
;;; my-modeline.el ends here
