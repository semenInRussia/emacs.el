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

(leaf minions
  :ensure t
  :global-minor-mode minions-mode)

(leaf moody
  :ensure t
  :require t
  :config
  (setq x-underline-at-descent-line t)
  (setq-default mode-line-format
                '(" "
                  mode-line-front-space
                  mode-line-client
                  mode-line-frame-identification
                  mode-line-buffer-identification " " mode-line-position
                  (vc-mode vc-mode)
                  (multiple-cursors-mode mc/mode-line)
                  " " mode-line-modes
                  mode-line-end-spaces))

  (leaf minions
    :ensure t
    :custom ((minions-mode-line-lighter . "…")
             (minions-mode-line-delimiters . '("" . "")))
    :global-minor-mode minions-mode)

  (setq global-mode-string (remove 'display-time-string global-mode-string))

  (line-number-mode 0)
  (moody-replace-mode-line-buffer-identification)
  (moody-replace-vc-mode))

(provide 'my-modeline)
;;; my-modeline.el ends here
