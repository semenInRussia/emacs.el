;;; my-helm.el --- My config for `helm'

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

;; My config for `helm'

;;; Code:
(leaf helm
  :ensure t
  :after doom-modeline
  :custom ((helm-M-x-fuzzy-match           . t)
           (helm-buffers-fuzzy-matching    . t)
           (helm-recentf-fuzzy-match       . t)
           (helm-imenu-fuzzy-match         . t)
           (helm-autoresize-min-height     . 20)
           (helm-left-margin-width         . 4)
           (helm-buffers-left-margin-width . 4))
  :bind (("C-h a"      . helm-apropos)
         (:xah-fly-command-map
          :package xah-fly-keys
          ("SPC SPC f" . helm-find-files)
          ("a"         . helm-M-x)
          ("SPC k r"   . helm-regexp)
          ("SPC i d" . 'helm-google-suggest)))
  :global-minor-mode helm-mode
  :fast-exec (("Get Color" 'helm-colors)
              ("Search in Goggle" 'helm-google-suggest))
  :defvar helm-completion-style
  :config                               ;nofmt
  (leaf helm-ext
    :ensure t
    :require t
    :custom ((helm-ext-ff-enable-skipping-dots       . t)
             (helm-ext-ff-enable-auto-path-expansion . t))))

(provide 'my-helm)
;;; my-helm.el ends here
