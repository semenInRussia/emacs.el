;;; my-helm.el --- my-helm

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
(use-package helm
    :ensure t
    :after (helm-core)
    :custom
    (helm-M-x-fuzzy-match           t)
    (helm-buffers-fuzzy-matching    t)
    (helm-recentf-fuzzy-match       t)
    (helm-imenu-fuzzy-match         t)
    (helm-autoresize-min-height    20)
    (helm-left-margin-width         4)
    (helm-buffers-left-margin-width 4)
    :bind
    (("C-h a"     . 'helm-apropos)
     (:map xah-fly-command-map)
     ("SPC SPC f" . 'helm-find-files)
     ("SPC k r"   . 'helm-regexp))
    :config
    (helm-mode t)
    (helm-autoresize-mode t)
    (defvar helm-completion-style nil))

(use-package helm-ext
    :ensure t
    :config
    (helm-ext-ff-enable-skipping-dots +1)
    (helm-ext-ff-enable-auto-path-expansion t))

(defun fast-exec-helm-net-define-keys ()
  "Keymaps for `helm-net' and `fast-exec'."
  (fast-exec/some-commands
   ("Search via Google" 'helm-google-suggest)))

(fast-exec/register-keymap-func 'fast-exec-helm-net-define-keys)
(fast-exec/reload-functions-chain)

(defun fast-exec-helm-colors-keys ()
  "Get some useful keymaps of  `fast-exec' for helm-colors."
  (fast-exec/some-commands ("Get Color" 'helm-colors)))

(fast-exec/register-keymap-func 'fast-exec-helm-colors-keys)
(fast-exec/reload-functions-chain)

(provide 'my-helm)
;;; my-helm.el ends here