;;; my-misc.el --- my-misc

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
(setq make-backup-files         nil)
(setq auto-save-list-file-name  nil)
(defalias 'yes-or-no-p 'y-or-n-p)
(toggle-truncate-lines 38)

(add-hook 'prog-mode-hook 'whitespace-mode)

(add-hook 'change-major-mode-hook 'visual-line-mode)

(setq default-input-method "russian-computer")

(setq buffer-file-coding-system 'utf-8)

(defun my-new-fake-pptx-file ()
  "Make this buffer, fake presentation with format (.pptx)."
  (interactive)
  (->> "~/broken.pptx" (f-read) (insert))
  (text-mode))

(defun fast-exec-fake-pptx-keys ()
  "Get some useful keymaps of  `fast-exec' for fake-pptx."
  (fast-exec/some-commands ("New Fake PPTX File" 'my-new-fake-pptx-file)))

(fast-exec/register-keymap-func 'fast-exec-fake-pptx-keys)
(fast-exec/reload-functions-chain)

(provide 'my-misc)
;;; my-misc.el ends here