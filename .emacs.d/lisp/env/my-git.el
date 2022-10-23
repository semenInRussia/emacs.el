;;; my-git.el --- My config of `git'

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

;; My config of `git'

;;; Code:

(leaf git-gutter                        ;nofmt
  :ensure t
  :hook prog-mode-hook)

(leaf magit                             ;nofmt
  :ensure t
  :custom (magit-refresh-status-buffer . nil)
  :config                               ;nofmt
  (leaf forge                           ;nofmt
    :ensure t
    :require t))

(leaf blamer
  :ensure t
  :custom ((blamer-idle-time . 0.3)
           (blamer-min-offset . 70))
  :custom-face (blamer-face .
                            '((t
                               (:foreground "#7a88cf"
                                            :background nil
                                            :height 140
                                            :italic t)))))

(leaf git-modes :ensure t)

(leaf helm-gitignore
  :after fast-exec
  :config                               ;nofmt
  :fast-exec ("Generate Gitignore" 'helm-gitignore))

(provide 'my-git)
;;; my-git.el ends here
