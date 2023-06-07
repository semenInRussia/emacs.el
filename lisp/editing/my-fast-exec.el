;;; my-fast-exec.el --- My configuration of the `fast-exec'

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

;; My configuration of the `fast-exec'

;;; Code:

(require 'my-leaf)

(require 'dash)

(declare-function visual-fill "my-lang-utils")

(leaf fast-exec
  :load-path "~/projects/fast-exec.el/"
  :defun fast-exec-use
  :require t
  :bind ("M-=" . fast-exec-exec)
  :commands fast-exec-exec
  :config (add-hook 'fast-exec-hint-buffer-mode-hook #'visual-fill)
  (require 'my-fast-exec-misc))

(provide 'my-fast-exec)
;;; my-fast-exec.el ends here
