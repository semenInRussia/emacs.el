;;; my-aggressive-indent-mode.el --- My configuration of `aggressive-indent-mode' -*- lexical-binding: t; -*-

;; Copyright (C) 2023 semenInRussia

;; Author: semenInRussia <hrams205@gmail.com>
;; Version: 0.1
;; Homepage: https://github.com/semeninrussia/emacs.el

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; My configuration of `aggressive-indent-mode'.

;;; Code:

(require 'my-leaf)


(leaf aggressive-indent-mode
  :ensure (aggressive-indent-mode :repo "Malabarba/aggressive-indent-mode"
                                  :host github)
  :hook emacs-lisp-mode-hook
  :config
  (advice-add 'indent-region-line-by-line
              :around
              'my-remove-progresses)
  (advice-add 'lisp-indent-region
              :around
              'my-remove-progresses)

  (defun my-remove-progresses (fn &rest r)
    "Remove displaying of the progresses in FN, call it with R args."
    (cl-letf (((symbol-function 'make-progress-reporter) 'ignore)
              ((symbol-function 'progress-reporter-done) 'ignore)
              ((symbol-function 'progress-reporter-force-update) 'ignore)
              ((symbol-function 'dotimes-with-progress-reporter) 'ignore)
              ((symbol-function 'dolist-with-progress-reporter) 'ignore)
              ((symbol-function 'progress-reporter-do-update) 'ignore))
      (apply fn r))))

(provide 'my-aggressive-indent-mode)
;;; my-aggressive-indent-mode.el ends here
