;;; my-embrace.el --- My configuration of the `embrace'

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

;; My configuration of the `embrace'

;;; Code:
(require 'my-leaf)

(leaf embrace
  :ensure t
  :defvar embrace-semantic-units-alist
  :setq-default (embrace-show-help-p . nil)
  :bind (:xah-fly-command-map
         :package xah-fly-keys
         ("/"         . embrace-commander)
         ("SPC SPC /" . xah-goto-matching-bracket))
  :hook (emacs-lisp-mode-hook . embrace-emacs-lisp-mode-hook)
  :config                             ;nofmt
  (unless (assq ?n embrace-semantic-units-alist)
    (setq-default embrace-semantic-units-alist
                  (cons
                   '(?n . embrace-avy-semantic-unit)
                   embrace-semantic-units-alist)))

  (defun embrace-avy-semantic-unit ()
    "Semantic unit for `embrace' which ask expression with the `avy'."
    (call-interactively 'avy-mark-word)))

(provide 'my-embrace)
;;; my-embrace.el ends here
