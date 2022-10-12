;;; my-rectangles.el --- My configuration for the `rect'

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

;; My configuration for the `rect'

;;; Code:
(require 'my-lib)

(leaf rect
  :bind (:xah-fly-command-map
         :package xah-fly-keys
         ("SPC t" . rectangle-mark-mode)
         ("SPC v" . yank-rectangle))
  :after xah-fly-keys
  :config                             ;nofmt
  (defun rectangle-mark-mode-p ()
    "Return t, when `rectangle-mark-mode' is enabled."
    rectangle-mark-mode)

  (define-key-when
    my-copy-rectangle-or-copy-line
    xah-fly-command-map
    "c"
    'copy-rectangle-as-kill
    'rectangle-mark-mode-p)

  (define-key-when
    my-kill-rectangle-or-delete-char
    xah-fly-command-map
    "d"
    'kill-rectangle
    'rectangle-mark-mode-p)

  (define-key-when
    my-kill-rectangle-or-kill-line
    xah-fly-command-map
    "x"
    'kill-rectangle
    'rectangle-mark-mode-p)

  (define-key-when
    my-xah-activate-insert-mode-or-replace-rectangle
    xah-fly-command-map
    "f"
    'replace-rectangle
    'rectangle-mark-mode-p)

  (define-key-when
    any-exchange-point-and-mark-or-splice-sexp
    xah-fly-command-map
    "-"
    'rectangle-exchange-point-and-mark
    'rectangle-mark-mode-p))

(provide 'my-rectangles)
;;; my-rectangles.el ends here
