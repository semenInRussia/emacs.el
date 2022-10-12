;;; my-smartkeys.el --- Some key bindings which depends on context -*- lexical-binding: t; -*-

;; Copyright (C) 2022 semenInRussia

;; Author: semenInRussia <hrams205@gmail.com>
;; Version: 0.0.1
;; Package-Requires: ((dash "2.18.0") (s "1.12.0"))
;; Homepage: https://github.el/semenInRussia/emacs.el

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

;; Some key bindings which depends on context.

;;; Code:

(require 'my-lib)

(eval-after-load 'smartparens
  '(progn
     (define-key-when my-kill-line-or-region
                      xah-fly-command-map
                      "x"               ;nofmt
                      'kill-region
                      'use-region-p)

     (define-key-when my-exchange-point-and-mark-or-splice-sexp
                      xah-fly-command-map
                      "-"
                      'exchange-point-and-mark
                      'use-region-p)))

(provide 'my-smartkeys)
;;; my-smartkeys.el ends here
