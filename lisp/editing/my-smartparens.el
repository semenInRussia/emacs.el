;;; my-smartparens.el --- My configuration for the `smartparens'

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

;; My configuration for the `smartparens'

;;; Code:

(require 'my-leaf)
(require 'my-lib)

(leaf smartparens
  :ensure t
  :global-minor-mode smartparens-global-mode
  :require smartparens-config
  :defun sp-clone-sexp
  :bind (;; Wrap anything
         (global-map
          :package xah-fly-keys
          ("M-("       . sp-wrap-round)
          ("M-["       . sp-wrap-square)
          ("M-{"       . sp-wrap-curly))
         (:xah-fly-command-map
          :package xah-fly-keys
          ;; Kill anything
          ("x"         . sp-kill-whole-line)
          ("DEL"       . delete-only-1-char)
          ("SPC 8"     . sp-kill-sexp)
          ("SPC k e"   . sp-backward-kill-sexp)
          ("SPC SPC g" . sp-kill-hybrid-sexp)
          ("SPC SPC e" . sp-backward-kill-sexp)
          ("SPC -"     . sp-rewrap-sexp)
          ("-"         . sp-splice-sexp)
          ("SPC 9"     . sp-change-enclosing)

          ;; Change wrap placement
          ("]"         . 'sp-slurp-hybrid-sexp)
          ("["         . 'sp-forward-barf-sexp)
          ("SPC ["     . 'sp-backward-slurp-sexp)
          ("SPC ]"     . 'sp-backward-barf-sexp)
          ("SPC ="     . 'sp-raise-sexp)

          ;; Navigation
          ("m"         . 'sp-backward-sexp)
          ("."         . 'sp-forward-sexp)

          ;; misc.
          ("SPC 1"     . sp-join-sexp)
          ("SPC SPC 1" . sp-split-sexp)
          ("SPC SPC y" . my-sp-clone)))
  :config                               ;nofmt
  (defun my-sp-clone ()
    (interactive)
    (sp-clone-sexp)
    (repeat-at-last-keystroke))

  (defun delete-only-1-char ()
    "Delete only 1 character before point."
    (interactive)
    (backward-char)
    (delete-char 1)))

(provide 'my-smartparens)
;;; my-smartparens.el ends here
