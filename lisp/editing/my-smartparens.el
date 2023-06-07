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
  :defun (sp-clone-sexp sp-use-paredit-bindings)
  :bind (:smartparens-mode-map
         ("C-c C-w" . 'sp-splice-sexp-killing-backward)
         ("C-x C-y" . 'my-sp-clone)
         ("C-c DEL" . 'sp-change-enclosing)
         ("C-M-."   . 'sp-forward-slurp-sexp)
         ("C-M-,"   . 'sp-forward-barf-sexp)
         ("C-M->"   . 'sp-backward-slurp-sexp)
         ("C-M-<"   . 'sp-backward-barf-sexp))
  :config
  (sp-use-paredit-bindings)
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
