;;; my-consult.el --- My config for `consult'

;; Copyright (C) 2022, 2023 Semen Khramtsov
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

;; My config for `consult'

;;; Code:

(require 'my-leaf)

(require 'dash)

(leaf vertico
  :ensure t
  :global-minor-mode vertico-mode)

(leaf consult
  :ensure t
  :defun ((consult-register-format
           consult-register-window
           consult-xref
           ;; for my consult-ripgrep
           consult--grep
           consult--ripgrep-make-builder)
          (projectile-project-root . projectile))
  :defvar (consult-narrow-key consult-project-function)
  :bind (:minibuffer-local-map
         ("M-s" . consult-history) ;; orig. next-matching-history-element
         ("M-r" . consult-history))
  :bind (:xah-fly-command-map
         :package xah-fly-keys
         ("SPC SPC f" . find-file)
         ("SPC '" . my-consult-ripgrep)
         ("SPC SPC SPC" . consult-imenu)
         ("SPC SPC n" . consult-imenu-multi))
  :bind (;; C-c bindings in `mode-specific-map'
         ("C-c M-x" . consult-mode-command)
         ("C-c h" . consult-history)
         ("C-c k" . consult-kmacro)
         ("C-c m" . consult-man)
         ("C-c i" . consult-info)
         ([remap Info-search] . consult-info)
         ("M-#" . consult-register-load)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ;; M-g bindings in `goto-map'
         ("M-g e" . consult-compile-error)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings in `search-map'
         ("M-s d" . consult-find)
         ("M-s D" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         ;; Minibuffer history
         )
  :custom ((consult-narrow-key . "]")
           (register-preview-delay  . 0.5)
           (register-preview-function . #'consult-register-format)
           (consult-project-function . 'projectile-project-root))

  :hook ((completion-list-mode-hook . consult-preview-at-point-mode))

  ;; The :init configuration is always executed (Not lazy)
  :init

  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Use Consult to select xref locations with preview
  (leaf xref
    :defvar (xref-show-xrefs-function
             xref-show-definitions-function)
    :custom ((xref-show-xrefs-function . #'consult-xref)
             (xref-show-definitions-function . #'consult-xref)))

  (leaf orderless
    :ensure t
    :require t
    :custom (completion-styles . '(orderless)))

  (leaf marginalia
    :ensure t
    :global-minor-mode marginalia-mode)

  (defvar string-width 0)

  (defun compat-call (&rest b)
    0)

  (defun my-consult-ripgrep (&optional dir initial)
    "Search with `rg' for files in DIR with INITIAL input.

See `consult-grep' for details."
    (interactive "P")
    (or dir (setq dir (projectile-project-root)))
    (consult--grep "Ripgrep" #'consult--ripgrep-make-builder dir initial)))

(provide 'my-consult)
;;; my-consult.el ends here
