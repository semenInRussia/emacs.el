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

;; My config for `consult'.

;;; Code:

(require 'my-leaf)
(require 'dash)


(leaf vertico
  :ensure (vertico :host github
                   :repo "minad/vertico"
                   :files ("*.el" "extensions/*.el"))
  :global-minor-mode vertico-mode
  :config
  ;; I press `M-delete' to go the up directory inside of `vertico'
  (leaf vertico-directory
    :bind (:vertico-map
           :package vertico
           ;; instead I press TAB
           ;; ("RET" . vertico-directory-enter)
           ("DEL" . vertico-directory-delete-char)
           ("M-DEL" . vertico-directory-delete-word)))

  ;; beautifull icons inside `vertico'
  (leaf all-the-icons-completion
    :ensure t
    :defun all-the-icons-completion-mode
    ;; I load it after init because otherwise it doesn't work
    ;; I don't know WHYYY!?
    :hook after-init-hook))

;; some useful things:
;;
;; - `ripgrep' in the project
;; - choose one from `kill-ring' with preview
;; - `imenu' with preview
;; - switch to buffer one of the project buffers, recent opened files and other
(leaf consult
  :ensure t
  :defun (consult-register-format
          consult-register-window
          consult-xref)
  :defvar (consult-narrow-key consult-project-function)
  :bind (:minibuffer-local-map
         ("M-s" . consult-history) ;; orig. next-matching-history-element
         ("M-r" . consult-history))
  :bind (("C-x C-b" . consult-buffer)
         ("C-c i" . consult-imenu)
         ("C-c n" . consult-imenu-multi))
  :bind ((:project-prefix-map
          ;; instead of built-in `projectile-find-regexp'
          ;; sometimes use command from `projectile-prefix-map' more useful
          ;; , than "C-c s" for example, when you swithch to project and need to find regexp
          ("g" . consult-ripgrep))
         ;; C-c bindings in `mode-specific-map'
         ("C-c s" . consult-ripgrep)
         ("C-c M-x" . consult-mode-command)
         ("C-c k" . consult-kmacro)
         ("C-c m" . consult-man)
         ([remap Info-search] . consult-info)
         ("M-#" . consult-register-load)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ;; M-g bindings in `goto-map'
         ("M-g e" . consult-compile-error)
         ("M-g I" . consult-imenu-multi))
  :custom ((register-preview-delay  . 0.5)
           (register-preview-function . #'consult-register-format))

  ;; i don't know what does the next line
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

  ;; more fast way to search things inside `completing-read'
  (leaf orderless
    :ensure t
    :require t
    :custom (completion-styles . '(orderless)))

  ;; show a bit of additional info inside the `vertico' `minibuffer'
  (leaf marginalia
    :ensure t
    :global-minor-mode marginalia-mode)

  ;; `embark' is anything like to flexible keymap that changes depending on
  ;; when I call `embark-act'
  ;;
  ;; here only the integration of `embark' with `vertico', the configuration of
  ;; `vertico' inside `my-embark'
  (leaf embark-consult
    :ensure t
    :after consult)

  ;; the following lines fix some things which are wrong in my emacs@29
  (defvar string-width 0)

  (defun compat-call (&rest _)
    0))

(provide 'my-consult)
;;; my-consult.el ends here
