;;; my-consult.el --- My config for `consult'

;; Copyright (C) 2022, 2023 Semen Khramtsov
;; Author: Semen Khramtsov <hrams205@gmail.com>
;; Version: 0.1
;; URL: https://github.com/semenInRussia/emacs.el

;;; Commentary:

;; My config for `consult': find files, search string, switch to buffer and
;; other useful things.

;;; Code:

(require 'my-leaf)
(require 'dash)

(eval-and-compile
  (require 'my-macros))

;; some useful things:
;;
;; - `ripgrep' in the project
;; - choose one from `kill-ring' with preview
;; - `imenu' with preview
;; - switch to buffer one of the project buffers, recent opened files and other
(leaf consult
  :ensure t
  :commands (consult-register-format
             consult-register-window
             consult-xref)
  :defvar (consult-narrow-key
           consult-project-function
           consult-buffer-sources)
  :bind ((:minibuffer-local-map
          ("M-s" . consult-history) ;; orig. next-matching-history-element
          ("M-r" . consult-history))
         (("C-x C-b" . consult-buffer)
          ("C-c i" . consult-imenu)
          ("C-c n" . consult-imenu-multi))
         (:meow-normal-state-keymap
          :package meow
          ("X" . consult-line)
          ("Q" . consult-goto-line))
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
         ("M-g I" . consult-imenu-multi)
         ("M-g i" . consult-imenu))

  :custom (;; Idk what is it
           (register-preview-delay  . 0.5)
           (register-preview-function . #'consult-register-format))

  ;; i don't know what does the next line
  :hook ((completion-list-mode-hook . consult-preview-at-point-mode))
  :config

  (leaf project
    :bind (:project-prefix-map
           ;; instead of built-in `projectile-find-regexp' sometimes use command
           ;; from `projectile-prefix-map' more useful , than "C-c s" for
           ;; example, when you swithch to project and need to find regexp
           ("g" . consult-ripgrep))
    :config
    ;; change "Find regrexp" with `consult-ripgrep' instead of
    ;; `project-find-regexp'
    (->>
     project-switch-commands
     (--replace-where
      (equal (-second-item it) "Find regexp")
      '(consult-ripgrep "Find regexp"))
     (setq project-switch-commands)))

  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Use Consult to select xref locations with preview
  (leaf xref
    :defvar (xref-show-xrefs-function
             xref-show-definitions-function)
    :custom ((xref-show-xrefs-function . #'consult-xref)
             (xref-show-definitions-function . #'consult-xref)))

  ;; `embark' like to flexible keymap that changes depending on
  ;; when I call `embark-act'
  ;;
  ;; here (in this file) only the integration of `embark' with `vertico'
  (leaf embark-consult
    :ensure t
    :hook (embark-collect-mode-hook . consult-preview-at-point-mode))

  ;; don't suggest `recentf' files in `consult-buffer', because on
  ;; Windows10 it can crush the Emacs :oops
  (when (equal system-type 'windows-nt)
    (remove-from-list! consult-buffer-sources
                       'consult--source-recent-file)))

(leaf consult-dir
  :ensure t)

(provide 'my-consult)
;;; my-consult.el ends here
