;;; my-dired.el --- My configuration of the `dired'

;; Copyright (C) 2022-2024 semenInRussia

;; Author: semenInRussia <hrams205@gmail.com>
;; Version: 0.1
;; URL: https://github.com/semenInRussia/emacs.el

;;; Commentary:

;; My configuration of the `dired': the powerful directory explorer inside of
;; the Emacs.

;;; Code:

(require 'my-leaf)
(require 'dash)
(require 's)
(require 'just)
(require 'f)

(leaf dired
  ;; don't show extra info about files like:
  ;; - owner
  ;; - group
  ;; - last modified time
  ;; but u can show it with "("
  :hook (dired-mode-hook . dired-hide-details-mode)
  :custom ((dired-dwim-target . t)
           (delete-by-moving-to-trash . t))
  :bind (:dired-mode-map
         ;; i'm the user of `meow' with hjkl, where "h" is right, so i press
         ;; right to go the "back" directory
         ("h" . dired-up-directory)
         ("A" . agnifize-dwim))
 :config
 ;; I use `repeat-mode' which have a stupid default option:
 ;; when I hit C-x C-j (`dired-jump') and press j, it another time
 ;; call `dired-jump'.
 ;;
 ;; disable it
 (put 'dired-jump 'repeat-map nil)

 ;; some my commands for `dired'
 (leaf my-dired-commands
   :bind (:dired-mode-map
          :package dired
          ("~" . my-dired-jump-to-home)
          ("C-x h" . my-dired-mark-all-files)
          ("C-y" . my-dired-duplicate)
          ("C-o" . my-dired-new-file)))

  (leaf dired-async
    :ensure async
    :global-minor-mode t)

  (leaf dired-hacks-utils
    :ensure t)

  ;; show directories with 1 file
  ;;
  ;; it be like "a/b.txt", instead of just "a"
  (leaf dired-collapse
    :ensure (dired-collapse :repo "Fuco1/dired-hacks" :host github)
    :hook dired-mode-hook)

  ;; icons inside `dired'
  (leaf nerd-icons-dired
    :ensure (nerd-icons-dired :repo "rainstormstudio/nerd-icons-dired" :host github)
    :hook dired-mode-hook)

  ;; Command for printing file
  (with-eval-after-load 'lpr
    (setq lpr-command "PDFToPrinter"))

  ;; some `dired' add-ons which are built-in Emacs
  (leaf dired-x
    :commands (virtual-dired
               dired-x-hands-off-my-keys
               dired-x-find-file-other-window)
    :bind (:dired-mode-map
           :package dired
           ("* ." . dired-mark-suffix)
           ("M-!" . dired-smart-shell-command)))

  ;; ???
  (remove-hook 'dired-mode-hook 'dired-mode))

;;; my-dired.el ends here
(provide 'my-dired)
