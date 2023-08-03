;;; my-dired.el --- My configuration of the `dired'

;; Copyright (C) 2022 Semen Khramtsov

;; Author: Semen Khramtsov <hrams205@gmail.com>
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


;; the most heavy functions placed at the `my-dired-commands'
;; and will be evaluated when really needed (autoloading)
(declare-function my-dired-save-excursion "my-dired-commands.el")
(declare-function embark-open-externally "embark")


(leaf dired
  ;; don't show extra info about files like:
  ;; - owner
  ;; - group
  ;; - last modified time
  ;; but u can show it with ")"
  :hook (dired-mode-hook . dired-hide-details-mode)
  :defun ((my-dired-save-excursion . my-dired-commands.el)
          (embark-open-externally . embark)
          dired-get-file-for-visit)
  :bind (:dired-mode-map
         ;; i'm the user of `meow' with hjkl, where "h" is right, so i press
         ;; right to go the "back" directory
         ("h" . dired-up-directory)
         ("A" . agnifize-dwim))
  :config
  ;; some my commands for `dired'
  (leaf my-dired-commands
    :bind (:dired-mode-map
           :package dired
           ("~" . my-dired-jump-to-home)
           ("C-x h"   . my-dired-mark-all-files)
           ("C-y"     . my-dired-duplicate)
           ("C-o"     . my-dired-new-file)))

  (leaf dired-async
    :ensure async
    :global-minor-mode dired-async-mode)

  (leaf dired-hacks-utils
    :ensure t)

  ;; open PDF and other not in Emacs
  (leaf dired-open
    :ensure t
    :bind (:dired-mode-map
           :package dired
           ("RET" . 'dired-open-file))
    :defvar dired-open-functions
    :push ((dired-open-functions . 'my-dired-open-function-pdf))
    :defun (my-dired . (my-pdf-file-p my-try-open-pdf-file))
    :config                             ;nofmt
    (defun my-dired-open-function-pdf ()
      "Open function for `dired-open-functions'."
      (my-try-open-pdf-file (dired-get-file-for-visit)))

    (defun my-try-open-pdf-file (filename)
      "If file at FILENAME is a pdf file, then open as pdf, other return nil."
      (when (my-pdf-file-p filename)
        (embark-open-externally filename)
        t))

    (defun my-pdf-file-p (filename)
      "Return t, when FILENAME is path to a PDF file."
      (s-suffix-p ".pdf" filename)))

  (leaf dired-subtree
    :ensure (dired-subtree :repo "Fuco1/dired-hacks" :host github)
    :defun dired-subtree-beginning
    :bind (:dired-mode-map
           :package dired
           ("TAB" . 'dired-subtree-cycle)
           ("/"   . 'my-dired-subtree-in-special-buffer))
    :config                           ;nofmt
    (defun my-dired-subtree-in-special-buffer ()
      "Open current `dired-subtree' in the separate `dired' buffer."
      (interactive)
      (my-dired-save-excursion
       (dired-subtree-beginning)
       (forward-line -1)
       (dired (thing-at-point 'filename)))))

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
    :require t)

  ;; ???
  (remove-hook 'dired-mode-hook 'dired-mode))

;;; my-dired.el ends here
(provide 'my-dired)
