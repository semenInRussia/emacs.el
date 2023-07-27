;;; my-dired.el --- My configuration of the `dired'

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

;; My configuration of the `dired'

;;; Code:

(require 'my-leaf)
(require 's)
(require 'dash)
(require 'just)
(require 'f)


(defvar dired-filter-map)

;; the most heavy functions placed at the `my-dired-commands'
;; and will be evaluated when really needed (autoloading)
(declare-function my-dired-save-excursion "my-dired-commands.el")
(declare-function embark-open-externally "embark")


(leaf dired
  ;; don't show extra info about files like:
  ;; - owner
  ;; - group
  ;; - last modified time
  :hook (dired-mode-hook . dired-hide-details-mode)
  :defun dired-get-file-for-visit
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
    :defun dired-async-mode
    :global-minor-mode dired-async-mode)

  (leaf dired-hacks-utils
    :ensure t)

  ;; filter files from the buffre
  (leaf dired-filter
    :ensure (dired-filter
             :repo "Fuco1/dired-hacks"
             :host github)
    :disabled t
    :defvar dired-filter-map
    :bind-keymap (:dired-mode-map
                  :package dired
                  ("." . dired-filter-map)))

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

  ;; ???
  (remove-hook 'dired-mode-hook 'dired-mode))

;;; my-dired.el ends here
(provide 'my-dired)
