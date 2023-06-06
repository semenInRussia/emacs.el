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

(defvar dired-filter-map)
(defvar my-dired-commands-using-minibuffer)

;; the most heavy functions placed at the `my-dired-commands'
;; and will be evaluated when really needed
(declare-function my-dired-save-excursion "my-dired-commands.el")
(declare-function visual-fill "my-lang-utils.el")

(require 's)
(require 'dash)
(require 'just)
(require 'f)

(leaf dired
  :hook ((dired-mode-hook . xah-fly-insert-mode-activate)
         (dired-mode-hook . dired-hide-details-mode))
  :defun ((dired-mark
           dired-do-rename
           dired-current-directory
           dired-jump
           dired-get-file-for-visit)
          (org-link-open-from-string . ol))
  :bind (:dired-mode-map
         ("SPC"     . nil)                ; make command at space empty prefix

         ;; Navigation
         ("k"       . next-line)
         ("i"       . previous-line)
         ("n"       . dired-avy)
         ("SPC h"   . beginning-of-buffer)
         ("SPC n"   . end-of-buffer)
         ("'"       . dired-isearch-filenames)

         ;; Open file
         ("o"       . dired-find-file-other-window)
         ;; some other open files I define in the section "Dired Hacks: Open"

         ;; Mark files
         ("t"       . dired-mark)
         ("SPC u"   . dired-unmark-all-marks)

         ;; Misc.
         ("y"       . dired-undo)
         ("A"       . agnifize-dwim)
         ("~"       . my-dired-jump-to-home)

         ;; Key bindings which not change your commands
         ("a"       . execute-extended-command)
         (","       . xah-next-window-or-frame))

  :config
  (leaf dired-async
    :ensure async
    :defun dired-async-mode
    :global-minor-mode dired-async-mode)

  (leaf my-dired-commands
    :bind (:dired-mode-map
           :package dired
           ;; Mark anything
           ("SPC a"   . my-dired-mark-all-files)
           ;; Manipulation with file(s)
           ;; copy/move/paste also defines in the section "Dired Hacks: Ranger"
           ("SPC g"   . my-dired-delete)
           ("SPC x"   . my-dired-delete-all-files)
           ("SPC y"   . my-dired-duplicate)
           ("f"       . my-dired-rename)
           ("SPC TAB" . my-dired-move)
           ("s"       . my-dired-new-file)
           ("j"       . my-dired-goto-parent-dir)))

  (leaf dired-filter
    :ensure t
    :require t
    :defvar dired-filter-map
    :defer-config
    (require 'dired-filter)
    (leaf-keys-bind-keymap
     ((dired-mode-map :package dired
                      ("." . dired-filter-map)))
     nil 'dired-filter))

  (leaf dired-open
    :ensure t
    :bind (:dired-mode-map
           :package dired
           ("l"   . 'dired-open-file)
           ("RET" . 'dired-open-file))
    :push ((dired-open-functions . 'my-dired-open-function-pdf))
    :defvar dired-open-functions
    :defun (my-dired . (my-pdf-file my-try-open-pdf-file))
    :config                             ;nofmt
    (defun my-dired-open-function-pdf ()
      "Open function for `dired-open-functions'."
      (my-try-open-pdf-file (dired-get-file-for-visit)))

    (defun my-try-open-pdf-file (filename)
      "If file at FILENAME is a pdf file, then open as pdf, other return nil."
      (when (my-pdf-file filename)
        (xah-open-in-external-app)
        (org-link-open-from-string filename)
        t))

    (defun my-pdf-file (filename)
      "Return t, when FILENAME is path to a PDF file."
      (s-suffix-p ".pdf" filename)))

  (leaf dired-ranger
    :ensure t
    :bind (:dired-mode-map
           :package dired
           ("m" . 'dired-ranger-move)
           ("v" . 'dired-ranger-paste)
           ("c" . 'dired-ranger-copy)))

  (leaf dired-subtree
    :ensure t
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
       (dired-previous-line 1)
       (dired (thing-at-point 'filename)))))

  (leaf dired-collapse
    :ensure t
    :hook (dired-mode-hook . dired-collapse-mode))

  (defcustom my-dired-commands-using-minibuffer
    '(dired-filter-by-file
      dired-filter-by-extension
      my-dired-new-file
      dired-byte-compile
      dired-do-delete
      dired-create-directory
      dired-isearch-filenames)
    "List of the `dired' commands using the minubuffer."
    :type '(repeat symbol)
    :group 'my)

  (advice-add 'dired-jump
              :after (lambda (&rest _ignore) (xah-fly-insert-mode-activate))
              '((name . "xah-fly-insert-mode-activate")))

  (--each my-dired-commands-using-minibuffer
    (advice-add it :after
                (lambda (&rest _) (xah-fly-insert-mode-activate))
                '((name . xah-fly-insert-mode-activate))))

  ;; Command for printing file
  (with-eval-after-load 'lpr (setq lpr-command "PDFToPrinter"))

  (remove-hook 'dired-mode-hook 'dired-mode))

(provide 'my-dired)
;;; my-dired.el ends here
