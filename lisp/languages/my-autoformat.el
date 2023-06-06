;;; my-autoformat.el --- My function for `autoformat'

;; Copyright (C) 2022-2023 Semen Khramtsov

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

;; My function for `autoformat'

;;; Code:
(require 'my-leaf)

(require 'just)
(require 'dash)

(defgroup my-autoformat nil
  "Automatically format of source code (add spaces, capitalze and etc)."
  :group 'editing)

(defcustom my-autoformat-sentence-end "[.?!]  "
  "Regexp that indicates the end of a sentence for `autoformat'."
  :group 'my-autoformat
  :type 'regexp)

(defvar my-autoformat-local-functions nil
  "Autoformat functions that will be called to format a code.

This variable is local to each buffer, so every buffer can has own special
formatting functions.")

(make-local-variable 'my-autoformat-local-functions)

(defcustom my-autoformat-global-functions
  nil
  "Autoformat functions that working everywhere.

This be like on `my-autoformat-local-functions', but works globally"
  :group 'my-autoformat
  :type '(repeat function))

(defun my-autoformat-sentence-capitalization (&optional prev-line-can-be-text)
  "Auto-capitalize the first letter of a sentence.

Either at the beginning of a line, or after a sentence end, if
PREV-LINE-CAN-BE-TEXT is nil (by default), then capitalize only when
previous line is empty."
  (interactive)
  (just-call-on-backward-char*
   (and
    (looking-at-p "[[:alpha:]]")
    (or prev-line-can-be-text
        (just-call-on-prev-line 'just-line-is-whitespaces-p)
        (equal (line-beginning-position) (point-min)))
    (or
     (just-beginning-of-line-text-p)
     (bobp)
     (looking-back my-autoformat-sentence-end nil))
    (upcase-char 1))))

(defun my-previous-line-is-empty ()
  "Return non-nil value, if the previous line is empty.

See `just-line-is-whitespaces-p' to understand what \"empty\" is mean"
  (just-call-on-prev-line 'just-line-is-whitespaces-p))

(defun my-autoformat-do ()
  "Funcall each of autoformat functions.

See variable `my-autoformat-local-functions' to know about autoformat
functions working locally in the buffer and `my-autoformat-global-functions'
to know about autoformat functions working everywhere"
  (interactive)
  (-each my-autoformat-local-functions 'funcall)
  (-each my-autoformat-global-functions 'funcall))

(define-minor-mode my-autoformat-mode
  "Toggle `my-autoformat-mode'.

If the ARG is non-nil, then enable the mode, otherwise disable it.

In this mode each keyboard press calls functions to format a code.  These
functions can be defined either locally (see `my-autoformat-local-functions')
or everywhere (see `my-autoformat-global-functions')."
  :init-value nil
  (if my-autoformat-mode
      (progn
        (my-autoformat-activate-for-major-mode)
        (add-hook 'post-self-insert-hook #'my-autoformat-do nil t))
    (my-autoformat-activate-for-major-mode)
    (remove-hook 'post-self-insert-hook #'my-autoformat-do t)))

(define-globalized-minor-mode
  my-autoformat-global-mode
  my-autoformat-mode
  my-autoformat-turn-on-mode)

(defun my-autoformat-turn-on-mode ()
  "Enable `my-autoformat-mode' locally."
  (my-autoformat-mode t))

(defvar my-autoformat-functions-of-major-modes nil
  "Alist from keys `major-mode' s and values their autoformat functions.")

(defun my-autoformat-activate-for-major-mode (&optional mm)
  "Change the autoformat local functions depends on major mode (MM).

MM defaults to value of the `major-mode'"
  (interactive)
  (or mm (setq mm major-mode))
  (setq-local my-autoformat-local-functions
              (alist-get mm
                         my-autoformat-functions-of-major-modes
                         '(ignore)
                         nil
                         'eq)))

(defun my-autoformat-bind-for-major-mode (mode &rest functions)
  "Bind autoformat FUNCTIONS as local to major-mode MODE."
  (->>
   my-autoformat-functions-of-major-modes
   (assq-delete-all mode)
   (cons (cons mode functions))
   (setq my-autoformat-functions-of-major-modes)))

(my-autoformat-global-mode t)

(provide 'my-autoformat)
;;; my-autoformat.el ends here
