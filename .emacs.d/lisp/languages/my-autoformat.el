;;; my-autoformat.el --- My function for `autoformat'

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

;; My function for `autoformat'

;;; Code:
(require 'just)
(require 'org)




(defgroup my-autoformat nil
  "Automatically format of source code (add spaces, capitalze and etc)."
  :group 'editing)

(defvar my-autoformat-local-functions nil
  "Autoformat functions works locally in the buffer.")

(make-local-variable 'my-autoformat-local-functions)

(defcustom my-autoformat-global-functions
  nil
  "Autoformat functions works everywhere."
  :type '(repeat symbol))

(defun autoformat-sentence-capitalization (&optional ;nofmt
                                           prev-line-should-not-be-empty)
  "Auto-capitalize first words of a sentence.

Either at the beginning of a line, or after a sentence end, if
PREV-LINE-SHOULD-NOT-BE-EMPTY is nil (by default), then capitalize only when
previous line is empty."
  (interactive)
  (and
   (looking-back "[[:alpha:]]" nil)
   (just-call-on-backward-char*
    (or
     (bobp)
     (looking-back (sentence-end) nil)
     (and
      (skip-chars-backward " ")
      (bolp)
      (or prev-line-should-not-be-empty (my-previous-line-is-empty)))))
   (upcase-char -1)))

(defun my-previous-line-is-empty ()
  "Move to previous line and return t, when this line is empty.

See `just-line-is-whitespaces-p'"
  (just-call-on-prev-line 'just-line-is-whitespaces-p))

(defun my-autoformat-do ()
  "Funcall each of the local autoformat functions.

See variable `my-autoformat-local-functions'"
  (interactive)
  (-each my-autoformat-local-functions 'funcall))

(define-minor-mode my-autoformat-mode
  "Toggle `my-autoformat-mode'."
  :init-value nil
  (if my-autoformat-mode
      (progn
        (my-autoformat-activate-for-major-mode)
        (add-hook 'post-self-insert-hook #'my-autoformat-do 0 t))
    (my-autoformat-activate-for-major-mode)
    (remove-hook 'post-self-insert-hook #'my-autoformat-do t)))

(define-global-minor-mode my-autoformat-global-mode
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
  "Bind autoformat FUNCTIONS for MODE."
  (->>
   my-autoformat-functions-of-major-modes
   (assq-delete-all mode)
   (cons (cons mode functions))
   (setq my-autoformat-functions-of-major-modes)))

(my-autoformat-global-mode t)

(provide 'my-autoformat)
;;; my-autoformat.el ends here
