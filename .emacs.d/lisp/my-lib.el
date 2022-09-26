;;; my-lib.el --- my-lib

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

;;; Code:
(defun my-call-interactivelly-or-funcall (symbol)
  "If SYBMOL function is command, `call-interactively', otherwise `funcall'."
  (if (commandp symbol)
      (call-interactively symbol)
    (funcall symbol)))

(defmacro define-key-when (fun-name map key def pred)
  "Define to KEY in MAP DEF when PRED return t or run old command.

Instead of KEY will command FUN-NAME"
  (declare (indent 0))
  (let ((old-def (lookup-key (eval map) key)))
    (unless (eq old-def fun-name)
      `(progn
         (defun ,fun-name ()
           ,(s-lex-format "Run `${old-def}' or `${def}'.")
           (interactive)
           (if (my-call-interactivelly-or-funcall ,pred)
               (my-call-interactivelly-or-funcall ,def)
             (my-call-interactivelly-or-funcall ',old-def)))
         (define-key ,map ,key #',fun-name)))))

(defun repeat-at-last-keystroke ()
  "Define in the tempory keymap at last pressed keystroke `this-command'."
  (one-shot-keybinding
   (char-to-string (event-basic-type last-input-event))
   this-command))

(defun one-shot-keybinding (key command)
  (set-temporary-overlay-map
   (let ((map (make-sparse-keymap)))
     (define-key map (kbd key) command)
     map)
   t))

(provide 'my-lib)
;;; my-lib.el ends here
