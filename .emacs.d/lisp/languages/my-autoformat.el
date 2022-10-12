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
(defvar my-autoformat-functions nil
  "Current used autoformat functions.")

(defcustom my-autoformat-all-functions
  '(sentence-capitalization)
  "All my autoformat functions."
  :type '(repeat symbol))

(defun my-use-autoformat-function-p (f)
  "Return t, when must use F as autoformat function."
  (-contains-p my-autoformat-functions f))

(defmacro my-use-autoformat-in-mode (mode &rest autoformat-functions)
  "Add hook to MODE, which enable AUTOFORMAT-FUNCTIONS."
  (let* ((hook
          (intern (s-append "-hook" (symbol-name (eval mode)))))
         (fun-name
          (->>
           mode
           (eval)
           (symbol-name)
           (s-prepend "my-autoformat-set-functions-for-")
           (intern)))
         (funcs
          (--map
           (intern (s-prepend "autoformat-" (symbol-name it)))
           autoformat-functions)))
    `(progn
       (defun ,fun-name ()
         "Add autoformat special functions for mode."
         (interactive)
         (setq-local my-autoformat-functions ',funcs))
       (add-hook ',hook ',fun-name))))

(defmacro my-also-use-autoformat-in-mode (mode &rest autoformat-functions)
  "Add hook to MODE, which enable AUTOFORMAT-FUNCTIONS plus default functions."
  `(my-use-autoformat-in-mode ,mode
                              ,@(-concat autoformat-functions
                                         my-autoformat-all-functions)))

(defmacro my-use-all-autoformat-in-mode (mode)
  "Use my all autoformat functions in MODE."
  `(my-use-autoformat-in-mode ,mode ,@my-autoformat-all-functions))

(defun autoformat-sentence-capitalization ()
  "Auto-capitalize first words of a sentence.
Either at the beginning of a line, or after a sentence end."
  (interactive)
  (when (and
         (my-in-text-p)
         (looking-back "[а-яa-z]")
         (save-excursion
           (forward-char -1)
           (or
            (bobp)
            (looking-back (sentence-end))
            (and
             (skip-chars-backward " ")
             (bolp)
             (my-previous-line-is-empty))
            (and
             (skip-chars-backward " ")
             (< (skip-chars-backward "*") 0)
             (bolp)))))
    (undo-boundary)
    (capitalize-word -1)))

(defun my-previous-line-is-empty ()
  "Move to previous line and return t, when this line is empty.
See `just-line-is-whitespaces-p'"
  (just-call-on-prev-line 'just-line-is-whitespaces-p))

(defun my-in-text-p ()
  "Return t, when cursor has position on common text."
  (and
   (not (org-in-src-block-p))
   (not (texmathp))))

(defun my-autoformat ()
  "Call all autoformat functions."
  (interactive)
  (--each my-autoformat-functions (funcall it)))

(define-minor-mode my-autoformat-mode
  "Toggle `my-autoformat-mode'."
  :init-value nil
  (if my-autoformat-mode
      (add-hook 'post-self-insert-hook #'my-autoformat)
    (remove-hook 'post-self-insert-hook #'my-autoformat)))

(my-autoformat-mode t)

(provide 'my-autoformat)
;;; my-autoformat.el ends here
