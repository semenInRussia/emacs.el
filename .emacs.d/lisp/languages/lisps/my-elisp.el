;;; my-elisp.el --- My configuration of the elisp

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

;; My configuration of the `emacs-lisp-mode'

;;; Code:
(use-package elisp-mode
    :config                             ; nofmt
  (my-define-local-major-mode-map 'elisp
                                  '(emacs-lisp-mode inferior-emacs-lisp-mode)
                                  my-lisp-map)
  :hook (emacs-lisp-mode . paxedit-mode))

(use-package package-lint :ensure t)

(use-package flycheck-package
    :ensure t
    :init (flycheck-package-setup))

(use-package emr
    :ensure t
    :bind ((:map xah-fly-command-map)
           ("SPC /" . emr-show-refactor-menu)))

(use-package elfmt
    :load-path "site-lisp"
    :config (elfmt-global-mode 1))

(use-package suggest :ensure t)

(use-package lisp-mode :custom (lisp-body-indent 2))

(defun my-goto-defclass-beg ()
  "Goto backward defclass."
  (search-backward-regexp "(\\W*defclass" nil t)
  (skip-chars-forward "("))

(defun my-goto-fields-defclass-defnition ()
  "Go to fields of `defclass' defnition."
  (interactive)
  (my-goto-defclass-beg)
  (forward-sexp 4)
  (forward-char -1)
  (-when-let
      (sexp (sp-get-enclosing-sexp))
    (sp-get sexp (goto-char :end-in))))

(defun my-elisp-in-defclass-p (&optional pt)
  "Move to PT and return name of function/macros in which stay this sexp."
  (setq pt (or pt (point)))
  (save-excursion
    (goto-char pt)
    (when (my-goto-defclass-beg)
      (-when-let
          (sexp (sp-get-enclosing-sexp))
        (sp-get sexp (< :beg pt :end))))))

(defun my-elisp-defclass-name ()
  "Return name of `defclass' defnition."
  (interactive)
  (save-excursion
    (my-goto-defclass-beg)
    (forward-sexp 1)
    (forward-char 1)
    (sexp-at-point)))

(defun my-elisp-new-field-of-class ()
  "Insert new field of Lisp class.
Only when in class defnition."
  (interactive)
  (when (my-elisp-in-defclass-p)
    (my-goto-fields-defclass-defnition)
    (unless (just-line-is-whitespaces-p) (newline-and-indent))
    (yas-expand-snippet
     (format
      "(${1:name} :initarg :$1 :accessor %s-$1)"
      (my-elisp-defclass-name)))))

(define-key emacs-lisp-mode-map
    (kbd "M-RET")
  'my-elisp-new-field-of-class)

(provide 'my-elisp)
;;; my-elisp.el ends here
