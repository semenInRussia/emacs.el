;;; my-auto-compile.el --- My configuration for auto compile lisp files of config -*- lexical-binding: t; -*-
;; Copyright (C) 2022 semenInRussia
;; Author: semenInRussia <hrams205@gmail.com>
;; Version: 0.1
;; Homepage: https://github.com/semeninrussia/emacs.el
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.
;;; Commentary:
;; My configuration of `auto-compile'.
;;; Code:
(require 'my-leaf)
(require 'async)
(require 'async-bytecomp)
(require 'f)
(defun my-auto-recompile-current-elisp-file ()
  "If the opened buffer is an Elisp file and it was be compiled, recompile it."
  (and
   (buffer-file-name)
   (eq major-mode 'emacs-lisp-mode)
   (f-exists-p (f-swap-ext (buffer-file-name) "elc"))
   (async-byte-compile-file (buffer-file-name))))
(define-minor-mode my-auto-recompile-current-elisp-file-mode
  "After each save of an Elisp file that was be compiled, recompile it.
If the ARG is non-nil, then enable the mode, otherwise disable it."
  :init-value t
  (if my-auto-recompile-current-elisp-file-mode
      (add-hook 'after-save-hook 'my-auto-recompile-current-elisp-file t)
    (remove-hook 'after-save-hook 'my-auto-recompile-current-elisp-file t)))
(define-global-minor-mode my-global-auto-recompile-current-elisp-file-mode
  my-auto-recompile-current-elisp-file-mode
  (lambda () (my-auto-recompile-current-elisp-file-mode t)))
(my-global-auto-recompile-current-elisp-file-mode t)
(provide 'my-auto-compile)
;;; my-auto-compile.el ends here
