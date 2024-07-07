;;; my-auto-compile.el --- My configuration for auto compile lisp files of config -*- lexical-binding: t; -*-

;; Copyright (C) 2022 semenInRussia
;; Author: semenInRussia <hrams205@gmail.com>
;; Version: 0.1
;; Homepage: https://github.com/semeninrussia/emacs.el

;;; Commentary:

;; My configuration of `auto-compile'.  Automatically byte-compile Emacs Lisp
;; files after save.

;;; Code:

(require 'f)

(defun my--maybe-compile-cur-buf ()
  "If the opened buffer is an Elisp file and it was be compiled, recompile it."
  (and (buffer-file-name)
       (derived-mode-p 'emacs-lisp-mode)
       (f-exists-p (f-swap-ext (buffer-file-name) "elc"))
       (make-thread #'elisp-byte-compile-file
                    (format "byte-auto-compile-%s"
                            (buffer-file-name)))))

(define-minor-mode my-recompile-mode
  "After each save of an Elisp file that was be compiled, recompile it.

If the ARG is non-nil, then enable the mode, otherwise disable it."
  :init-value t
  :group 'my
  (if my-recompile-mode
      (add-hook 'after-save-hook 'my--maybe-compile-cur-buf t)
    (remove-hook 'after-save-hook 'my--maybe-compile-cur-buf t)))

(add-hook 'emacs-lisp-mode-hook #'my-recompile-mode)

(provide 'my-auto-compile)
;;; my-auto-compile.el ends here
