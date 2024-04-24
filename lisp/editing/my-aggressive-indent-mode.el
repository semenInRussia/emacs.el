;;; my-aggressive-indent-mode.el --- My configuration of `aggressive-indent-mode' -*- lexical-binding: t; -*-

;; Copyright (C) 2023 semenInRussia

;; Author: semenInRussia <hrams205@gmail.com>
;; Version: 0.1
;; Homepage: https://github.com/semeninrussia/emacs.el

;;; Commentary:

;; My configuration of `aggressive-indent-mode'.

;;; Code:

(require 'my-leaf)


(leaf aggressive-indent
  :ensure t
  :hook emacs-lisp-mode-hook
  :config
  (advice-add 'indent-region-line-by-line
              :around
              'my-remove-progresses)
  (advice-add 'lisp-indent-region
              :around
              'my-remove-progresses)

  (defun my-remove-progresses (fn &rest r)
    "Remove displaying of the progresses in FN, call it with R args."
    (cl-letf (((symbol-function 'make-progress-reporter) 'ignore)
              ((symbol-function 'progress-reporter-done) 'ignore)
              ((symbol-function 'progress-reporter-force-update) 'ignore)
              ((symbol-function 'dotimes-with-progress-reporter) 'ignore)
              ((symbol-function 'dolist-with-progress-reporter) 'ignore)
              ((symbol-function 'progress-reporter-do-update) 'ignore))
      (apply fn r))))

(provide 'my-aggressive-indent-mode)
;;; my-aggressive-indent-mode.el ends here
