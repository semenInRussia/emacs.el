;;; my-lsp.el --- My choose between LSP clieents -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2025 semenInRussia

;;; Commentary:
;; My choose between LSP clieents.

;;; Code:
(declare-function eglot-ensure "eglot.el")
(declare-function lsp-bridge-mode "lsp-bridge.el")


(defun my-lsp-ensure ()
  "Run chosen LSP client for the current buffer*.

* or add a command to run LSP client to the future calls stacks."
  (interactive)
  ;; run it with idle timer.  It's useful, because in this case Emacs
  ;; don't need to load `eglot' instantly after somebody open a file,
  ;; so file will be opened more quickly and lsp will be activated
  ;; after some time
  (let ((buf (current-buffer)))
    (run-with-idle-timer 1 nil
                         (lambda ()
                           (with-current-buffer buf
                             (eglot-ensure))))))

(provide 'my-lsp)
;;; my-lsp.el ends here
