;;; my-emacsclient.el --- My configuration of `emacsclient' -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024 semenInRussia

;;; Commentary:

;; My configuration of `emacsclient'.

;;; Code:

(declare-function server-running-p "server")

(when (display-graphic-p)
  (add-hook
   'after-init-hook
   (defun my-maybe-server-start ()
     "Run the Emacs server if servers haven't been started."
     (require 'server)
     (unless (server-running-p)
       (server-start)))))

(with-eval-after-load 'server
  (defvar server-mode-map)
  (keymap-set server-mode-map "C-x C-S-c" 'server-force-delete))

(provide 'my-emacsclient)
;;; my-emacsclient.el ends here
