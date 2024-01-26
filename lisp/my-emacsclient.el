;;; my-emacsclient.el --- My configuration of `emacsclient' -*- lexical-binding: t; -*-

;; Copyright (C) 2022 semenInRussia

;;; Commentary:

;; My configuration of `emacsclient'.

;;; Code:

(declare-function server-running-p "server")

(add-hook
 'emacs-startup-hook
 (defun my-maybe-server-start ()
   "Run the Emacs server if servers haven't been started."
   (require 'server)
   (unless (server-running-p)
     (server-start))))

(provide 'my-emacsclient)
;;; my-emacsclient.el ends here
