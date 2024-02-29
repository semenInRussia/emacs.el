;;; my-recentf.el --- My configuration of `recentf' -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024 semenInRussia

;;; Commentary:

;; My configuration of `recentf'.

;;; Code:

(require 'my-leaf)


(defun my-recentf-load ()
  "Just load `recentf'."
  (interactive)
  (unless recentf-mode
    (recentf-mode t)))

(leaf recentf
  ;; :global-minor-mode recentf-mode
  :hook (prog-mode-hook
         text-mode-hook)
  :bind ("C-c r" . recentf)
  :custom (;; by default, `recentf' load the list of recent file at
           ;; startup, it can eat our init time.
           ;;
           ;; NOTE: that because
           ;; auto cleanup is disabled, sometimes you must run
           ;; `recentf-cleanup'
           (recentf-auto-cleanup . 'never))
  :init
  (advice-add 'consult-buffer :before #'my-recentf-load)
  (run-with-idle-timer 3 nil #'my-recentf-load))

(provide 'my-recentf)
;;; my-recentf.el ends here
