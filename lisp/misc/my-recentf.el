;;; my-recentf.el --- My configuration of `recentf' -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024 semenInRussia

;;; Commentary:

;; My configuration of `recentf'.

;;; Code:

(require 'my-leaf)


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
  :config
  (advice-add
   'consult-buffer :before
   (defun my-recentf-load ()
     (interactive)
     (unless (boundp 'recentf-list)
       (recentf-mode)))))

(provide 'my-recentf)
;;; my-recentf.el ends here
