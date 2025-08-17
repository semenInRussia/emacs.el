;;; my-desktop.el --- My configuration of `desktop' -*- lexical-binding: t; -*-

;; Copyright (C) 2025 semenInRussia

;;; Commentary:
;; My configuration of `desktop': open opened buffers in last session, saving
;; clipboard buffer, enabled minor modes, sizes of windows.

;;; Code:
(require 'my-leaf)

(leaf desktop
  :hook (after-init-hook . desktop-save-mode)
  :custom (desktop-save . t)
  :bind ("C-x :" . desktop-read)
  :defvar desktop-dirname desktop-path desktop-globals-to-save
  :config
  ;; add path
  (setq desktop-dirname (locate-user-emacs-file ".desktops"))
  (add-to-list 'desktop-path desktop-dirname)
  (unless (file-exists-p desktop-dirname)
    (make-directory desktop-dirname))

  ;; save kill-ring through sessions
  (add-to-list 'desktop-globals-to-save 'kill-ring))

(provide 'my-desktop)
;;; my-desktop.el ends here
