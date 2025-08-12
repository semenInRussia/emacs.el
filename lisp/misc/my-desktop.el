;;; my-desktop.el --- My configuration of `desktop' -*- lexical-binding: t; -*-

;; Copyright (C) 2025 semenInRussia

;;; Commentary:
;; My configuration of `desktop': open opened buffers in last session.

;;; Code:
(require 'my-leaf)

(leaf desktop
  :hook (after-init-hook . desktop-save-mode)
  :custom `((desktop-dirname . ,(locate-user-emacs-file ".desktops"))
            (desktop-save . t))
  :bind ("C-x :" . desktop-read)
  :config
  (unless (file-exists-p desktop-dirname)
    (make-directory desktop-dirname))
  (add-to-list 'desktop-path desktop-dirname)
  ;; save kill-ring through sessions
  (add-to-list 'desktop-globals-to-save 'kill-ring))

(provide 'my-desktop)
;;; my-desktop.el ends here
