;;; my-desktop.el --- My configuration of `desktop' -*- lexical-binding: t; -*-

;; Copyright (C) 2025 semenInRussia
;; Author: semenInRussia <hrams205@gmail.com>

;;; Commentary:
;; My configuration of `desktop': open opened buffers in last session.

;;; Code:
(require 'my-leaf)

(leaf desktop
  :hook (after-init-hook . desktop-save-mode)
  :custom `((desktop-dirname . ,(locate-user-emacs-file ".desktops")))
  :bind ("C-x :" . desktop-read)
  :config
  (unless (file-exists-p desktop-dirname)
    (make-directory desktop-dirname))
  (add-to-list 'desktop-path desktop-dirname))

(provide 'my-desktop)
;;; my-desktop.el ends here
