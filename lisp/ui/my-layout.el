;;; my-layout.el --- My settings to layout -*- lexical-binding: t; -*-
;; Copyright (C) 2023-2024 semenInRussia
;;; Commentary:

;; My settings to layout: padding, window size

;;; Code:

(setq inhibit-startup-screen t
      inhibit-startup-message t
      inhibit-startup-echo-area-message t
      initial-scratch-message nil
      ;; don't use system things, only Emacs
      use-file-dialog nil
      use-dialog-box nil
      ;; Inhibit resizing frame
      frame-inhibit-implied-resize t
      frame-resize-pixelwise t)

(defcustom my-layout-size '(72 . 30)
  "Cons of width and height of editor window."
  :group 'my
  :type '(cons number number))

(setf
 (alist-get 'width default-frame-alist) (car my-layout-size)
 (alist-get 'height default-frame-alist) (cdr my-layout-size)
 ;;
 (alist-get 'width initial-frame-alist) (car my-layout-size)
 (alist-get 'height initial-frame-alist) (cdr my-layout-size))

(require 'my-leaf)
(setq frame-title-format '(buffer-file-name "%f" ("%b")))

;; paddings
(leaf spacious-padding
  :ensure t
  ;; :global-minor-mode t
  :hook after-init-hook)

;;; my-layout.el ends here
