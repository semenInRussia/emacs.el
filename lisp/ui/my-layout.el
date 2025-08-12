;;; my-layout.el --- My settings to layout -*- lexical-binding: t; -*-
;; Copyright (C) 2023-2025 semenInRussia
;;; Commentary:

;; My settings to layout: padding, window size

;;; Code:

(setq inhibit-startup-screen t
      inhibit-startup-message t
      inhibit-startup-echo-area-message t
      initial-scratch-message nil
      ;; don't use system things, only Emacs
      use-file-dialog nil
      use-dialog-box nil)

(defcustom my-layout-size '(85 . 23)
  "Cons of width and height of editor window.

These values added as width and height in `default-frame-alist' at Emacs start
up.  Height is the amount of rows at the screen, width is the amount of
characters viewed on one line"
  :group 'my
  :type '(cons number number))

(setf
 ;; Don't resize the frames in steps; it looks weird, especially in tiling window
 ;; managers, where it can leave unseemly gaps.
 frame-resize-pixelwise t
 ;; Inhibit resizing frame
 frame-inhibit-implied-resize t
 ;; But do not resize windows pixelwise, this can cause crashes in some cases
 ;; when resizing too many windows at once or rapidly.
 window-resize-pixelwise nil
 (alist-get 'width default-frame-alist) (car my-layout-size)
 (alist-get 'height default-frame-alist) (cdr my-layout-size)
 (alist-get 'width initial-frame-alist) (car my-layout-size)
 (alist-get 'height initial-frame-alist) (cdr my-layout-size)

 ;; don't use the system title bar
 frame-title-format '(buffer-file-name "%f" ("%b"))
 ;; (alist-get 'undecorated default-frame-alist) t
 (alist-get 'drag-internal-border default-frame-alist) 1
 (alist-get 'internal-border-width default-frame-alist) 5)

(setq-default line-spacing 0.3)

(require 'my-leaf)

(leaf spacious-padding
  :ensure t
  :when (display-graphic-p)
  ;; :global-minor-mode t
  :hook after-init-hook)

;;; my-layout.el ends here
