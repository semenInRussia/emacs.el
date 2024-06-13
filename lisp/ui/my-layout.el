;;; my-layout.el --- My settings to layout -*- lexical-binding: t; -*-
;; Copyright (C) 2023-2024 semenInRussia
;;; Commentary:

;; My settings to layout: padding, window size

;;; Code:

(setq inhibit-startup-screen t
      inhibit-startup-message t
      inhibit-startup-echo-area-message t
      initial-scratch-message nil
      use-file-dialog nil
      use-dialog-box nil)

(defcustom my-layout-size '(70 . 35)
  "Cons of width and height of editor window."
  :group 'my
  :type '(cons number number))

(dolist (var '(initial-frame-alist
               default-frame-alist))
  (set var
       (append
        (list (cons 'width (car my-layout-size))
              (cons 'height (cdr my-layout-size)))
        (eval var))))

(require 'my-leaf)
(setq frame-title-format '(buffer-file-name "%f" ("%b")))

;; paddings
(leaf spacious-padding
  :ensure t
  :global-minor-mode t)

;;; my-layout.el ends here
