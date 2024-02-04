;;; my-layout.el --- My settings to layout: paddings -*- lexical-binding: t; -*-

;; Copyright (C) 2023 semenInRussia

;; Author: semenInRussia <hrams205@gmail.com>
;; Version: 0.1
;; Homepage: https://github.com/semeninrussia/emacs.el

;;; Commentary:

;; My settings to layout: paddings

;;; Code:

(setq inhibit-startup-screen t
      inhibit-startup-message t
      inhibit-startup-echo-area-message t
      initial-scratch-message nil)

(defcustom my-layout-size '(80 . 20)
  "Cons of width and height of editor window."
  :group 'my
  :type '(cons number number))

(setq frame-inhibit-implied-resize t)
(setq initial-frame-alist
      (append
       (list (cons 'width (car my-layout-size))
             (cons 'width (cdr my-layout-size)))
       initial-frame-alist))

(require 'my-leaf)

(setq frame-title-format '(buffer-file-name "%f" ("%b")))

;; paddings
(leaf spacious-padding
  :ensure t
  :global-minor-mode spacious-padding-mode)

;;; my-layout.el ends here
