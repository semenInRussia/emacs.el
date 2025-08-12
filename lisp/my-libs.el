;;; my-libs.el --- Some libraries -*- lexical-binding: t -*-

;; Copyright (C) 2022-2025 semenInRussia

;;; Commentary:

;; Some libraries

;;; Code:

(require 'my-leaf)


(eval-and-compile
  ;; `eval-and-compile' installs all libraries in compile-time
  ;; , so "(require \\='dash)" compiles successufelly
  (leaf dash :ensure t)
  (leaf s :ensure t)
  (leaf f :ensure t)

  (leaf just
    :ensure (just :host github :repo "semenInRussia/just.el"))

  (leaf compat
    :ensure t)

  (leaf svg-lib
    :ensure t))

(provide 'my-libs)
;;; my-libs.el ends here
