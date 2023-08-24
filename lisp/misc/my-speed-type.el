;;; my-speed-type.el --- My configuration of `speed-type' -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2023 semenInRussia

;;; Commentary:

;; My configuration of `speed-type'.

;;; Code:

(require 'my-leaf)
(require 'dash)


(leaf speed-type
  :ensure t
  :fast-exec ("Start a Type Test" 'speed-type-text))

(provide 'my-speed-type)
;;; my-speed-type.el ends here
