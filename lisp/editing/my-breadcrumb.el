;;; my-breadcrumb.el --- My configuration of breadcrumb -*- lexical-binding: t; -*-

;; Copyright (C) 2025 semenInRussia
;; Author: semenInRussia <hrams205@gmail.com>

;;; Commentary:

;; My configuration of breadcrumb.  Breadcrumb show the module in which cursor
;; is now and the name function, class and etc.

;;; Code:
(require 'my-leaf)

(leaf breadcrumb
  :ensure t
  :hook (prog-mode-hook . breadcrumb-local-mode))

(provide 'my-breadcrumb)
;;; my-breadcrumb.el ends here
