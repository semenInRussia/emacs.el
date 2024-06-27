;;; my-whitespace-cleanup-mode.el --- My configuration of `whitespaces-cleanup-mode' -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024 semenInRussia
;; Author: semenInRussia <hrams205@gmail.com>

;; My configuration of `whitespace-cleanup-mode'.

;;; Code:

(require 'my-leaf)

(leaf whitespace-cleanup-mode
  ;; :ensure t
  :hook (write-file-functions . delete-trailing-whitespace))

(provide 'my-whitespace-cleanup-mode)
;;; my-whitespace-cleanup-mode.el ends here
