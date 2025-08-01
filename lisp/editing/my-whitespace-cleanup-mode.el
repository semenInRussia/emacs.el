;;; my-whitespace-cleanup-mode.el --- My configuration of `whitespaces-cleanup-mode' -*- lexical-binding: t -*-

;; Copyright (C) 2022-2025 semenInRussia

;;; Commentary:
;; My configuration of `whitespace-cleanup-mode'.

;;; Code:
(require 'my-leaf)

(leaf whitespace
  ;; :ensure t
  :hook (write-file-functions . whitespace-write-file-hook))

(provide 'my-whitespace-cleanup-mode)
;;; my-whitespace-cleanup-mode.el ends here
