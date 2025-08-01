;;; my-save-place-mode.el --- My configuration of `save-place-mode' -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025 semenInRussia

;;; Commentary:

;; My configuration of `save-place-mode'.  Just jump to saved position
;; when file is opened.  NOTE that it's a builtin power of Emacs

;;; Code:

(require 'my-leaf)

(leaf save-place-mode
  :global-minor-mode t)

(provide 'my-save-place-mode)
;;; my-save-place-mode.el ends here
