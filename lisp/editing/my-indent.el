;;; my-indent.el --- My configuration for the indentation -*- lexical-binding: t -*-
;; Copyright (C) 2022-2025 semenInRussia

;;; Commentary:
;; My configuration for the indentation

;;; Code:
(require 'my-leaf)

;; disable tabs, sorry Richard
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(leaf-keys (prog-mode-map ("RET" . newline-and-indent)))

(provide 'my-indent)
;;; my-indent.el ends here
