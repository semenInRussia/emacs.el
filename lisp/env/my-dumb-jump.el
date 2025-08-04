;;; my-dumb-jump.el --- My configuration of `dumb-jump' -*- lexical-binding: t; -*-

;; Copyright (C) 2025 semenInRussia
;; Author: semenInRussia <hrams205@gmail.com>

;;; Commentary:
;; My configuration of `dumb-jump'.  Dumb jump is dumb way to jump to
;; definition, without LSP and other shit

;;; Code:
(require 'my-leaf)

(leaf dumb-jump
  :ensure t
  :hook (xref-backend-functions . dumb-jump-xref-activate))

(provide 'my-dumb-jump)
;;; my-dumb-jump.el ends here
