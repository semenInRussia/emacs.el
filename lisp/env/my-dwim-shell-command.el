;;; my-dwim-shell-command.el --- My configuration of `dwim-shell-command' -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2023 semenInRussia

;;; Commentary:

;; My configuration of `dwim-shell-command'.

;;; Code:

(require 'my-leaf)


(leaf dwim-shell-command
  :ensure (dwim-shell-command :repo "xenodium/dwim-shell-command" :host github)
  :bind ("C-M-!" . dwim-shell-command))

(provide 'my-dwim-shell-command)
;;; my-dwim-shell-command.el ends here
