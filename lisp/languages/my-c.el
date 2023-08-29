;;; my-c.el --- My configuration of c and c++ languages -*- lexical-binding: t; -*-

;; Copyright (C) 2023 semenInRussia

;;; Commentary:

;; My configuration of c and c++ languages

;;; Code:
(require 'my-leaf)


(leaf cc-mode
  :setq-default (;; enable auto insert newline after ";"
                 (c-auto-newline . t)
                 (c-electric-flag . t)
                 (c-hungry-delete-key . t))
  :config (leaf google-c-style
            :ensure (google-c-style :repo "google/styleguide" :host github)
            :hook ((c++-mode-hook c-mode-hook)   . google-set-c-style)))

(provide 'my-c)
;;; my-c.el ends here
