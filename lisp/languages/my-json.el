;;; my-json.el --- My configuration for editing JSON inside Emacs -*- lexical-binding: t -*-

;; Copyright (C) 2022-2025 semenInRussia

;;; Commentary:

;; My configuration for editing JSON inside Emacs.

;;; Code:

(require 'my-leaf)


(leaf json-mode
  :ensure (json-mode :repo "joshwnj/json-mode" :host github)
  :bind (:json-mode-map
         ([:remap my-format-expression] . json-pretty-print-buffer))
  :setq-default (js-indent-level . 2)
  :config
  (leaf json-snatcher
    :ensure (json-snatcher :repo "Sterlingg/json-snatcher" :host github)
    :bind (:json-mode-map
           :package json-mode
           ("C-c M-w" . jsons-print-path))))

(provide 'my-json)
;;; my-json.el ends here
