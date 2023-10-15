;;; my-json.el --- My configuration for editing JSON inside Emacs

;; Copyright (C) 2022 Semen Khramtsov

;; Author: Semen Khramtsov <hrams205@gmail.com>
;; Version: 0.1
;; URL: https://github.com/semenInRussia/emacs.el

;;; Commentary:

;; My configuration for editing JSON inside Emacs.

;;; Code:

(require 'my-leaf)


(leaf json-mode
  :ensure (json-mode :repo "joshwnj/json-mode" :host github)
  :bind (:json-mode-map
         ([:remap my-format-expression] . json-pretty-print-buffer))

  :hook (json-mode-hook . my-json-fix-indent-funcs)

  :config
  (leaf json-snatcher
    :ensure (json-snatcher :repo "Sterlingg/json-snatcher" :host github)
    :bind (:json-mode-map
           :package json-mode
           ("C-c M-w" . jsons-print-path)))

  (defun my-json-fix-indent-funcs ()
    "Fix the functions that changes indent in JSON files."
    (interactive)
    (setq-local js-indent-level 2)))

(provide 'my-json)
;;; my-json.el ends here
