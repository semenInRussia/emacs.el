;;; my-autosave.el --- Autosave file after delay -*- lexical-binding: t -*-
;; Copyright (C) 2024 semenInRussia

;;; Commentary:

;; Autosave file after delay.
;;
;; Use built-in Emacs `auto-save-visited-mode' which run `save-file'
;; peridotitic after the certain delay

;;; Code:

(require 'my-leaf)

;; (leaf files
;;   :hook ((text-mode-hook prog-mode-hook) . auto-save-visited-mode)
;;   :custom (auto-save-interval . 5)) ;; default is 3

;;; my-autosave.el ends here
(provide 'my-autosave)
