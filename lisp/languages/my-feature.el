;;; my-feature.el --- My configuration of `feature' -*- lexical-binding: t; -*-

;; Copyright (C) 2022 semenInRussia

;; Author: semenInRussia <hrams205@gmail.com>
;; Version: 0.1
;; Homepage: https://github.com/semeninrussia/emacs.el


;;; Commentary:

;; My configuration of `feature-mode'.

;;; Code:

(require 'my-leaf)


;; NOTE that here support only `ecukes' now.
;; `ecukes' is cucumber for `emacs-lisp-mode'
(leaf feature-mode
  :ensure (feature-mode :repo "michaelklishin/cucumber.el" :host github)
  :hook (feature-mode-hook . my-feature-mode-hook)
  :bind (:feature-mode-map
         ("RET" . newline-and-indent)
         ("M-RET" . my-feature-add-and-statement))
  :config
  (defun my-feature-mode-hook ()
    "My hook of the `feature-mode'."
    (setq-local outline-regexp "\\( *Feature:\\| *Scenario:\\)"))

  (defun my-feature-add-and-statement ()
    "Add an \"And\" feature statement, like to statement at the current line."
    (interactive)
    (end-of-line)
    (insert "\n" (buffer-substring-no-properties (pos-bol) (pos-eol)))
    (forward-line 1)))

(provide 'my-feature)
;;; my-feature.el ends here
