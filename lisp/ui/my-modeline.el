;;; my-modeline.el --- My configuration for modeline -*- lexical-binding: t -*-
;; Copyright (C) 2022-2025 semenInRussia

;;; Commentary:
;; My configuration for modeline

;;; Code:
(require 'my-leaf)

;; I don't use `doom-modeline-env'
(add-hook 'after-init-hook 'doom-modeline-mode -100)
(defun my-dont-modeline-env (&rest r)
  "Don't load `doom-modeline-env'.

R."
  (interactive "P")
  (if (eq (nth 1 r) 'doom-modeline-env)
      t
    (apply r)))

(advice-add #'require :around #'my-dont-modeline-env)


(leaf doom-modeline
  :ensure t
  :custom (;; it looks like more nice
           (doom-modeline-height . 35)
           ;; just fun
           ;; (mode-line-right-align-edge . 'right-margin)
           ;; encoding not useful I think.
           (doom-modeline-buffer-encoding . nil)
           ;; don't use k8s
           (doom-modeline-k8s-show-namespace . nil)
           ;; don't show Bot, percentages and other
           (doom-modeline-percent-position . nil)
           ;; show count of Errors and Warnings with more simple way
           (doom-modeline-check-simple-format . t)
           ;; modal state (`meow')
           (doom-modeline-modal . nil)
           ;; version of env too
           (doom-modeline-env-version . nil)
           ;; don't show directory names in `doom-modeline'
           ;; (doom-modeline-project-detection . 'project)
           (doom-modeline-buffer-file-name-style . 'buffer-name))
  :config
  ;; I use Emacs in fullscreen mode, so I don't see time that provided
  ;; by OS, so I need time in modeline.  EMACS IS MY OS!!!
  ;; I need only to time (not date) in 24hour format
  (defvar display-time-format) ;; make compile happy
  (setq display-time-format "%H:%M")
  (display-time-mode t)

  ;; disable show line and column numbers in modeline, because it only
  ;; take off extra place
  (column-number-mode 0)
  (line-number-mode 0)

  ;; show size of the file.  My Emacs don't show line numbers, but know about
  ;; amount of text in the file is important
  (size-indication-mode t))

;; (define-minor-mode my-modeline-at-top-mode
;;   "Place mode-line at the top of the screen."
;;   :value nil
;;   (if my-modeline-at-top-mode
;;       (progn
;;         (setq-default header-line-format mode-line-format)
;;         (setq-default mode-line-format nil))
;;     (setq-default mode-line-format header-line-format)
;;     (setq header-line-format nil)))

(provide 'my-modeline)
;;; my-modeline.el ends here
