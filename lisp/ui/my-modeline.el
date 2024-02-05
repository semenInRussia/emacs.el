;;; my-modeline.el --- My configuration for modeline

;; Copyright (C) 2022 Semen Khramtsov

;; Author: Semen Khramtsov <hrams205@gmail.com>
;; Version: 0.1
;; URL: https://github.com/semenInRussia/emacs.el

;; This file is not part of GNU Emacs.

;;; Commentary:

;; My configuration for modeline

;;; Code:

(require 'my-leaf)


(leaf nerd-icons :ensure t)
(leaf shrink-path :ensure t)


(leaf doom-modeline
  :ensure t
  :custom (;; it looks like more nice
           (doom-modeline-height . 50)
           ;; enconding not useful I think.
           (doom-modeline-buffer-encoding . nil)
           ;; version of env too
           (doom-modeline-env-version . nil)
           ;; don't show directory names in `doom-modeline'
           (doom-modeline-project-detection . 'project)
           (doom-modeline-buffer-file-name-style . 'buffer-name))
  :hook after-init-hook
  :config
  ;; I use Emacs in fullscreen mode, so I don't see time that provided
  ;; by OS, so I need time in modeline.  EMACS IS MY OS!!!
  ;; I need only to time (not date) in 24hour format
  (defvar display-time-format) ;; make compile happy
  (setq display-time-format "%H:%M")
  (display-time-mode 1)

  ;; disable show line and column numbers in modeline, because it only
  ;; take off extra place
  (column-number-mode 0)
  (line-number-mode 0)

  ;; show size of the file.  My Emacs don't show line numbers, but know about
  ;; amount of text in the file is important
  (size-indication-mode t))

(define-minor-mode my-modeline-at-top-mode
  "Place mode-line at the top of the screen."
  :value nil
  (if my-modeline-at-top-mode
      (progn
        (setq-default header-line-format mode-line-format)
        (setq-default mode-line-format nil))
    (setq-default mode-line-format header-line-format)
    (setq header-line-format nil)))

(provide 'my-modeline)
;;; my-modeline.el ends here
