;;; my-calendar.el --- My configuration of `calendar' -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025 semenInRussia

;;; Commentary:

;; My configuration of `calendar' built-in Emacs.

;;; Code:

(require 'my-leaf)


(leaf calendar
  :defvar calendar-holidays
  :bind ("C-c C" . calendar)
  :config (leaf russian-holidays
            :ensure t
            :require t
            :defvar russian-holidays
            :config (setq calendar-holidays russian-holidays)))

(provide 'my-calendar)
;;; my-calendar.el ends here
