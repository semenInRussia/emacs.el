;;; my-calendar.el --- My configuration of `calendar' -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024 semenInRussia
;; Author: semenInRussia <hrams205@gmail.com>
;; Version: 0.1
;; Homepage: https://github.com/semeninrussia/emacs.el

;;; Commentary:

;; My configuration of `calendar' builtin Emacs.

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
