;;; my-dark-theme.el --- Configuration for the editor view (dark theme) -*- lexical-binding: t -*-

;; Copyright (C) 2024 semenInRussia
;; Author: semenInRussia <hrams205@gmail.com>

;;; Commentary:

;; Sometimes I need to dark-theme, then I replace `my-light-theme'
;; with `my-dark-theme' inside `my-load-theme'.  Configuration for the
;; editor view (dark theme)

;;; Code:

;;;###autoload
(defun my-dark-theme ()
  "Load dark theme."
  (interactive)
  (load-theme 'doom-1337 :no-confirm)
  (custom-set-faces
   ;; 'doom-1337
   ;; selected text with more light bg
   '(cursor ((t :background "#fff")))
   '(region ((t :background "#3F3F3F")))
   '(secondary-selection ((t :background "#337")))

   '(embark-target ((t :background "#335")))

   ;; Selection Popup (a `vertico' package)

   ;; I sometimes use a mouse to choose anything from `vertico' buffer
   ;; (What?), so for me it's important
   '(vertico-mouse ((t :background "#777")))

   ;; Auto-Complete Popup (`corfu' package)

   ;; light yellow border for auto-complete make border of
   ;; auto-completion minibuffer white/black, it looks like nice
   '(corfu-border ((t :background "#f4f4f4")))
   ;; italic name of the item like Function, Module, Method
   '(corfu-annotations ((t :italic t)))
   ;; use more like a string literals colors for current complete item
   '(corfu-current ((t :background "#252526" :bold t)))

   ;; highlight current symbol with nice background background
   '(eglot-highlight-symbol-face ((t
                                   ;; :background "#333"
                                   ;; :foreground "#000"
                                   )))

   ;; highlight TODO with red background, italic black text
   '(hl-todo ((t :backgorund "#FF5E5E" :foreground "black" :italic t))))

  (defvar hi-lock-face-defaults)
  (setq hi-lock-face-defaults
        (list "mode-line"
              "hi-green"
              "shr-mark"
              "dired-marked"
              "isearch-fail")))

(provide 'my-dark-theme)
;;; my-dark-theme.el ends here
