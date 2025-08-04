;;; my-light-theme.el --- Configuration for the editor view (light theme) -*- lexical-binding: t -*-

;; Copyright (C) 2024-2025 semenInRussia
;; Author: semenInRussia <hrams205@gmail.com>

;;; Commentary:

;; Sometimes I need to light-theme, then I replace `my-dark-theme'
;; with `my-light-theme' inside `my-load-theme'.  Configuration for
;; the editor view (light theme)

;;; Code:

;;;###autoload
(defun my-light-theme ()
  "Load light theme."
  (interactive)
  ;; acario-light
  ;; bluloco-light
  ;; solorized-light
  (load-theme 'doom-bluloco-light :no-confirm)
  (custom-theme-set-faces 'doom-bluloco-light
   ;; selected text with more light bg
   '(region ((t :background "#ddf")))

   ;; highlight the current line with a light red color
   '(hl-line ((t :background "#eef")))

   '(secondary-selection ((t :background "#ccf")))

   ;; Selection Popup (a `vertico' package)
   ;; I sometimes use a mouse to choose anything from `vertico' buffer
   ;; (What?), so for me it's important
   '(vertico-mouse ((t :background "#aaf")))

   ;; Auto-Complete Popup (`corfu' package)

   ;; light yellow border for auto-complete
   ;; make border of auto-completion minibuffer white/black, it looks like nice
   '(corfu-border ((t :background "#000")))
   ;; italic name of the item like Function, Module, Method
   '(corfu-annotations ((t :italic t)))
   ;; use more like a string literals colors for current complete item
   '(corfu-current ((t :bold t
                       :foreground "#000"
                       :background unspecified)))

   ;; highlight current symbol with nice background background
   '(eglot-highlight-symbol-face ((t :background "#eee"
                                     :bold t)))

   ;; highlight TODO with red background, italic black text
   '(hl-todo ((t :backgorund "#FF5E5E" :foreground "black" :italic t)))))

(provide 'my-light-theme)
;;; my-light-theme.el ends here
