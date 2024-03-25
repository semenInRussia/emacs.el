;;; my-load-theme --- Load the current theme

;; Copyright (C) 2022 Semen Khramtsov

;; Author: Semen Khramtsov <hrams205@gmail.com>
;; Version: 0.1
;; URL: https://github.com/semenInRussia/emacs.el

;; This file is not part of GNU Emacs.

;;; Commentary:

;;; Code:
;; See `doom-themes' (list of themes at starting comments)

;; List of my favorite themes:
;; The best one from `ef-themes':
;; - `ef-cyprus'
;; The favorite `doom' themes:
;; - `doom-dark+'
;; - `doom-one'
;; - `doom-1337'.  The best one, I think
;; - `doom-monokai-classic'.  Cool
;; - `doom-gruvbox' (see also just `gruvbox')
;;
;; Indie:
;; - `gruber-darker'.  Cool, but `org-mode' and `vertico' are bad
;; - `flatland'
;; - `chery-blossom'
;; - `gruvbox'
;; - `solarized'
;;
;; Classic Style:
;; - `doom-earl-grey'
;; - `doom-flatwhite'

(require 'my-leaf)


(setq font-lock-maximum-decoration t
      truncate-partial-width-windows nil)

(leaf doom-themes
  :ensure t
  :custom ((doom-themes-enable-italic . t)
           (doom-themes-padded-modeline . nil)
           (doom-themes-enable-bold . t)))

(leaf gruber-darker-theme
  :ensure t)

(leaf monokai-theme
  :ensure t)

(leaf modus-themes
  :custom ((modus-themes-bold-constructs . t)
           (modus-themes-italic-constructs . t)))

(leaf ef-themes
  :ensure t
  :config (global-hl-line-mode))

(leaf flatland-theme
  :ensure t
  :custom-face ((sml-modeline-end-face . '((t :inherit default :border nil)))))

(leaf cherry-blossom-theme
  :ensure t)

(leaf gruvbox-theme
  :ensure t)

(leaf os1-theme
  :ensure (os1-theme
           :host github
           :repo "sashimacs/os1-theme")
  :custom ((os1-modeline-padding . 8)
           (os1-use-variable-pitch . nil)
           (os1-use-more-italic . t)))

;; just load theme, the respective autloads and settings (see above)
;; will be loaded

(load-theme 'doom-1337 :no-confirm)
(custom-set-faces
 ;; The most important

 ;; selected text with more light bg
 '(region ((t :background "#777")))

 ;; ;; bold keywords
 ;; '(font-lock-keyword-face ((t :innerhit t
 ;;                              :bold t)))

 ;; Selection Popup (a `vertico' package)

 ;; I sometimes use a mouse to choose anything from `vertico' buffer
 ;; (What?), so for me it's important
 '(vertico-mouse ((t :background "#777")))

 ;; Auto-Complete Popup (`corfu' package)

 ;; light yellow border for auto-complete
 ;; make border of auto-completion minibuffer white/black, it looks like nice
 '(corfu-border ((t :background "#f4f4f4")))
 ;; italic name of the item like Function, Module, Method
 '(corfu-annotations ((t :italic t)))
 ;; use more like a string literals colors for current complete item
 '(corfu-current ((t :background "#252526" :bold t)))

 ;; highlight TODO with red background, italic black text
 '(hl-todo ((t :backgorund "#FF5E5E" :foreground "black" :italic t))))

(provide 'my-load-theme)
;;; my-load-theme.el ends here
