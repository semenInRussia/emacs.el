;;; my-racket.el --- My Configuration For The Lanugage `racket'
;; Copyright (C) 2022-2024 semenInRussia
;; Author: semenInRussia <hrams205@gmail.com>

;; This file is not part of GNU Emacs.

;;; Commentary:
;; My Configuration for the Lanugage `racket'

;;; Code:
(require 'dash)
(require 'my-leaf)
(require 'smartparens)

;; `flycheck' is enough slow plus `racket-xp-mode' highlight
;; errors too, so i disable `flycheck' for Racket
(add-hook 'racket-mode-hook #'turn-off-flycheck)
(autoload 'turn-off-flycheck "init.el")
(leaf racket-mode
  :ensure (racket-mode :repo "greghendershott/racket-mode" :host github)
  :hook ((racket-mode-hook . racket-xp-mode))
  :defun racket-xp-rename
  :bind (:racket-xp-mode-map
         ([remap racket-xp-rename] . eglot-rename)
         ([remap my-interactive-eglot-rename] . (lambda (_x) (racket-xp-rename))))
  :custom (racket-xp-mode-hook . nil)
  :config
  (leaf my-racket-funcs
    :bind (:racket-xp-mode-map
           :package racket-mode
           ("M-RET" . my-racket-meta-return))))

(declare-function my-autoformat-bind-for-major-mode "my-autoformat")
(leaf scribble-mode
  :ensure (scribble-mode :repo "emacs-pe/scribble-mode" :host github)
  :config
  (add-hook
   'scribble-mode-hook
   (defun my-autoformat-scribble ()
     "Define `my-autoformat' things for `scribble-mode'."
     (require 'my-autoformat)
     (my-autoformat-bind-for-major-mode 'scribble-mode
                                        'my-autoformat-sentence-capitalization))))

(provide 'my-racket)
;;; my-racket.el ends here
