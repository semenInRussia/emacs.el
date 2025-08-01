;;; my-css.el --- My configuration for `css'

;; Copyright (C) 2022-2025 semenInRussia

;;; Commentary:

;; My configuration for CSS

;;; Code:

(require 'my-leaf)
(require 'dash)


(leaf css-mode
  :config
  (leaf css-eldoc
    :ensure (css-eldoc :repo "zenozeng/css-eldoc" :host github)
    :hook (((css-mode-hook web-mode-hook)
            . css-eldoc-enable))))

(provide 'my-css)
;;; my-css.el ends here
