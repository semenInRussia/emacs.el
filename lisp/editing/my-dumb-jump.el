;;; my-dumb-jump.el --- My configuration of the `dumb-jump'

;; Copyright (C) 2022-2024 semenInRussia

;;; Commentary:

;; My configuration of the `dumb-jump'

;;; Code:

(require 'my-leaf)

(leaf rg
  :ensure (rg :repo "dajva/rg.el"
              :host github))

(leaf dumb-jump
  :ensure t
  :custom ((dumb-jump-prefer-searcher dumb-jump-force-searcher)
           . 'rg)
  :bind (("M-," . xref-go-back)
         ("M-." . xref-find-definitions))
  :hook (xref-backend-functions . dumb-jump-xref-activate))

(provide 'my-dumb-jump)
;;; my-dumb-jump.el ends here
