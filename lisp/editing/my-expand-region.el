;;; my-expand-region.el --- My configuration of the `expand-region'

;; Copyright (C) 2022 Semen Khramtsov

;; Author: Semen Khramtsov <hrams205@gmail.com>
;; Version: 0.1
;; URL: https://github.com/semenInRussia/emacs.el

;;; Commentary:

;; My configuration of the `expand-region'

;;; Code:

(require 'my-leaf)


(leaf expand-region
  :ensure (expand-region :repo "magnars/expand-region.el" :host github)
  :bind ("C-M-SPC" . er/expand-region))

(provide 'my-expand-region)
;;; my-expand-region.el ends here
