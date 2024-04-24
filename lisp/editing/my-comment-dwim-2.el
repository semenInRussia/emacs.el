;;; my-comment-dwim-2.el --- My configuration for the `comment-dwim-2'

;; Copyright (C) 2022-2024 semenInRussia

;; Author: semenInRussia <hrams205@gmail.com>
;; Version: 0.1
;; URL: https://github.com/semenInRussia/emacs.el

;;; Commentary:

;; My configuration for the `comment-dwim-2'

;;; Code:
(require 'my-leaf)

(leaf comment-dwim-2
  :ensure (comment-dwim-2 :repo "remyferre/comment-dwim-2"
                          :host github)
  :bind ("M-;" . comment-dwim-2))

(provide 'my-comment-dwim-2)
;;; my-comment-dwim-2.el ends here
