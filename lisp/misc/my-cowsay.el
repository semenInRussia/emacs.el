;;; my-cowsay.el --- My config of `cowsay'

;; Copyright (C) 2022 semenInRussia
;; Author: semenInRussia <hrams205@gmail.com>
;; URL: https://github.com/semenInRussia/emacs.el

;;; Commentary:

;; My config of `cowsay'

;;; Code:

(require 'my-leaf)
(require 'dash)

(leaf cowsay
  :ensure t
  :defun cowsay--get-default-cow cowsay-load-cows
  :defvar cowsay-cows cowsay-directories
  :custom ((cowsay-directories . `(,(locate-user-emacs-file "cows"))))
  :defer-config (cowsay-load-cows))

(provide 'my-cowsay)
;;; my-cowsay.el ends here
