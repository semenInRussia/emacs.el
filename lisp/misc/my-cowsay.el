;;; my-cowsay.el --- My config of `cowsay'

;; Copyright (C) 2022 semenInRussia
;; Author: semenInRussia <hrams205@gmail.com>
;; Version: 0.1
;; URL: https://github.com/semenInRussia/emacs.el

;;; Commentary:

;; My config of `cowsay'

;;; Code:

(require 'my-leaf)
(require 'dash)

(leaf cowsay
  :ensure t
  :defun cowsay--get-default-cow
  :defvar cowsay-cows
  :custom ((cowsay-directories . `(,(locate-user-emacs-file "cows"))))
  :defer-config (cowsay-load-cows)
  :config
  (defun cowsay--prompt-for-cow (&rest _ignored)
    "Read any cow name from the minibuffer."
    (let ((default (cowsay--get-default-cow)))
      (completing-read
       "Cow: "
       cowsay-cows
       nil t
       default
       'cowsay-cow-history
       default))))

(provide 'my-cowsay)
;;; my-cowsay.el ends here
