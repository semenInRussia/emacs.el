;;; my-cowsay.el --- My config of `cowsay'

;; Copyright (C) 2022 Semen Khramtsov
;; Author: Semen Khramtsov <hrams205@gmail.com>
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
  :fast-exec (("Cow Say String..."  'cowsay-string)
              ("Cow Say Region..."  'cowsay-region)
              ("Cow Say and Insert" 'cowsay-replace-region)
              ("Load Cows"  'cowsay-load-cows))
  :config (defun cowsay--prompt-for-cow
              (&rest _ignored)
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
