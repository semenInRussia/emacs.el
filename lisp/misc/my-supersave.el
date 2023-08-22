;;; my-supersave.el --- My config for `supersave'

;; Copyright (C) 2022-2023 Semen Khramtsov

;; Author: Semen Khramtsov <hrams205@gmail.com>
;; Version: 0.1
;; URL: https://github.com/semenInRussia/emacs.el

;;; Commentary:

;; My config for `supersave': automatically save the current file

;;; Code:

(require 'my-leaf)


(leaf super-save
  :ensure t
  :global-minor-mode super-save-mode
  :init
  (add-hook 'super-save-triggers 'dired-jump)
  (remove-hook 'super-save-hook-triggers 'focus-out-hook))

(provide 'my-supersave)
;;; my-supersave.el ends here
