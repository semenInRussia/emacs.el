;;; my-which-key.el --- My config for `which-key'
;; Copyright (C) 2022-2023 Semen Khramtsov

;;; Commentary:

;; My config for `which-key'

;;; Code:

(require 'my-leaf)


(leaf which-key
  :ensure t
  :global-minor-mode which-key-mode
  :defun which-key-setup-side-window-bottom
  :config (which-key-setup-side-window-bottom))

(provide 'my-which-key)
;;; my-which-key.el ends here
