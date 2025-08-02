;;; my-open-junk-file.el --- My config to `open-junk-file'

;; Copyright (C) 2022-2025 semenInRussia

;;; Commentary:

;; My config for `open-junk-file'.

;;; Code:

(require 'my-leaf)


(leaf open-junk-file
  :ensure t
  :bind ("C-c t" . 'open-junk-file))

(provide 'my-open-junk-file)
;;; my-open-junk-file.el ends here
