;;; my-aas.el --- My configuration of the `auto-activating-snippets'

;; Copyright (C) 2022-2024 semenInRussia

;;; Commentary:

;; My configuration of the `auto-activating-snippets'

;;; Code:
(require 'my-leaf)

(leaf aas
  :ensure t
  ;; don't use global mode, because `aas' is used with me only inside
  ;; a few amount of major modes
  ;;
  ;; :global-minor-mode aas-global-mode
  ;;
  ;; instead do it:
  :hook ((latex-mode-hook . aas-mode)
         (TeX-latex-mode-hook . aas-mode)))

(provide 'my-aas)
;;; my-aas.el ends here
