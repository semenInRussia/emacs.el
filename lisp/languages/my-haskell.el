;;; my-haskell.el --- My config for `haskell'

;; Copyright (C) 2022-2023 semenInRussia

;;; Commentary:

;; My config for `haskell'.

;;; Code:
(require 'my-leaf)


(leaf haskell-mode
  :ensure t
  ;; :ensure-system-package (("hoogle" . "cabal install hoogle"))
  :hook ((haskell-mode-hook . haskell-indent-mode)
         (haskell-mode-hook . interactive-haskell-mode))
  :config
  (leaf eglot
    :hook (haskell-mode-hook . my-lsp-ensure)))

(provide 'my-haskell)
;;; my-haskell.el ends here
