;;; my-haskell.el --- My config for `haskell'

;; Copyright (C) 2022-2023 Semen Khramtsov

;;; Commentary:

;; My config for `haskell'.

;;; Code:
(require 'my-leaf)


(defvar flymake-allowed-file-name-masks)

(leaf haskell-mode
  :ensure (haskell-mode :repo "haskell/haskell-mode" :host github)
  :defvar flymake-allowed-file-name-masks
  ;; :ensure-system-package (("hoogle" . "cabal install hoogle"))
  :hook ((haskell-mode-hook . haskell-indent-mode)
         (haskell-mode-hook . interactive-haskell-mode)
         (haskell-mode-hook . my-lsp-ensure)))

(provide 'my-haskell)
;;; my-haskell.el ends here
