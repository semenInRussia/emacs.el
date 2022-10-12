;;; my-haskell.el --- My config for `haskell'

;; Copyright (C) 2022 Semen Khramtsov

;; Author: Semen Khramtsov <hrams205@gmail.com>
;; Version: 0.1
;; URL: https://github.com/semenInRussia/emacs.el

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; My config for `haskell'.  Heavily inspired with `emacs-haskell-tutorial'
;; https://github.com/serras/emacs-haskell-tutorial/

;;; Code:
(leaf haskell-mode
  :ensure t
  :major-mode-map (haskell (haskell-mode haskell-interactive-mode))
  :ensure-system-package (("hoogle" . "cabal install hoogle"))
  :hook ((haskell-mode-hook . haskell-indent-mode)
         (haskell-mode-hook . interactive-haskell-mode))
  :bind (:my-haskell-local-map          ;nofmt
         ("e" . haskell-compile)
         ("i" . haskell-add-import)
         ("g" . haskell-process-load)
         ("p" . haskell-process-do-type))
  :config                               ;nofmt
  (leaf company-ghci
    :ensure t
    :push ((company-backends . 'company-ghci))
    :custom (company-ghc-show-info . t))

  (leaf hindent
    :ensure t
    :ensure-system-package (hindent . "cabal install hindent")
    :bind (:xah-fly-command-map
           :package xah-fly-keys
           ("SPC SPC q" . hindent-reformat-decl-or-fill)))

  (leaf hlint-refactor
    :ensure t
    :ensure-system-package ((hlint    . "cabal install hlint")
                            (refactor . "cabal install apply-refact"))
    :bind (:my-haskell-local-map
           :package haskell-mode
           ("l" . hlint-refactor-refactor-at-point)))

  (leaf hare
    :ensure-system-package (ghc-hare . "cabal install HaRe")
    :hook (haskell-mode-hook . hare-init))

  (leaf shm
    :ensure t
    :ensure-system-package (shm . "cabal install structured-haskell-mode")
    :hook (haskell-mode-hook . structured-haskell-mode))

  (leaf format-all
    :ensure-system-package (stylish-haskell . "cabal install stylish-haskell")))

(provide 'my-haskell)
;;; my-haskell.el ends here
