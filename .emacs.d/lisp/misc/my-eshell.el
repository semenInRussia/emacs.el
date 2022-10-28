;;; my-eshell.el --- My configuration of `eshell' -*- lexical-binding: t; -*-

;; Copyright (C) 2022 semenInRussia

;; Author: semenInRussia <hrams205@gmail.com>
;; Version: 0.1
;; Homepage: https://github.com/semeninrussia/emacs.el

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; My configuration of `eshell'.

;;; Code:

(leaf eshell
  :ensure t
  :config                               ;nofmt
  (leaf esh-autosuggest
    :ensure t
    :hook eshell-mode-hook
    :bind (:esh-autosuggest-active-map
           ("TAB" . esh-autosuggest-complete-word)
           ("RET" . company-complete-selection)))

  (defcustom my-eshell-commands-using-minibuffer
    '(completion-at-point)
    "List of the `eshell' commands using the minubuffer."
    :type '(repeat symbol)
    :group 'my)

  (--each my-eshell-commands-using-minibuffer
    (advice-add it :after
                (lambda (&rest _) (xah-fly-insert-mode-activate))
                '((name . xah-fly-insert-mode-activate)))))

(provide 'my-eshell)
;;; my-eshell.el ends here