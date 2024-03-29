;;; my-js.el --- My configuration for JavaScript and TypeScript

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
;; along with this program.  If not, see <http://www.gnu.org/licenses/>

;;; Commentary:

;; My configuration for JavaScript and TypeScript

;;; Code:

(require 'my-leaf)

(leaf js
  :defvar lsp-bridge-single-lang-server-mode-list
  :hook (js-mode-hook . my-lsp-ensure)
  :defvar lsp-bridge-multi-lang-server-mode-list
  :mode "\\.js$"
  :config
  (if (not (require 'lsp-bridge nil :noerror))
      (ignore-errors
        (user-error "`lsp-bridge' didn't installed!!!  LSPs for JS can't work"))
    (add-to-list 'lsp-bridge-multi-lang-server-mode-list
                 '((typescript-mode js-mode)
                   . "typescript_rome")))
  (leaf js-comint
    :ensure (js-comint :repo "redguardtoo/js-comint" :host github)))

(leaf typescript-mode
  :ensure (typescript-mode :repo "emacs-typescript/typescript.el" :host github)
  :hook (typescript-mode-hook . my-lsp-ensure)
  :custom (typescript-indent-level . 2)
  :config
  (if (not (require 'lsp-bridge nil :noerror))
      (user-error "`lsp-bridge' didn't installed!!!  LSPs for JS can't work")
    (add-to-list 'lsp-bridge-multi-lang-server-mode-list
                 '((typescript-mode js-mode)
                   . "typescript_rome"))))

(provide 'my-js)
;;; my-js.el ends here
