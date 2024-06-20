;;; my-elm.el --- My configuration for the elm language -*- lexical-binding: t; -*-

;; Copyright (C) 2023 semenInRussia

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

;; My configuration for the elm language.

;;; Code:

(require 'my-leaf)

(leaf elm-mode
  :ensure t
  :bind (:elm-mode-map
         ([remap my-format-expression] . elm-format))
  :config
  (leaf eglot
    :hook (elm-mode-hook . my-lsp-ensure)))

(provide 'my-elm)
;;; my-elm.el ends here
