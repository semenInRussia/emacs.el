;;; my-aas.el --- My configuration of the `auto-activating-snippets'

;; Copyright (C) 2022 semenInRussia

;; Author: semenInRussia <hrams205@gmail.com>
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
         (TeX-latex-mode-hook . aas-mode)
         (org-mode-hook . aas-mode)))

(provide 'my-aas)
;;; my-aas.el ends here
