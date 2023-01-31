;;; my-js.el --- My configuration for `js'

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

;; My configuration for `js'

;;; Code:

(defun my-enable-flycheck () (flycheck-mode 1))

(leaf js
  :ensure t
  :after lsp-bridge
  :mode "\\.js$"
  :config                               ;nofmt
  (add-to-list 'lsp-bridge-single-lang-server-mode-list
               '(js-mode . "typescript"))
  (leaf web-mode :ensure t)
  (leaf js-comint :ensure t))

(provide 'my-js)
;;; my-js.el ends here
