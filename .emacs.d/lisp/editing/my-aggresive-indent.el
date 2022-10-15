;;; my-aggresive-indent.el --- My configuration for the `aggressive-indent'

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

;; My configuration for the `aggressive-indent'

;;; Code:
(require 'leaf)

(defcustom my-aggresive-indent-hooks
  '(racket-mode-hook
    css-mode-hook
    emacs-lisp-mode-hook
    eshell-mode-hook)
  "List of hook on which should be enabled `aggressive-indent-mode'."
  :type '(repeat symbol)
  :group 'my)

(leaf aggressive-indent
  :ensure t
  :hook `(,my-aggresive-indent-hooks . aggressive-indent-mode))

(provide 'my-aggresive-indent)
;;; my-aggresive-indent.el ends here
