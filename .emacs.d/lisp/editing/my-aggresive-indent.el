;;; my-aggresive-indent.el --- my-aggresive-indent

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

;;; Code:
(defcustom my-aggresive-indent-in-modes '(racket-mode
                                          css-mode
                                          emacs-lisp-mode
                                          eshell-mode)
  "List of major modes in which should work `aggressive-indent-mode'."
  :type '(repeat symbol))

(use-package aggressive-indent
    :ensure t
    :init
    (--each my-aggresive-indent-in-modes
      (add-hook (->> it
                     (symbol-name)
                     (s-append "-hook")
                     (intern))
                #'aggressive-indent-mode)))

(provide 'my-aggresive-indent)
;;; my-aggresive-indent.el ends here