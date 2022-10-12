;;; my-cowsay.el --- My config of `cowsay'

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

;; My config of `cowsay'

;;; Code:
(leaf cowsay
  :ensure t
  :custom (cowsay-directories . '("~/.emacs.d/cows"))
  :config (cowsay-load-cows)
  :fast-exec (("Cow Say String..."  'cowsay-string)
              ("Cow Say Region..."  'cowsay-region)
              ("Cow Say and Insert" 'cowsay-replace-region)
              ("Load Cows"  'cowsay-load-cows))
  :config ; nofmt
  (defun cowsay--prompt-for-cow (&rest _ignored)
    "Read any cow name from the minibuffer."
    (let ((default (cowsay--get-default-cow)))
      (completing-read
       "Cow: "
       cowsay-cows
       nil t
       default
       'cowsay-cow-history
       default))))

(provide 'my-cowsay)
;;; my-cowsay.el ends here
