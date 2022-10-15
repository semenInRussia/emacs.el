;;; my-google-translate.el --- My config `google-translate'

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

;; My config `google-translate'

;;; Code:
(leaf google-translate
  :ensure t
  :custom ((google-translate-default-source-language . "auto")
           (google-translate-default-target-language . "ru"))

  :config                               ;nofmt
  (leaf google-translate-default-ui
    :fast-exec (("Google Translate" 'google-translate-at-point)
                ("Query to Google Translate"
                 'google-translate-query-translate-reverse))
    :defer-config                         ;nofmt
    (defun google-translate--search-tkk ()
      ;; a issue from the original
      ;; GitHub repo
      "Search TKK."
      (list 430675 2721866130))))

(provide 'my-google-translate)
;;; my-google-translate.el ends here
