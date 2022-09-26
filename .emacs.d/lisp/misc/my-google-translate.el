;;; my-google-translate.el --- my-google-translate

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
(use-package google-translate
    :ensure t
    :custom
    (google-translate-default-source-language "auto")
    (google-translate-default-target-language "ru")
    :config
    (defun google-translate--search-tkk ()
      "Search TKK."
      (list 430675 2721866130))

    (defun fast-exec-google-translate-keys ()
      "Get some useful keymaps of `fast-exec' for google-translate."
      (fast-exec/some-commands
       ("Google Translate" 'google-translate-at-point)
       ("Query to Google Translate" 'google-translate-query-translate-reverse)))

    (fast-exec/register-keymap-func 'fast-exec-google-translate-keys)
    (fast-exec/reload-functions-chain))

(provide 'my-google-translate)
;;; my-google-translate.el ends here