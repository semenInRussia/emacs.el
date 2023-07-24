;;; my-go-translate.el --- My config `go-translate'

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

;; My config `go-translate'

;;; Code:
(require 'my-leaf)

(require 'dash)


(leaf go-translate
  :ensure t
  :custom (gts-translate-list . '(("en" "ru")))
  :bind (;; so I can just hit C-. or o with T following to translate a thing
         ;; at point
         (:embark-region-map
          :package embark
          ("T" . gts-do-translate)))
  :defvar gts-default-translator
  :defun (gts-buffer-render gts-translator gts-prompt-picker gts-google-engine)
  :fast-exec ("Translate a String" 'gts-do-translate)
  :defer-config
  ;; I use Google Translate with the output in the separate buffer
  (setq gts-default-translator
        (gts-translator :picker
                        (gts-prompt-picker)
                        :engines
                        (list (gts-google-engine))
                        :render (gts-buffer-render))))

(provide 'my-go-translate)
;;; my-go-translate.el ends here
