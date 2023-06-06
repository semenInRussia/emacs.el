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
(leaf go-translate
  :ensure t
  :custom (gts-translate-list . '(("en" "ru")))
  :defvar gts-default-translator
  :defun (gts-buffer-render gts-translator gts-prompt-picker gts-google-engine)
  :bind (:xah-fly-command-map
         :package xah-fly-keys
         ("SPC i h"     . 'gts-do-translate)
         ("SPC i SPC h" . 'gts-do-translate))
  :config (setq gts-default-translator
                (gts-translator :picker
                                (gts-prompt-picker)
                                :engines  ;nofmt
                                (list (gts-google-engine))
                                :render (gts-buffer-render))))
(provide 'my-go-translate)
;;; my-go-translate.el ends here
