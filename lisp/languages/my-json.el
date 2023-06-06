;;; my-json.el --- My configuration for json

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

;; My configuration for json

;;; Code:

(require 'my-leaf)
(require 'my-xah)

(leaf json-mode
  :ensure t
  :major-mode-map json
  :bind (:json-mode-map
         ([:remap my-format-expression] . json-pretty-print-buffer))
  :hook (json-mode-hook . my-json-fix-indent-funcs)
  :config                               ;nofmt
  (leaf json-snatcher
    :ensure t
    :bind (:my-json-local-map           ;nofmt
           :package json-mode
           ("c" . jsons-print-path)))

  (defun my-json-fix-indent-funcs ()
    "Fix the functions that changes indent in JSON files."
    (interactive)
    (setq-local js-indent-level 2)))

(provide 'my-json)
;;; my-json.el ends here
