;;; my-apheleia.el --- My configuration of the `apheleia': auto format of source code after save

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

;; My configuration of the `apheleia'

;;; Code:

(require 'my-leaf)
(require 'f)

(defvar uncrustify-cfg-file (f-full "~/uncrustify.cfg"))


(leaf apheleia
  :ensure (apheleia :repo "radian-software/apheleia"
                    :host github)
  :defvar (apheleia-formatters apheleia-mode-alist)
  :require apheleia-core
  :global-minor-mode apheleia-global-mode
  :defer-config
  (push '(uncrustify
          . ("uncrustify" "-f" filepath "-c" uncrustify-cfg-file "-o"))
        ;; a formatter for C++
        apheleia-formatters)
  (setf (alist-get 'rustfmt apheleia-formatters)
        '("rustfmt" "--edition" "2021" "--quiet" "--emit" "stdout"))
  (setf (alist-get 'c++-mode apheleia-mode-alist)
        'uncrustify)
  (setf (alist-get 'python-mode apheleia-mode-alist)
        '(yapf isort)))

(provide 'my-apheleia)
;;; my-apheleia.el ends here
