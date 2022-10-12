;;; my-aggressive-fill-paragraph.el --- My config for `aggressive-fill-paragraph'

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

;; My config for `aggressive-fill-paragraph'

;;; Code:
(require 'dash)
(require 'my-lib)

(defcustom my-aggresive-fill-paragraph-modes
  '(org-mode markdown-mode)
  "List of major modes in which `aggressive-fill-paragraph' should work."
  :type '(repeat symbol)
  :group 'my)

(leaf aggressive-fill-paragraph
  :ensure t
  :config (--each my-aggresive-fill-paragraph-modes
            (add-hook
             (my-major-mode-to-hook it)
             #'aggressive-fill-paragraph-mode)))

(provide 'my-aggressive-fill-paragraph)
;;; my-aggressive-fill-paragraph.el ends here
