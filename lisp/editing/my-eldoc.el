;;; my-eldoc.el --- My configuration of the `eldoc'

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

;; My configuration of the `eldoc'

;;; Code:

(require 'my-leaf)

(leaf eldoc                             ;elfmt
  :ensure t
  :custom (eldoc-idle-delay . 0.01)
  :config (leaf eldoc-box
            :ensure t
            :defun eldoc-box-hover-mode
            :defvar global-eldoc-box-hover-mode
            :init (define-global-minor-mode global-eldoc-box-hover-mode
                    eldoc-box-hover-mode
                    (lambda () (eldoc-box-hover-mode 1)))
            :global-minor-mode global-eldoc-box-hover-mode))

(provide 'my-eldoc)
;;; my-eldoc.el ends here
