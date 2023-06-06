;;; my-goto-line-preview.el --- My configuration of `goto-line-preview' -*- lexical-binding: t; -*-
;; Copyright (C) 2022 semenInRussia
;; Author: semenInRussia <hrams205@gmail.com>
;; Version: 0.1
;; Homepage: https://github.com/semeninrussia/emacs.el
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.
;;; Commentary:
;; My configuration of `goto-line-preview'.  Preiew line before jump to it
;;; Code:
(require 'my-leaf)
(leaf goto-line-preview
  :ensure t
  :bind ([remap goto-line] . goto-line-preview))
(provide 'my-goto-line-preview)
;;; my-goto-line-preview.el ends here
