;;; my-autoinsert.el --- My configuration of `autoinsert': automatically insert any initial text into empty files -*- lexical-binding: t; -*-

;; Copyright (C) 2023 semenInRussia
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

;; My configuration of `autoinsert'.

;;; Code:

(require 'my-leaf)


(leaf autoinsert
  :custom ((auto-insert-alist .
                              '((c++-mode .
                                          (nil
                                           "// Copyright "
                                           (my-current-year)
                                           " semenInRussia"
                                           _)))))
  :global-minor-mode auto-insert-mode)
(provide 'my-autoinsert)
;;; my-autoinsert.el ends here
