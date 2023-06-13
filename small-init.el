;;; small-init.el --- configuration file to very small minimal Emacs -*- lexical-binding: t; -*-

;; Copyright (C) 2023 hrams205@gmail.com

;; Author: semenInRussia <hrams@DESKTOP-CQH054L>
;; Version: 0.0.1
;; Package-Requires: ((dash "2.18.0") (s "1.12.0"))
;; Homepage: https://github.com/semenInRussia

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

;; Configuration file to very small minimal Emacs.

;;; Code:

(defun my-load-file (filename)
  "Load Emacs configuration file with a FILENAME.

If a compiled version is exists, load it."
  (add-to-list 'load-path (directory-file-name (file-name-directory filename)))
  (require (intern (file-name-base filename))))

;; add my own Emacs library and third party library to `load-path', so you can use it with `require' before
(add-to-list 'load-path "~/.emacs.d/lisp/")

;; about 21 files (it's a little amount, so start up is faster)
(my-load-file "~/.emacs.d/lisp/package-management/my-straight.el")
(my-load-file "~/.emacs.d/lisp/package-management/my-leaf.el")
(my-load-file "~/.emacs.d/lisp/my-libs.el")
(my-load-file "~/.emacs.d/lisp/my-lib.el")
(my-load-file "~/.emacs.d/lisp/editing/my-flycheck.el")
(my-load-file "~/.emacs.d/lisp/editing/my-yas.el")
(my-load-file "~/.emacs.d/lisp/editing/my-company.el")
(my-load-file "~/.emacs.d/lisp/editing/my-smartparens.el")
(my-load-file "~/.emacs.d/lisp/misc/my-smartkeys.el")
(my-load-file "~/.emacs.d/lisp/editing/my-editing.el")
(my-load-file "~/.emacs.d/lisp/editing/my-indent.el")
(my-load-file "~/.emacs.d/lisp/editing/my-search.el")
(my-load-file "~/.emacs.d/lisp/ui/my-fonts.el")
(my-load-file "~/.emacs.d/lisp/editing/my-multiple-cursors.el")
(my-load-file "~/.emacs.d/lisp/languages/my-lang-utils.el")
(my-load-file "~/.emacs.d/lisp/editing/my-buffer-navigation.el")
(my-load-file "~/.emacs.d/lisp/misc/my-consult.el")
(my-load-file "~/.emacs.d/lisp/ui/my-default.el")

;; start a server to fast opening files in the same session
(server-start)

(provide 'small-init)
;;; small-init.el ends here
