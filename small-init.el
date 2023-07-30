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
(add-to-list 'load-path (locate-user-emacs-file "lisp/"))

;; about 21 files (it's a little amount, so start up is faster)
;; here required things (they're required for the rest of config)
(my-load-file (locate-user-emacs-file "lisp/package-management/my-straight.el"))
(my-load-file (locate-user-emacs-file "lisp/package-management/my-leaf.el"))
(my-load-file (locate-user-emacs-file "lisp/my-libs.el"))
(my-load-file (locate-user-emacs-file "lisp/my-lib.el"))
;; highlight errors on the fly
(my-load-file (locate-user-emacs-file "lisp/editing/my-flycheck.el"))
;; snippets
(my-load-file (locate-user-emacs-file "lisp/editing/my-yas.el"))
;; auto-completion popup
(my-load-file (locate-user-emacs-file "lisp/editing/my-corfu.el"))
;; auto-enclosing parentheses
(my-load-file (locate-user-emacs-file "lisp/editing/my-smartparens.el"))
;; some small editing tips
(my-load-file (locate-user-emacs-file "lisp/editing/my-editing.el"))
(my-load-file (locate-user-emacs-file "lisp/editing/my-indent.el"))
(my-load-file (locate-user-emacs-file "lisp/editing/my-search.el"))
;; load fonts
(my-load-file (locate-user-emacs-file "lisp/ui/my-fonts.el"))
;; multiple cursors that bound to "M-,"
(my-load-file (locate-user-emacs-file "lisp/editing/my-multiple-cursors.el"))
;; "M-o" to jump between buffers, also you can manipulate with windows, for
;; example "M-o x <N>" delete the Nth window
(my-load-file (locate-user-emacs-file "lisp/editing/my-buffer-navigation.el"))
;; completion in functions like `switch-to-buffer', `find-file'
(my-load-file (locate-user-emacs-file "lisp/misc/my-vertico.el"))
(my-load-file (locate-user-emacs-file "lisp/misc/my-orderless.el"))
(my-load-file (locate-user-emacs-file "lisp/misc/my-consult.el"))

;; start a server to fast opening files in the same session
(server-start)

(provide 'small-init)
;;; small-init.el ends here
