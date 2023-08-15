;;; small-init.el --- configuration file to very small minimal Emacs -*- lexical-binding: t; -*-

;; Copyright (C) 2023 hrams205@gmail.com

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

;; it add ability to load installed 3-rd party packages
(declare-function pam-activate "pam")
(my-load-file (locate-user-emacs-file "lisp/package-management/pam.el"))
(pam-activate)
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
(my-load-file (locate-user-emacs-file "lisp/misc/my-embark.el"))
(my-load-file (locate-user-emacs-file "lisp/misc/my-consult.el"))

;; start a server to fast opening files in the same session
(server-start)

(provide 'small-init)
;;; small-init.el ends here
