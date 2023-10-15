;;; my-register.el --- Configuration of Emacs registers -*- lexical-binding: t -*-

;; Copyright (C) 2023

;;; Commentary:

;; Configuration of Emacs registers.  I use Emacs registers really often: I use
;; both point, number, file and text registers it's really useful, I set some
;; registers at the startup to some constant values

;;; Code:

(require 'dash)


;; do it, because C-x C-r can conflict with C-x r, when `meow-keypad' is used
(global-set-key (kbd "C-x C-r") nil)

;;; "C-x r j m" to jump to the Messages buffer
(set-register ?m '(buffer . "*Messages*"))

;;; registers to jump to some config directories
;;
;; also I have some bookmarks (also to these directories or files)
(--each '((?L . "lisp/local-projects/")
          (?l . "lisp/")
          (?i . "init.el"))
  (set-register (car it) (cons 'file (locate-user-emacs-file (cdr it)))))

(provide 'my-register)
;;; my-register.el ends here
