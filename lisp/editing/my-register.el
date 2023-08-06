;;; my-register.el --- Configuration of Emacs registers -*- lexical-binding: t -*-

;; Copyright (C) 2023

;;; Commentary:

;; Configuration of Emacs registers.  I use Emacs registers really often: I use
;; both point, number, file and text registers it's really useful, I set some
;; registers at the startup to some constant values

;;; Code:

;; do it, because C-x C-r can conflict with C-x r, when `meow-keypad' is used
(global-set-key (kbd "C-x C-r") nil)

;;; "C-x r j m" to jump to the Messages buffer
(set-register ?m '(buffer . "*Messages*"))

;;; registers to jump to some config directories
;;
;; also I have some bookmarks (also to these directories or files)
(set-register ?L '(file . "~/.emacs.d/lisp/local-projects/"))
(set-register ?l '(file . "~/.emacs.d/lisp/"))
(set-register ?i '(file . "~/.emacs.d/init.el"))

(provide 'my-register)
;;; my-register.el ends here
