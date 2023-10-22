;;; my-realgud.el --- My configuration of `realgud': debugging -*- lexical-binding: t; -*-

;; Copyright (C) 2023 semenInRussia

;;; Commentary:

;; My configuration of `realgud': debugger for Emacs, it consist of
;; several of debuggers like gdb(C,C++,Zig...) or pdb(python).

;;; Code:

(require 'my-leaf)


;; I satisfied with the default configuration of `realgud'.
;;
;; - To run gdb debugger, call `realgud:gdb' via M-x.
;; - Quit the debugger with S-f5.
;; - Some commands inside the source code buffer:
;;   + n - next,
;;   + c - continue,
(leaf realgud
  :ensure t)

;;; my-realgud.el ends here
