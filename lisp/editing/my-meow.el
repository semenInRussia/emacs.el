;;; my-meow.el --- My configuration of `meow' -*- lexical-binding: t; -*-

;; Copyright (C) 2023 semenInRussia

;; Author: semenInRussia <hrams205@gmail.com>
;; Version: 0.1
;; Homepage: https://github.com/semeninrussia/emacs.el

;;; Commentary:

;; My configuration of `meow'.  `meow' is a modal editing mode for
;; Emacs.  It was inspired by kakoune and has Helix-like key bindings.
;; I don't love virgin `meow' (without any configs), because every
;; command is "hardcoded" with contributers.  For example the
;; keybindings "o" and "O" is hardcoded with Lisp expresion and jump
;; only around round parentheses, but can also around quotes,
;; double-quotes, symbols, i think that use `forward-sexp',
;; `backward-sexp' and `mark-sexp' is the better choice.  So i try to
;; move on `boon': also modal editing mode for Emacs that was created
;; 9 years ago, while `meow' only 3 and has by 3 times lesser stars on
;; GitHub.  I try to fight with it using my own structural state (see
;; `my-structural')

;;; Code:

(require 'my-leaf)


(leaf meow
  :ensure (meow :repo "meow-edit/meow" :host github)
  :require t
  :defvar (meow-cheatsheet-layout
           meow-cheatsheet-layout-qwerty
           meow-replace-state-name-list)
  :defun (my-meow-setup . my-meow)
  :defun (meow-global-mode
          meow-motion-overwrite-define-key
          meow-motion-overwrite-define-key
          meow-leader-define-key
          meow-normal-define-key
          meow-leader-define-keys
          meow-leader-define-state)
  :custom (meow-use-clipboard . t)
  :config
  (defun my-meow-setup ()
    (setq meow-cheatsheet-layout meow-cheatsheet-layout-qwerty)
    (meow-motion-overwrite-define-key
     '("j" . meow-next)
     '("k" . meow-prev)
     '("<escape>" . ignore))
    (meow-leader-define-key
     ;; SPC j/k will run the original command in MOTION state.
     '("j" . "H-j")
     '("k" . "H-k")
     ;; Use SPC (0-9) for digit arguments.
     '("1" . meow-digit-argument)
     '("2" . meow-digit-argument)
     '("3" . meow-digit-argument)
     '("4" . meow-digit-argument)
     '("5" . meow-digit-argument)
     '("6" . meow-digit-argument)
     '("7" . meow-digit-argument)
     '("8" . meow-digit-argument)
     '("9" . meow-digit-argument)
     '("0" . meow-digit-argument)
     '("/" . meow-keypad-describe-key)
     '("?" . meow-cheatsheet))
    (meow-normal-define-key
     '("0" . meow-expand-0)
     '("9" . meow-expand-9)
     '("8" . meow-expand-8)
     '("7" . meow-expand-7)
     '("6" . meow-expand-6)
     '("5" . meow-expand-5)
     '("4" . meow-expand-4)
     '("3" . meow-expand-3)
     '("2" . meow-expand-2)
     '("1" . meow-expand-1)
     '("-" . negative-argument)
     '(";" . meow-reverse)
     '("," . meow-inner-of-thing)
     '("." . meow-bounds-of-thing)
     '("<" . previous-error)
     '(">" . next-error)
     '("[" . meow-beginning-of-thing)
     '("]" . meow-end-of-thing)
     '("a" . meow-append)
     '("A" . meow-open-below)
     '("b" . meow-back-word)
     '("B" . meow-back-symbol)
     '("c" . meow-change)
     '("d" . meow-delete)
     '("D" . meow-backward-delete)
     '("e" . meow-next-word)
     '("E" . meow-next-symbol)
     '("f" . meow-find)
     '("g" . meow-cancel-selection)
     '("G" . meow-grab)
     '("h" . meow-left)
     '("H" . meow-left-expand)
     '("i" . meow-insert)
     '("I" . meow-open-above)
     '("j" . meow-next)
     '("J" . meow-next-expand)
     '("k" . meow-prev)
     '("K" . meow-prev-expand)
     '("l" . meow-right)
     '("L" . meow-right-expand)
     '("m" . meow-join)
     '("n" . meow-search)
     '("O" . meow-to-block)
     '("o" . embark-act)
     '("p" . meow-yank)
     '("P" . meow-yank-pop)
     '("q" . meow-quit)
     '("Q" . meow-goto-line)
     '("r" . meow-replace)
     '("R" . meow-swap-grab)
     '("s" . meow-kill)
     '("t" . meow-till)
     '("u" . meow-undo)
     '("U" . meow-undo-in-selection)
     '("v" . meow-visit)
     '("w" . meow-mark-word)
     '("W" . meow-mark-symbol)
     '("x" . meow-line)
     '("X" . meow-goto-line)
     '("y" . meow-save)
     '("Y" . meow-sync-grab)
     '("z" . meow-pop-selection)
     '("Z" . meow-comment)
     '("'" . repeat)
     '("%" . meow-query-replace-regexp)
     ;; my own modal state
     '("/"  . my-load-meow-structural-mode)
     '("<escape>" . ignore)))

  (defun my-load-meow-structural-mode ()
    (require 'my-meow-structural))

  (my-meow-setup)
  (meow-global-mode t)

  ;; settings show of `meow' state in modeline
  ;;
  ;; I prefer more short names of states
  ;; , so NORMAL => N
  ;;      BEACON => B
  ;;      and etc
  (setq meow-replace-state-name-list
        '((structural . "S") ;; structural is my own state
          (normal . "N")
          (motion . "M")
          (keypad . "K")
          (insert . "I")
          (beacon . "B"))))

(provide 'my-meow)
;;; my-meow.el ends here
