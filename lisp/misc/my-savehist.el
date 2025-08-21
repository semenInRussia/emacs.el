;;; my-savehist.el --- My configuration of savehist -*- lexical-binding: t; -*-

;; Copyright (C) 2025 semenInRussia

;;; Commentary:
;; My configuration of savehist.
;;
;; `savehist-mode' is an Emacs feature that preserves the minibuffer history and
;; other things like this between sessions.  It includes:
;; - clipboard
;; - macros / registers
;; - global marks (you can use them with C-x C-a or M-g -)

;;; Code:
(require 'my-leaf)

(leaf savehist)

(provide 'my-savehist)
;;; my-savehist.el ends here
