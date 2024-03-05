;;; my-merge.el --- A modal state to resolve Git conflicts using sipple keystrokes -*- lexical-binding: t -*-

;; Copyright (C) 2024 semenInRussia

;;; Commentary:

;; A modal state to resolve Git conflicts using simple keystrokes
;;
;; It's built over the built-in `smerge-mode' and `meow' (a native
;; Emacs modal editing thing).
;;
;; I found the default way to resolve Git conflicts very annoying and unintuitive,
;; with `my-merge' you simple use keystrokes j, k to navigate to
;; next/previous different places and <, > to choose other/mine(HEAD)
;; codes.  I found it very very simple and effective.

;;; Code:

(require 'meow-helpers)

(defvar my-merge-map
  (define-keymap
    :parent meow-normal-state-keymap
    "j" #'smerge-next
    "k" #'smerge-prev
    "<" #'smerge-keep-lower
    ">" #'smerge-keep-upper)
  "Keymap for `meow' structural editing state.")

(meow-define-state my-merge
  "A modal editing state to resolve Git conflicts."
  :lighter " MERGE"
  :keymap my-merge-map)

(provide 'my-merge)
;;; my-merge.el ends here
