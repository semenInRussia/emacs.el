;;; my-meow-structural.el --- Structural editing for `meow' -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2024 semenInRussia

;;; Commentary:

;; Structural editing for `meow'.  It provides additional state to `meow' like insert, beacon
;; keypad or command.  It change keybindings that very similar to keybindigs outside of normal mode,
;; so to go forward sexp just use "l" that originally bound to `meow-next-char'.

;;; Code:

(require 'meow-core)                    ; for `meow-normal-mode'
(require 'meow-util)
(require 'meow-command)


(defgroup my-meow-structural '()
  "Structural editing for `meow' modal editing mode.
Originally was written for original `meow' QWERTY layout."
  :group 'editing)

;;; Variables that defines original paredit keybidings

(defcustom my-meow-structural-kbd-mark-sexp "C-M-@"
  "Keybiding to mark the s-expression after the cursor."
  :group 'my-meow-structural
  :type 'string)

(defcustom my-meow-structural-kbd-forward-sexp "C-M-f"
  "Keybiding to go the next s-expression."
  :group 'my-meow-structural
  :type 'string)

(defcustom my-meow-structural-kbd-backward-sexp "C-M-b"
  "Keybiding to go the backward s-expression."
  :group 'my-meow-structural
  :type 'string)

(defcustom my-meow-structural-kbd-backward-up-sexp "C-M-u"
  "Keybiding to backward up the s-expression."
  :group 'my-meow-structural
  :type 'string)

(defcustom my-meow-structural-kbd-forward-down-sexp "C-M-d"
  "Keybiding to forward down s-expression."
  :group 'my-meow-structural
  :type 'string)

;;; Functions that do all structural things

(defun my-meow-structural-mark-sexp ()
  "Mark the s-expression after the cursor."
  (interactive)
  ;; go to the beginning of the sexp at point
  (my-meow-structural--just-forward-sexp)
  (my-meow-structural--just-backward-sexp)
  (save-excursion
    ;; push mark to the end of the next s-expression
    (my-meow-structural--just-forward-sexp)
    (my-meow-structural--maybe-start-sexp-selection)))

(defun my-meow-structural--just-forward-sexp ()
  "Just go the forward N th sexp."
  (meow--execute-kbd-macro my-meow-structural-kbd-forward-sexp))

(defun my-meow-structural--just-backward-sexp ()
  "Just go the backward N th sexp."
  (meow--execute-kbd-macro my-meow-structural-kbd-backward-sexp))

(defun my-meow-structural--just-backward-up-sexp ()
  "Just go the backward-up N th sexp."
  (meow--execute-kbd-macro my-meow-structural-kbd-backward-up-sexp))

(defun my-meow-structural--just-forward-down-sexp ()
  "Just go the forward-down N th sexp."
  (meow--execute-kbd-macro my-meow-structural-kbd-forward-down-sexp))

(defun my-meow-structural-forward-sexp ()
  "Go the next s-expression."
  (interactive)
  (my-meow-structural--maybe-start-sexp-selection)
  (my-meow-structural--just-forward-sexp))

(defun my-meow-structural-backward-sexp ()
  "Go the backward s-expression."
  (interactive)
  (my-meow-structural--maybe-start-sexp-selection)
  (my-meow-structural--just-backward-sexp))

(defun my-meow-structural--maybe-start-sexp-selection (&optional pt)
  "If the current selection isn't expandable, then start new sexp selection.

Start expansion at the point PT.  PT defaults to the value of `point'"
  (or pt (setq pt (point)))
  (unless (meow--select-expandable-p)
    ;; cancel old selection and start new
    (set-mark pt)
    (setq meow--selection '(select . sexp))))

(defun my-meow-structural-backward-up-sexp ()
  "Backward up the s-expression."
  (interactive)
  (my-meow-structural--just-backward-up-sexp)
  (my-meow-structural-mark-sexp))

(defun my-meow-structural-forward-down-sexp ()
  "Forward down s-expression."
  (interactive)
  (when (and (region-active-p) (meow--direction-forward-p))
    (my-meow-structural--just-backward-sexp))
  (my-meow-structural--just-forward-down-sexp)
  (my-meow-structural-mark-sexp))

;;; Provide `meow' state

(defvar my-meow-structural-state-keymap
  (define-keymap
    ;; i think that if a keybindings isn't exists in structural editing
    ;; that should be exists a function from the normal state instead with the
    ;; same key.  for it specify :parent
    :parent meow-normal-state-keymap
    "<escape>" #'meow-normal-mode
    "C-g" #'meow-normal-mode
    "g" #'meow-normal-mode
    "/" #'meow-normal-mode
    "c" #'meow-change
    "l" #'my-meow-structural-forward-sexp
    "h" #'my-meow-structural-backward-sexp
    "k" #'my-meow-structural-backward-up-sexp
    "j" #'my-meow-structural-forward-down-sexp
    "i" #'meow-insert
    "x" #'my-meow-structural-mark-sexp
    "r" #'meow-raise-sexp
    "s" #'meow-kill
    "S" #'meow-splice-sexp
    "." #'meow-forward-slurp
    "," #'meow-forward-barf
    "]" #'meow-backward-barf
    "[" #'meow-backward-slurp)
  "Keymap for `meow' structural editing state.")

(meow-define-state structural
  "Meow state for structural editing."
  :lighter " STRUCTURE"
  :keymap my-meow-structural-state-keymap)

;; meow-define-state creates the variable
(setq meow-cursor-type-structural 'hollow)

(define-key meow-normal-state-keymap "/" #'meow-structural-mode)

(provide 'my-meow-structural)
;;; my-meow-structural.el ends here
