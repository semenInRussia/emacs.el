;;; my-editing.el --- My configuration for the custom editing -*- lexical-binding: t -*-
;; Copyright (C) 2022-2025 semenInRussia

;;; Commentary:
;; My configuration for the custom editing

;;; Code:

(require 'dash)
(require 'my-leaf)
(require 's)

(leaf yank-indent
  :ensure (yank-indent :repo "jimeh/yank-indent" :host github)
  ;; :hook prog-mode-hook text-mode-hook
  :commands yank-indent--post-command-hook
  :advice ((:after yank my-yank-indent--post-command-hook)
           (:after yank-pop yank-indent--post-command-hook))
  :config
  (defun my-yank-indent--post-command-hook (&rest _) (yank-indent--post-command-hook)))

(defvar w32-pass-lwindow-to-system)
(defvar w32-lwindow-modifier)
(defvar w32-pass-rwindow-to-system)
(defvar w32-rwindow-modifier)
(defvar w32-pass-apps-to-system)
(defvar w32-apps-modifier)

;; make PC keyboard's Win key or other to type Super or Hyper, for emacs running on Windows.
(when (eq system-type 'windows-nt)
  ;; Left Windows key
  (setq w32-pass-lwindow-to-system nil)
  (setq w32-lwindow-modifier 'super)

  ;; Right Windows key
  (setq w32-pass-rwindow-to-system nil)
  (setq w32-rwindow-modifier 'super)

  ;; Menu/App key
  (setq w32-pass-apps-to-system nil)
  (setq w32-apps-modifier 'hyper))


;; PERF,HACK: don't call `repeat-mode' and require it, cause it do
;;   extra work, require it only when the called command have
;;   repeat-map property
(setq repeat-mode t)
(autoload 'repeat-post-hook "repeat")
(add-hook 'post-command-hook
          (defun my--repeat-post-hook ()
            (and
             (or (and (symbolp this-command)
                      (get this-command 'repeat-map))
                 (and (symbolp real-this-command)
                      (get real-this-command 'repeat-map)))
             (repeat-post-hook))))

;; Alt+{down,up} to transpose lines
(leaf move-text
  :ensure t
  :bind (("M-S-<up>" . move-text-up)
         ("M-S-<down>" . move-text-down)))

(declare-function my-open-line-saving-indent "my-editing-funcs.el")
(declare-function my-sport-copy-filename "my-sport-funcs")

(--each
    '(("C-a". my-beginning-of-line-text-or-visual-line)
      ("C-d". delete-forward-char)
      ("C-o". my-open-line-saving-indent)
      ("C-x C-y". duplicate-line)
      ("M-D". duplicate-line)
      ;; `auto-fill-mode': while you are typing symbols and line size increase any
      ;; limit it inserts break
      ("C-x a C-f" . auto-fill-mode)
      ("C-x a f" . auto-fill-mode)
      ("M-A" . auto-fill-mode)
      ;; like in VSCode
      ("M-C". my-sport-copy-filename))
  (keymap-global-set (car it) (cdr it)))

(defvar-keymap my-dupliacte-map
  :repeat (:enter (duplicate-line))
  "y" #'duplicate-line
  "d" #'duplicate-line)

;; disable tabs, sorry Richard
(keymap-set prog-mode-map "RET" #'newline-and-indent)
(setq-default indent-tabs-mode nil
              tab-width 2
              fill-column 80)

;; delete trailing spaces, spaces at the ends of lines
(leaf whitespace
  :hook (before-save-hook . whitespace-cleanup)
  :bind ("M-W" . whitespace-mode))

(leaf avy
  :ensure t
  :bind ("C-;" . avy-goto-char)
  :custom ((avy-keys . '(?a ?s ?d ?f ?l ?k ?j ?o ?e ?i ?w ?q ?x ?n ?m ?p ?u))
           (avy-dispatch-alist .
            '((?W . avy-action-copy)
              (25 . avy-action-yank)        ; C-y
              (?Y . avy-action-yank-line)
              (?K . avy-action-kill)
              (?T . avy-action-teleport))))
  :config
   (advice-remove 'avy-goto-char 'set-mark)
   (advice-add 'avy-goto-char :before
               (defun my-add-set-mark (&rest _args)
                 (push-mark (point) :nomsg))))

;; if you have selected region, type any symbol, it replace region with symbol
(delete-selection-mode 1)

;; insert a template text after file is created
(leaf autoinsert :global-minor-mode auto-insert-mode)

(provide 'my-editing)
;;; my-editing.el ends here
