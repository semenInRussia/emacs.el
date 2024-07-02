;;; my-editing.el --- My configuration for the custom editing

;; Copyright (C) 2022-2023 semenInRussia

;;; Commentary:

;; My configuration for the custom editing

;;; Code:
(require 'my-leaf)
(require 'dash)
(require 's)


(defun open-line-saving-indent ()
  "Inserting new line, saving position and inserting new line."
  (interactive)
  (newline)
  (unless (s-blank-p (s-trim (thing-at-point 'line t)))
    (indent-according-to-mode))
  (forward-line -1)
  (end-of-line)
  (delete-horizontal-space t))

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


(defun my-beginning-of-line-text-or-visual-line ()
  "I think the command name explain everything."
  (interactive)
  (goto-char
   (max (save-excursion
          (beginning-of-line-text)
          (point))
        (save-excursion
          (beginning-of-visual-line)
          (point)))))

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

(--each
    '(("M-y" . consult-yank-from-kill-ring)
      ("C-a" . my-beginning-of-line-text-or-visual-line)
      ("C-d" . delete-forward-char)
      ("C-o" . open-line-saving-indent)
      ("C-x C-y" . duplicate-line))
  (global-set-key (kbd (car it)) (cdr it)))

(provide 'my-editing)
;;; my-editing.el ends here
