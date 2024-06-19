;;; my-editing.el --- My configuration for the custom editing

;; Copyright (C) 2022-2023 semenInRussia

;;; Commentary:

;; My configuration for the custom editing

;;; Code:

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

(setf
 ;; Don't resize the frames in steps; it looks weird, especially in tiling window
 ;; managers, where it can leave unseemly gaps.
 frame-resize-pixelwise t
 ;; Inhibit resizing frame
 frame-inhibit-implied-resize t
 ;; But do not resize windows pixelwise, this can cause crashes in some cases
 ;; when resizing too many windows at once or rapidly.
 window-resize-pixelwise nil
 (alist-get 'width default-frame-alist) (car my-layout-size)
 (alist-get 'height default-frame-alist) (cdr my-layout-size)
 (alist-get 'width initial-frame-alist) (car my-layout-size)
 (alist-get 'height initial-frame-alist) (cdr my-layout-size)

 ;; don't use the system title bar
 frame-title-format '(buffer-file-name "%f" ("%b"))
 (alist-get 'undecorated default-frame-alist) t
 (alist-get 'drag-internal-border default-frame-alist) 1
 (alist-get 'internal-border-width default-frame-alist) 5)

(leaf yank-indent
  :ensure (yank-indent :repo "jimeh/yank-indent" :host github))

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

;; PERF,HACK: don't call `repeat-mode', cause it do extra work, like
;;   counting their commands + do stuoid message'

;; (repeat-mode)
(require 'repeat)
(setq repeat-mode t)
(when repeat-keep-prefix
  (add-hook 'pre-command-hook 'repeat-pre-hook))
(add-hook 'post-command-hook 'repeat-post-hook)

(--each
    '(("M-y" . consult-yank-from-kill-ring)
      ("C-a" . my-beginning-of-line-text-or-visual-line)
      ("C-d" . delete-forward-char)
      ("C-o" . open-line-saving-indent)
      ("C-x C-y" . duplicate-line))
  (global-set-key (kbd (car it)) (cdr it)))

(provide 'my-editing)
;;; my-editing.el ends here
