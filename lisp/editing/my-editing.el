;;; my-editing.el --- My configuration for the custom editing

;; Copyright (C) 2022-2023 Semen Khramtsov

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

(defvar yank-indent-modes
  '(prog-mode sgml-mode js2-mode)
  "Modes in which to indent regions that are yanked (or yank-popped).")

(defvar yank-advised-indent-threshold 1000
  "Threshold (# chars) over which indentation does not automatically occur.")

(defun yank-advised-indent-function (beg end)
  "Do indentation, as long as the region beetween BEG END isn't too large."
  (when (<= (- end beg) yank-advised-indent-threshold)
    (indent-region beg end nil)))

(defadvice yank (after yank-indent activate)
  "If current mode is one of 'yank-indent-modes, indent yanked text.
With prefix arg don't indent."
  (if (and
       (not (ad-get-arg 0))
       (--any? (derived-mode-p it) yank-indent-modes))
      (let ((transient-mark-mode nil))
        (yank-advised-indent-function
         (region-beginning)
         (region-end)))))

(defadvice yank-pop (after yank-pop-indent activate)
  "If current mode is one of 'yank-indent-modes, indent yanked text.
With prefix arg don't indent."
  (if (and
       (not (ad-get-arg 0))
       (member major-mode yank-indent-modes))
      (let ((transient-mark-mode nil))
        (yank-advised-indent-function
         (region-beginning)
         (region-end)))))

(defun yank-unindented ()
  "Just `yunk'."
  (interactive)
  (yank 1))

(defvar w32-apps-modifier)
(defvar w32-pass-lwindow-to-system)
(defvar w32-lwindow-modifier)
(defvar w32-pass-rwindow-to-system)
(defvar w32-rwindow-modifier)
(defvar w32-pass-apps-to-system)

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


(--each
    '(("M-y" . consult-yank-from-kill-ring)
      ("C-a" . beginning-of-line-text)
      ("C-o" . open-line-saving-indent)
      ("M-y" . consult-yank-from-kill-ring)
      ("C-x C-y" . duplicate-line))
  (global-set-key (kbd (car it)) (cdr it)))

(provide 'my-editing)
;;; my-editing.el ends here
