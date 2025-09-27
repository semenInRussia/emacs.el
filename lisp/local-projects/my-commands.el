;;; my-acommands.el --- Some commands I use sometimes -*- lexical-binding: t -*-
;; Copyright (C) 2025 semenInRussia

;;; Commentary:
;; Some commands I use sometimes.

;;; Code:

(require 'dash)
(require 's)

;;;###autoload
(defun my-open-line-saving-indent ()
  "Inserting new line, saving position and inserting new line."
  (interactive)
  (newline)
  (unless (s-blank-p (s-trim (thing-at-point 'line t)))
    (indent-according-to-mode))
  (forward-line -1)
  (end-of-line)
  (delete-horizontal-space t))

;;;###autoload
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

;;;###autoload
(defun my-toggle-line-numbers (&optional msg-p)
  "Toggle line numbers mode.

If MSG-P is non-nil display the current display line numbers type."
  (interactive (list t))
  (let ((typs (list 'relative 'absolute nil)))
    (if (not (memq display-line-numbers-type typs))
        (setq display-line-numbers-type nil)
      (->>
       (-zip-pair typs (-rotate 1 typs))
       (--find (eq (car it) display-line-numbers-type))
       cdr
       (setq display-line-numbers-type)))
    (display-line-numbers-mode)
    (when msg-p
      (message "Line numbers type: `%s'" display-line-numbers-type))))

(provide 'my-editing-funcs)
;;; my-editing-funcs.el ends here
