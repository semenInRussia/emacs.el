;;; my-window-funcs.el --- Some funcs and commands for navigating windows and buffers -*- lexical-binding: t -*-
;; semenInRussia 2024-2025
;;; Commentary:
;; Some funcs and commands for navigating windows and buffers.

;;; Code:
(declare-function consult-buffer "consult")

;;;###autoload
(defun my-split-right (&optional arg)
  "My version of `split-window-right', difference that new window is active.

ARG is mean that command called interactively."
  (interactive "p")
  (split-window-right)
  (other-window 1)
  (when arg
    (if (featurep 'consult)
        (consult-buffer)
      (call-interactively #'switch-to-buffer))))

;;;###autoload
(defun my-split-below (&optional arg)
  "My version of `split-window-below', difference that new window is active.

ARG is mean that command called interactively."
  (interactive "p")
  (split-window-below)
  (other-window 1)
  (when arg
    (if (featurep 'consult)
        (consult-buffer)
      (call-interactively #'switch-to-buffer))))

;;;###autoload
(defun my-buffer-to-frame-floating ()
  "Display the current buffer in a new floating frame.

This passes certain parameters to the newly created frame:

- use a different name than the default;
- use a graphical frame;
- do not display the minibuffer.

The name is meant to be used by the external rules of a tiling
window manager to present the frame in a floating state."
  (interactive)
  (let ((buf (current-buffer)))
    (when (not (one-window-p t))
      (delete-window))
    (make-frame '((name . "dropdown_emacs-buffer")
                  (window-system . x)
                  (minibuffer . nil)))
    (with-selected-frame (get-other-frame)
      (switch-to-buffer buf))))

;;;###autoload
(defun my-delete-window-frame (&optional window)
  "Delete the current window or frame if the window is one exists in frame."
  (interactive)
  (condition-case nil
      (delete-window window)
    (error (if (and tab-bar-mode
                    (> (length (funcall tab-bar-tabs-function)) 1))
               (tab-bar-close-tab)
             (delete-frame)))))

(declare-function aw-select "ace-window")
;;;###autoload
(defun ace-window-one-command ()
  (interactive)
  (let ((win (aw-select " ACE")))
    (when (windowp win)
      (with-selected-window win
        (let* ((command (key-binding
                         (read-key-sequence
                          (format "Run in %s..." (buffer-name)))))
               (this-command command))
          (call-interactively command))))))

;;;###autoload
(defun ace-window-prefix ()
  "Use `ace-window' to display the buffer of the next command.
The next buffer is the buffer displayed by the next command invoked
immediately after this command (ignoring reading from the minibuffer).
Creates a new window before displaying the buffer.
When `switch-to-buffer-obey-display-actions' is non-nil,
`switch-to-buffer' commands are also supported."
  (interactive)
  (display-buffer-override-next-command
   (lambda (_ _)
     (let (window type)
       (setq
        window (aw-select (propertize " ACE" 'face 'mode-line-highlight))
        type 'reuse)
       (cons window type)))
   nil "[ace-window]")
  (message "Use `ace-window' to display next command buffer..."))

(provide 'my-window-funcs)
;;; my-window-funcs.el ends here
