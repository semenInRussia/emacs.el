;;; my-buffer-navigation.el --- My config for navigation beetween buffers

;; Copyright (C) 2022-2024 semenInRussia

;; Author: semenInRussia <hrams205@gmail.com>
;; Version: 0.1
;; URL: https://github.com/semenInRussia/emacs.el

;;; Commentary:

;; My config for navigation between buffers.
;;
;; check this article of @karthinks
;; https://karthinks.com/software/emacs-window-management-almanac/
;;
;; Tutorial how to navigate windows
;;
;; Termins
;; - Buffer is a thing which have the text, name and almost usually
;;   have a file path
;; - Frame is separated floating window, you can move it, using your
;;   OS things
;; - Window is part of current frame which is shown now at the top.
;;   if you see only one file, then you see one window.
;;
;; How to use:
;;
;; 1. Change the current(active) window with M-o
;; 2. Close the current(active) window M-0(zero)
;; 3. Split horizontally with hit C-x 3 and choose needed buffer
;;    when I choose `consult-buffer' will be called, if I need other
;;    command (like `find-file'), I can hit > to which is
;;    `embark-become'
;; 4. Split vertically with   ... C-x 5 ...
;; 5. Make a window frame (for fun), with C-x f
;; 6. Also you can change the buffer of current window with M-[ and M-]
;; 7. If you are `meow' user, try q to either kill window, change
;;    buffer to other

;;; Code:
(require 'my-leaf)

(require 'dash)
(require 's)


(leaf ace-window
  :ensure (ace-window :repo "abo-abo/ace-window"
                      :host github)
  :bind ("M-o" . ace-window))

(defun my-visit-last-opened-buffer ()
  "Visit buffer which was opened recently."
  (interactive)
  (switch-to-buffer (my-last-opened-buffer)))

(defun my-last-opened-buffer ()
  "Get buffer which was visited most recently."
  (--find
   (not (my--visit-last-opened-buffer-ignore-p it))
   (cdr (buffer-list))))

(defun my--visit-last-opened-buffer-ignore-p (buffer)
  "Take object of BUFFER and return nil when don't need visit its."
  (->> buffer (buffer-name) (s-trim) (s-prefix-p "*Minibuf")))

(defun my-split-right ()
  "My version of `split-window-right', difference that new window is active."
  (interactive)
  (split-window-right)
  (other-window 1)
  (when (interactive-p)
    (if (featurep 'consult)
        (consult-buffer)
      (call-interactively #'switch-to-buffer))))

(defun my-split-below ()
  "My version of `split-window-below', difference that new window is active."
  (interactive)
  (split-window-below)
  (other-window 1)
  (when (interactive-p)
    (if (featurep 'consult)
        (consult-buffer)
      (call-interactively #'switch-to-buffer))))

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
    (if (not (one-window-p t))
        (delete-window))
    (make-frame '((name . "dropdown_emacs-buffer")
                  (window-system . x)
                  (minibuffer . nil)))
    (with-selected-frame (get-other-frame)
      (switch-to-buffer buf))))

(defun my-delete-window-frame (&optional window)
  "Delete the current window or frame if the window is one exists in frame."
  (interactive)
  (condition-case nil
      (delete-window window)
    (error (if (and tab-bar-mode
                    (> (length (funcall tab-bar-tabs-function)) 1))
               (tab-bar-close-tab)
             (delete-frame)))))

(with-eval-after-load 'embark
  (keymap-set embark-become-file+buffer-map "2" #'my-split-below)
  (keymap-set embark-become-file+buffer-map "3" #'my-split-right))

(leaf-keys
 (("C-<tab>" . 'my-visit-last-opened-buffer)
  ;; Fast select buffers
  ("M-[" . previous-buffer)
  ("M-]" . next-buffer)

  ;; split
  ("C-x 3" . my-split-right)
  ("C-x 2" . my-split-below)

  ;; Close window
  ("M-0" . my-delete-window-frame)

  ;; Make window separately frame
  ("C-x f" . my-buffer-to-frame-floating)))

(provide 'my-buffer-navigation)
;;; my-buffer-navigation.el ends here
