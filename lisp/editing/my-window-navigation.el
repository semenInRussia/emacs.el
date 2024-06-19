;;; my-window-navigation.el --- My config for navigation beetween buffers
;; Copyright (C) 2022-2024 semenInRussia
;; Author: semenInRussia <hrams205@gmail.com>

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
;; 4. Split vertically with ... C-x 2 ...
;; 5. Make a window frame (for fun), with C-x f
;; 6. Also you can change the buffer of current window with C-x C-n
;;    and C-x C-p
;; 7. If you are `meow' user, try q to either kill window, change
;;    buffer to other
;; 8. If I need to open anything like documentation
;;   (`describe-variable') I press "C-x O", after choose command "C-h
;;   v", after choose the place where this documentation buffer will
;;   be opened

;;; Code:
(require 'my-leaf)
(require 'dash)
(require 's)

(leaf ace-window
  :ensure t
  :bind ("M-o" . ace-window))

(declare-function my-split-below "my-window-funcs")
(declare-function my-split-right "my-window-funcs")

(with-eval-after-load 'embark
  (defvar embark-become-file+buffer-map)
  (keymap-set embark-become-file+buffer-map "2" #'my-split-below)
  (keymap-set embark-become-file+buffer-map "3" #'my-split-right))

(defvar-keymap my-prev-next-buf-map
  :repeat (:enter (next-buffer previous-buffer))
  "n" #'next-buffer
  "p" #'previous-buffer)

(leaf-keys
 (("C-<tab>" . 'my-visit-last-opened-buffer)
  ;; Fast select buffers
  ("C-x C-p" . previous-buffer)
  ("C-x C-n" . next-buffer)

  ;; split
  ([remap split-window-right] . my-split-right) ;; C-x 3
  ([remap split-window-below] . my-split-below) ;; C-x 2

  ;; Close window
  ("M-0" . my-delete-window-frame)

  ;; Make window separately frame
  ("C-x f" . my-buffer-to-frame-floating)

  ("C-;" . ace-window-one-command)
  ("C-x O" . ace-window-prefix)))

(provide 'my-window-navigation)
;;; my-window-navigation.el ends here
