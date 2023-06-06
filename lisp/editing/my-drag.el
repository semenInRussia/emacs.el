;;; my-drag.el --- My config for the things dragging

;; Copyright (C) 2022 Semen Khramtsov

;; Author: Semen Khramtsov <hrams205@gmail.com>
;; Version: 0.1
;; URL: https://github.com/semenInRussia/emacs.el

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; My config for the things dragging

;;; Code:
(require 'my-leaf)
(require 'dash)
(require 'my-xah)
(require 'just)

(defcustom my-drag-map
  (make-sparse-keymap)
  "Keymap for my own small drag system."
  :type 'keymap
  :group 'my)

(defcustom my-left-draggers nil
  "Functions, which drag stuff to left, or return nil.

It is used in `my-drag-stuff-left'."
  :type '(repeat symbol)
  :group 'my)

(defvar my-last-command-is-drag-stuff nil
  "When is non-nil indicates, then last command was dragged any stuff.")

(defun my-drag-p ()
  "Return non-nil, when last command is drag-command."
  (memq last-command
        '(my-drag-stuff-right
          my-drag-stuff-left
          my-drag-stuff-up
          my-drag-stuff-down)))

(defun my-drag-stuff-left ()
  "My more general and functional version of `drag-stuff-left'."
  (interactive)
  (my-drag-anything my-left-draggers))

(defcustom my-right-draggers nil
  "Functions, which drag stuff to right, or return nil.

 It is used in `my-drag-stuff-right'."
  :type '(repeat symbol)
  :group 'my)

(defun my-drag-stuff-right ()
  "My more general and functional version of `drag-stuff-right'."
  (interactive)
  (my-drag-anything my-right-draggers))

(defcustom my-up-draggers nil
  "Functions, which drag stuff to up, or return nil.

Is used in `my-drag-stuff-up'."
  :type '(repeat symbol)
  :group 'my)

(defun my-drag-stuff-up ()
  "My more general and functional version of `drag-stuff-up'."
  (interactive)
  (my-drag-anything my-up-draggers))

(defcustom my-down-draggers nil
  "Functions, which drag stuff to up, or return nil.

It is used in `my-drag-stuff-down'."
  :type '(repeat symbol)
  :group 'my)

(defun my-drag-stuff-down ()
  "My more general and functional version of `drag-stuff-down'."
  (interactive)
  (my-drag-anything my-down-draggers))

(defun my-drag-anything (draggers)
  "Find and call first found true dragger from the DRAGGERS list.

After a call of the this command, current map will be setted to `my-drag-map'
for return the current map back, press either RET or a key which isn't bound in
`my-drag-map'.

True dragger mean that its function return non-nil when called interactively."
  (--find (call-interactively it) draggers)
  (message "Start dragging, use keys u, i, o, k. Type RET for exit...")
  (set-transient-map my-drag-map))

(defun add-left-dragger (f)
  "Add F to list draggers for `my-drag-stuff-left'."
  (add-to-list 'my-left-draggers f))

(defun add-right-dragger (f)
  "Add F to list draggers for `my-drag-stuff-right'."
  (add-to-list 'my-right-draggers f))

(defun add-up-dragger (f)
  "Add F to list draggers for `my-drag-stuff-up'."
  (add-to-list 'my-up-draggers f))

(defun add-down-dragger (f)
  "Add F to list draggers for `my-drag-stuff-down'."
  (add-to-list 'my-down-draggers f))

(defun stop-drag ()
  "Stop drag, just something print, and nothing do, set to nil something."
  (interactive)
  (setq my-last-command-is-drag-stuff nil)
  (message "Turn `drag' to normal!"))

(leaf-keys
 (my-drag-map
  ("RET" . stop-drag)
  ("i"   . my-drag-stuff-up)
  ("k"   . my-drag-stuff-down)
  ("o"   . my-drag-stuff-right)
  ("u"   . my-drag-stuff-left)))

(leaf drag-stuff
  :ensure t
  :global-minor-mode drag-stuff-global-mode
  :bind (:my-drag-map
         ("j"       . my-drag-stuff-left-char)
         ("."       . transpose-sexps)
         ("m"       . transpose-sexps)
         ("n"       . avy-transpose-lines-in-region)
         ("l"       . my-drag-stuff-right-char)
         ("t"       . transpose-regions))
  :config                             ;nfomt
  (eval-after-load 'xah-fly-keys
    '(define-key xah-fly-command-map (kbd "SPC TAB") my-drag-map))

  (defun my-drag-stuff-left-char ()
    "Drag char to left."
    (interactive)
    (transpose-chars -1))

  (defun my-drag-stuff-right-char ()
    "Drag char to right."
    (interactive)
    (transpose-chars 1))

  (add-left-dragger  'drag-stuff-left)
  (add-right-dragger 'drag-stuff-right)
  (add-up-dragger    'drag-stuff-up)
  (add-down-dragger  'drag-stuff-down))

(provide 'my-drag)
;;; my-drag.el ends here
