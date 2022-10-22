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

(eval-after-load 'org
  '(progn
     (defun my-drag-org-right ()
       "Try drag anything `org-mode' thing to right."
       (interactive)
       (when (my-drag-org-p) (org-metaright) t))

     (defun my-drag-org-left ()
       "Try drag anything `org-mode' thing to left."
       (interactive)
       (when (my-drag-org-p) (org-metaleft) t))

     (defun my-drag-org-up ()
       "Try drag anything `org-mode' thing to up."
       (interactive)
       (when (my-drag-org-p) (org-metaup) t))

     (defun my-drag-org-down ()
       "Try drag anything `org-mode' thing to down."
       (interactive)
       (when (my-drag-org-p) (org-metadown) t))

     (defun my-drag-org-p ()
       "Return t, when draggers for `org-mode' should work."
       (and
        (eq major-mode 'org-mode)
        (or
         (my-org-mode-in-heading-start-p)
         (my-org-list-item-p)
         (org-at-table-p))))

     (defun my-org-mode-in-heading-start-p ()
       "Return t, when the current position being at a `org-mode' heading."
       (interactive "d")
       (and (not (org-in-src-block-p)) (just-line-prefix-p "*")))

     (add-right-dragger 'my-drag-org-right)
     (add-left-dragger 'my-drag-org-left)
     (add-down-dragger 'my-drag-org-down)
     (add-up-dragger 'my-drag-org-up)))

(eval-after-load 'org
  '(progn
     (defvar my-latex-list-item-string "\\item"
       "String which indicates list item.")

     (defun my-latex-drag-right-list-item ()
       "Drag the list item at the point to the right LaTeX list item."
       (interactive)
       (my-latex-mark-list-item)
       (let ((1-list-item (just-text-in-region)))
         (delete-region (region-beginning) (region-end))
         (my-latex-end-of-list-item)
         (insert 1-list-item)
         (my-latex-goto-backward-list-item)))

     (defun my-latex-drag-left-list-item ()
       "Drag the list item at the point to the left LaTeX list item."
       (interactive)
       (my-latex-mark-list-item)
       (let ((1-list-item (just-text-in-region)))
         (delete-region (region-beginning) (region-end))
         (my-latex-goto-backward-list-item)
         (insert 1-list-item)
         (my-latex-goto-backward-list-item)))

     (defun my-latex-mark-list-item ()
       "Mark as region from the start of the current list item to end of that."
       (interactive)
       (just-mark-region-between-movements 'my-latex-beginning-of-list-item
                                           'my-latex-end-of-list-item))

     (defun my-latex-beginning-of-list-item ()
       "Go to the beginning of an LaTeX list item."
       (interactive)
       (end-of-line)
       (just-search-backward-one-of-regexp
        '("\\\\item"                         ;nofmt
          "\\\\begin *{\\(enumerate\\|itemize\\)}")))

     (defun my-latex-end-of-list-item ()
       "Go to the end of an LaTeX list item."
       (interactive)
       (end-of-line)
       (just-search-forward-one-of-regexp
        '("\\\\item"                         ;nofmt
          "\\\\end *{\\(itemize\\|enumerate\\)}"))
       (beginning-of-line))

     (defun my-latex-goto-backward-list-item ()
       "Go to the beginning of the backward list item."
       (beginning-of-line)
       (search-backward my-latex-list-item-string))

     (defun my-latex-list-item-drag-p ()
       "Return t, when dragger for LaTeX list items should work."
       (interactive)
       (and (eq major-mode 'latex-mode) (my-latex-list-item-line-p)))

     (defun my-latex-list-item-line-p ()
       "Return t, when current line is a LaTeX list item."
       (just-line-prefix-p "\\item" nil t))

     (defun my-latex-try-drag-right-list-item ()
       "If the dragger for LaTeX list item should be work, drag that to right."
       (interactive)
       (when (my-latex-list-item-drag-p)
         (my-latex-drag-right-list-item)
         t))

     (defun my-latex-try-drag-left-list-item ()
       "If the dragger for LaTeX list item should be work, drag that to left."
       (interactive)
       (when (my-latex-list-item-drag-p)
         (my-latex-drag-left-list-item)
         t))

     (add-up-dragger 'my-latex-try-drag-left-list-item)
     (add-down-dragger 'my-latex-try-drag-right-list-item)))

(provide 'my-drag)
;;; my-drag.el ends here
