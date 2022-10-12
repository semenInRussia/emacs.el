;;; my-editing.el --- My configuration for the custom editing

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

;; My configuration for the custom editing

;;; Code:

(require 'dash)
(require 'leaf)
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

(defun open-line-below ()
  "Make new empty line below current."
  (interactive)
  (end-of-line)
  (newline)
  (indent-for-tab-command))

(defun open-line-above ()
  "Make new empty line below current."
  (interactive)
  (beginning-of-line)
  (newline)
  (forward-line -1)
  (indent-for-tab-command))

(defun duplicate-current-line-or-region (arg)
  "Duplicates the current line or region ARG times.

If there's no region, the current line will be duplicated."
  (interactive "p")
  (if (region-active-p)
      (let ((beg (region-beginning))
            (end (region-end)))
        (duplicate-region arg beg end)
        (one-shot-keybinding
         "y"
         (lambda (interactive) (duplicate-region 1 beg end))))
    (duplicate-current-line arg)
    (one-shot-keybinding "y" 'duplicate-current-line)))

(defun duplicate-region (&optional num start end)
  "Duplicates the region bounded by START and END NUM times.

If no START and END is provided, the current `region-beginning' and
`region-end' is used."
  (interactive "p")
  (save-excursion
    (let* ((start (or start (region-beginning)))
           (end (or end (region-end)))
           (region (buffer-substring start end)))
      (goto-char end)
      (dotimes (i num) (insert region)))))

(defun duplicate-current-line (&optional num)
  "Duplicate the current line NUM times."
  (interactive "p")
  (save-excursion
    (when (eq (point-at-eol) (point-max))
      (goto-char (point-max))
      (newline)
      (forward-char -1))
    (duplicate-region num (point-at-bol) (1+ (point-at-eol)))))

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

(defun yank-unindented () "Just `yunk'." (interactive) (yank 1))

(defun kill-to-beginning-of-line ()
  "Kill region from the current position to the beginning of line."
  (interactive)
  (kill-region (point-at-bol) (point)))

(leaf-keys
 (:xah-fly-command-map
  :package xah-fly-keys
  ("SPC y"     . duplicate-current-line-or-region)
  ("SPC s"     . open-line-below)
  ("SPC SPC s" . open-line-above)
  ("s"         . open-line-saving-indent)
  ("SPC SPC b" . my-change-case-of-current-line)
  ("SPC e"     . kill-to-beginning-of-line)))

(defun my-change-case-of-current-line ()
  "Change case of current line to next (see `xah-toggle-letter-case')."
  (interactive)
  (save-mark-and-excursion
    (select-current-line)
    (xah-toggle-letter-case))
  (repeat-at-last-keystroke))

(defvar my-last-command-is-changed-case-of-current-line
  nil "In t, when last command change case.")

(defun my-duplicate-last-block ()
  "Take last text block and insert."
  (interactive)
  (while (looking-back "[\n\t ]" nil) (delete-char -1))
  (->>
   (buffer-substring (my-point-at-last-block-beg) (point))
   (s-trim)
   (s-append "\n")
   (s-prepend "\n\n")
   (insert))
  (goto-char (my-point-at-last-block-beg)))

(defun my-point-at-last-block-beg ()
  "Return the position of beginning of last block."
  (interactive)
  (save-excursion
    (if (re-search-backward "\n[\t\n ]*\n+" nil 1)
        (match-end 0)
      (point-min))))

(leaf-keys
 (xah-fly-command-map
  :package xah-fly-keys
  ("SPC k 6"    . my-duplicate-last-block)
  ("SPC SPC v"  . helm-show-kill-ring)))

(provide 'my-editing)
;;; my-editing.el ends here
