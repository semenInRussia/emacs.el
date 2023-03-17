;;; my-org-editing.el --- Some commands to edit an `org-mode' source -*- lexical-binding: t; -*-

;; Copyright (C) 2023 semenInRussia

;; Author: semenInRussia <hrams205@gmail.com>
;; Version: 0.0.1

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Some commands to edit an `org-mode' source.

;;; Code:

(require 'org)
(require 'xah-fly-keys)

;;;###autoload
(defun my-org-clear-subtree ()
  "Kill subtree at the position, and activate insertion mode."
  (org-cut-subtree)
  (xah-fly-insert-mode-activate))

(defun my-org-table-eval-formula-in-field ()
  "Eval formula with `orgtbl' syntax for the current field of the table."
  (interactive)
  (org-table-eval-formula '(4)))

;;;###autoload
(defvar my-org-table-cut-map
  '(keymap
    (?. . org-cut-special)
    (?c . org-table-delete-column)
    (?r . my-kill-line-or-region)))

;;;###autoload
(defun my-org-cut ()
  "Cut any `org' thing.

If in a table ask from the user: either kill column, kill cell or kill row, if
in a src block cut it, otherwise kill heading"
  (interactive)
  (cond
   ((org-at-table-p)
    (set-transient-map my-org-table-cut-map))
   (t (org-cut-subtree))))

;;;###autoload
(defun my-org-schedule-to-today ()
  "Scheduale a `org-mode' heading to today."
  (interactive)
  (org-schedule t (format-time-string "%Y-%m-%d")))

;;;###autoload
(defun my-org-indent-subtree ()
  "Indent current the `org-mode' subtree at current position."
  (interactive)
  (save-excursion
    (org-mark-subtree)
    (indent-region (region-beginning) (region-end))))

;;;###autoload
(defun my-org-todo ()
  "My version of the `org-todo'.

Different with the original functions is that this function can be repeated by
pressing of the previous last pressed char.  So if functions is bound to
\"SPC l 1\", that after pressing that user can press \"1\" and this command will
be repeated"
  (interactive)
  (call-interactively #'org-todo)
  (one-shot-keybinding "1" 'my-org-todo))

;;;###autoload
(defun my-org-insert-image (filename &optional caption)
  "Insert a image with FILENAME.

By default, caption for the image don't inserts, but if CAPTION is a
string, then define caption of the image to the CAPTION.

In the interactive, If the region is active, the FILENAME will be text
in the region."
  (interactive
   (list (my-org-read-image-filename) (my-org-read-image-caption)))
  (setq filename (my-org-path-for-image filename))
  (just-ensure-empty-line)
  (when caption                     ;nofmt
    (insert "#+CAPTION: " caption)
    (newline))
  (insert "[[" filename "]]"))

(defun my-org-path-for-image (path)
  "Make PATH to an image to path for `org-mode' images specially."
  (->>
   path
   (f-full)
   (s-chop-prefix (f-full default-directory))
   (s-prepend "./")))

(defun my-org-read-image-filename ()
  "Read a image filename.

If the region is active, then return text in the region as filename, otherwise
return filename readed from the minibuffer."
  (my-org-path-for-image
   (or
    (just-text-in-region)
    (read-file-name "Please, choose image to insert: "))))

(defun my-org-read-image-caption ()
  "Read a image caption from the minibuffer.

If the user insert any caption, return its, otherwise return nil."
  (let ((caption (read-string "Caption for the image, please: ")))
    (unless (s-blank-p caption) caption)))

(defcustom my-org-default-images-dir "./images/"
  "Default directory for images of a `org-mode' document."
  :type 'string
  :group 'my)

(defun my-org-insert-img-at-url     ;nofmt
    (url &optional new-file-name images-dir caption)
  "Insert org image at URL, download it into IMAGES-DIR with name NEW-FILE-NAME.

If the region is active return it, otherwise read URL from the minibuffer.
If caption isn't empty string, then insert image with the caption CAPTION."
  (interactive (my--get-arguments-for-org-insert-img-at-url))
  (or images-dir (setq images-dir my-org-default-images-dir))
  (let ((new-filename (f-join images-dir new-file-name)))
    (my-download url new-filename)
    (my-org-insert-image new-filename caption)))

(defun my--get-arguments-for-org-insert-img-at-url ()
  "Get arguments from the user for `my-org-insert-img-at-url'."
  (let* ((url (my-read-image-url))
         (new-file-name (my-org-read-new-image-at-url-file-name url))
         (images-dir (my-org-read-images-dir))
         (caption (my-org-read-image-caption)))
    (list url new-file-name images-dir caption)))

(defun my-org-read-images-dir ()
  "Read directory path for downloading of the image."
  (read-directory-name "Image will download into directory:"
                       my-org-default-images-dir))

(defun my-org-read-new-image-at-url-file-name (url)
  "Read from the minibuffer new file name for the image at URL."
  (read-string "Image will be downloaded with name: "
               (my-uri-of-url url)))

(defun my-download (url new-filename)
  "Download file at URL as file with NEW-FILENAME."
  (make-directory (f-dirname new-filename) t)
  (url-copy-file url new-filename t))

(provide 'my-org-editing)
;;; my-org-editing.el ends here
