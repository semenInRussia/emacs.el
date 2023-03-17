;;; my-dired-commands.el --- Provide some useful commands for dired -*- lexical-binding: t; -*-

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

;; Provide some useful commands for dired.

;;; Code:

(require 'dired)
(require 'f)

(defmacro my-dired-save-excursion (&rest body)
  "Evaluate BODY without change a file/directory at point."
  (declare (indent 0))
  `(let ((file (f-full (dired-file-name-at-point))))
     ,@body
     (dired-goto-file file)))

;;;###autoload
(defun my-dired-mark-all-files ()
  "Mark all file in `dired'."
  (interactive)
  (save-excursion (goto-char (point-min)) (dired-mark 1)))

(defmacro my-define-dired-command-taking-file (name args docstring &rest body)
  "Define the command from function which take a 1 argument: filename."
  (declare (indent 2))
  `(defun ,name ()
     (interactive)
     ,docstring
     (funcall
      (lambda ,args ,@body)
      (dired-get-filename))
     (revert-buffer)))

;;;###autoload(autoload 'my-dired-rename "my-dired-commands")
(my-define-dired-command-taking-file my-dired-rename
    (from)
  "Rename file at point from FROM to other name readed from the minibuffer."
  (let ((to (my-rename-file from)))
    (revert-buffer)
    (dired-goto-file to)))

(defun my-rename-file (file)
  "Change name of FILE to new readed from the minibuffer name.

Return new name of FILE"
  (let* ((new-name-of-file
          (read-string "New name, please: " (f-filename file)))
         (to (f-join (f-dirname file) new-name-of-file)))
    (f-move file to)
    to))

;;;###autoload
(defun my-dired-move ()
  "Move file of current directory of `dired' at the point."
  (interactive)
  (dired-do-rename))

;;;###autoload(autoload 'my-dired-delete "my-dired-commands")
(my-define-dired-command-taking-file my-dired-delete
    (file)
  "Delete file at dired object at current position of the cursor."
  (f-delete file t))

;;;###autoload
(defun my-dired-goto-parent-dir ()
  "Navigate to parent directory of current dired directory."
  (interactive)
  (let ((parent (f-parent (dired-current-directory))))
    (kill-buffer)
    (dired parent)
    (dired-goto-file parent)))

;;;###autoload
(defun my-dired-new-file (filename)
  "Create file with FILENAME in the directory which opened in the Dired buffer."
  (interactive "sName of new file, please: ")
  (f-touch (f-join (dired-current-directory) filename))
  (revert-buffer)
  (dired-goto-file (f-full filename)))

;;;###autoload
(defun my-dired-delete-all-files ()
  "Delete all files from the directory of the `dired' buffer."
  (interactive)
  (my-dired-mark-all-files)
  (dired-do-delete)
  (revert-buffer))

;;;###autoload
(defun dired-avy ()
  "Version of `avy' for the `dired'."
  (interactive)
  (avy-goto-line))

;;;###autoload(autoload 'my-dired-duplicate "my-dired-commands")
(my-define-dired-command-taking-file my-dired-duplicate
    (filename)
  "Make copy of the file with FILENAME in the same directory."
  (f-touch
   (f-join
    (dired-current-directory)
    (read-string "Name of the filename, please: "
                 (f-filename filename)))))

;;;###autoload
(defun my-dired-jump-to-home ()
  "Open a `dired' buffer of the home directory."
  (interactive)
  (dired-jump nil "~/"))

(provide 'my-dired-commands)
;;; my-dired-commands.el ends here
