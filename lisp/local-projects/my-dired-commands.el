;;; my-dired-commands.el --- Provide some useful commands for Dired -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2025 semenInRussia

;;; Commentary:

;; Provide some useful commands for Dired.

;;; Code:

(require 'dired)
(require 'f)

;;;###autoload
(defun my-dired-mark-all-files ()
  "Mark all file in `dired'."
  (interactive)
  (save-excursion (goto-char (point-min)) (dired-mark 1)))

;;;###autoload
(defun my-dired-new-file (filename)
  "Create file with FILENAME in the directory which opened in the Dired buffer."
  (interactive "sName of new file, please: ")
  (f-touch (f-join (dired-current-directory) filename))
  (revert-buffer)
  (dired-goto-file (f-full filename)))

;;;###autoload(autoload 'my-dired-duplicate "my-dired-commands")
(defun my-dired-duplicate ()
  "Make copy of the file with FILENAME in the same directory."
  (interactive)
  (f-touch
   (f-join
    (dired-current-directory)
    (read-string "Name of the filename, please: "
                 (f-filename (dired-get-filename)))))
  (revert-buffer))

;;;###autoload
(defun my-dired-jump-to-home ()
  "Open a `dired' buffer of the home directory."
  (interactive)
  (dired-jump nil "~/"))

(provide 'my-dired-commands)
;;; my-dired-commands.el ends here
