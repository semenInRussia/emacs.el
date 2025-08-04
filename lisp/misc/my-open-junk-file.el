;;; my-open-junk-file.el --- My config for opening junk files -*- lexical-binding: t -*-

;; Copyright (C) 2022-2025 semenInRussia

;;; Commentary:

;; My config for opening junk files

;;; Code:
(require 'my-leaf)

(defcustom my-open-junk-file-format "~/junk/%Y/%m/%d-%H%M%S."
  "File format to put junk files with directory.

It can include `format-time-string' format specifications."
  :type 'string
  :group 'my)

(defun my-open-junk-file (&optional format find-file-fn)
  "Open a new file whose filename is derived from current time.

You can write short program in it.  It helps to try-and-error programs.

For example, in Emacs Lisp programming, use `open-junk-file' instead of
*scratch* buffer.  The junk code is SEARCHABLE.

FORMAT and FIND-FILE-FN are optional.  Default value of them are
`open-junk-file-format' and `open-junk-file-find-file-function'."
  (interactive)
  (let* ((file (format-time-string (or format my-open-junk-file-format) (current-time)))
         (dir (file-name-directory file)))
    (make-directory dir t)
    (funcall (or find-file-fn #'find-file)
             (read-string "Junk Code (Enter extension): " file))))

(define-key global-map (kbd "C-c t") 'my-open-junk-file)

(provide 'my-open-junk-file)
;;; my-open-junk-file.el ends here
