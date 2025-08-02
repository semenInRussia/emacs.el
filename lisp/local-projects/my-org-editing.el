;;; my-org-editing.el --- Some commands to edit an `org-mode' source -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2025 semenInRussia

;;; Commentary:

;; Some commands to edit an `org-mode' source.

;;; Code:

(require 'just)
(require 'org)

(declare-function meow-insert-mode "meow")
(declare-function repeat-at-last-keystroke "my-lib")

;;; URL functions

(defvar my-url-prefixes
  '("https://" "http://" "ftp://" "file://")
  "List of the prefixes, which indicates that is URL.")

(defun my-uri-of-url (url)
  "Get the URI of URL."
  (or
   (-some->> url
     (s-chop-prefixes my-url-prefixes)
     (s-split "/")
     ;; ensure that has some URL parts, otherwise return nil
     cdr
     -last-item
     (s-split "?")
     car)
   ""))

(defun my-url-p (str)
  "Return non-nil, if STR is URL."
  (--some (s-prefix-p it str) my-url-prefixes))

(defun my-read-image-url ()
  "Read the URL of a image from the user.

If copied text is a URL, then return.  If region is active, then return a text
in the region.  Otherwise, read a URL from the minibuffer."
  (or (my-url-from-kill-ring)
      (just-text-in-region)
      (read-string "Enter URL for image, please: ")))

(defun my-read-url ()
  "Read the URL of from the user.

If copied text is a URL, then return.  If region is active, then return a text
in the region.  Otherwise, read a URL from the minibuffer."
  (or (my-url-from-kill-ring)
      (just-text-in-region)
      (read-string "URL, please: ")))

(defun my-url-from-kill-ring ()
  "If the last element of the kill ring is a URL, get it, otherwise get nil."
  (let ((copied (current-kill 0)))
    (and (my-url-p copied) copied)))

(defun my-read-string-or-nil
    (prompt &optional initial-input history default-value inherit-input-method)
  "Read string from the minibuffer, if the user type nothing, return nil.

Pass PROMPT, INITIAL-INPUT, HISTORY, DEFAULT-VALUE, INHERIT-INPUT-METHOD to
`read-string'"
  (let ((input
         (read-string prompt initial-input history default-value
                      inherit-input-method)))
    (unless (s-blank-p input) input)))

(defun my-org-table-eval-formula-in-field ()
  "Eval formula with `orgtbl' syntax for the current field of the table."
  (interactive)
  (org-table-eval-formula '(4)))

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
   f-full
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

;;;###autoload
(defun my-org-insert-img-at-url (url &optional new-file-name images-dir caption)
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
