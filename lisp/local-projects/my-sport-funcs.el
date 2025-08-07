;;; my-sport-funcs.el --- Some functions for sport programming -*- lexical-binding: t -*-

;;; Commentary:
;; Some functions for sport (Olympic) programming in C++.  Here a list
;; (maybe not full) of provided functions:
;;
;; 1. find file for problem samples (input.txt)
;; 2. insert this file
;; 3. copy code from current file (useful to quickly paste it into the
;;   web-site, like codeforces.com)
;;
;; NOTE: that some functions are located inside my-c.el

;;; Code:
(require 'dash)
(require 'f)
(require 's)

;;;###autoload
(defun my-copy-whole-buffer-as-kill (&optional msg-p)
  "Copy the content of whole current buffer onto `kill-ring'.

If MSG-P is non-nil, say that content was copied."
  (interactive "p")
  (kill-new (buffer-string))
  (when msg-p
    (message "%s chars was COPIED!" (- (point-max) (point-min)))))

;;;###autoload
(defun my-sport-find-samples-file ()
  "Find input.txt file for current C++ file."
  (interactive)
  (find-file "input.txt"))

;; In sport programming I sometimes use debugger (gdb) or run
;; `eshell'.  When I run `gud-gdb' (see `my-realgud') I need to enter
;; all samples data (located in file input.txt) in one line.  The
;; following function do it.  I also can call it with (C-c ; C-i, it)
;; + it is like on (C-x i) which inserts content of the file.
;;;###autoload
(defun my-sport-insert-samples ()
  "And insert the content of the input.txt onto the buffer in one line."
  (interactive)
  (and
   (or (file-exists-p "input.txt")
       (user-error "File input.txt isn't exists, create it using C-c ; C-f (SPC ; f)"))
   (->>
    "input.txt"
    f-read-text
    (s-replace "\n" " ")
    insert)))

(defun my-sport-copy-filename (&optional buf inter-p)
  "Copy absolute path of BUF's file as kill.

BUF defaults to the current buffer.  INTER-P is non-nil when this functions is
called \"interactively\""
  (interactive (list nil t))
  (let ((it (buffer-file-name buf)))
    (if it
        (progn
         (kill-new (buffer-file-name))
         (when inter-p
           (message "File path `%s' copied as kill" it)))
      (user-error "Can't copy path of non-file buffer"))))

(provide 'my-sport-funcs)
;;; my-sport-funcs.el ends here
