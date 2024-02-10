;;; my-latex-insert.el --- LaTeX-insert module which expand words with . to latex fragments  -*- lexical-binding: t; -*-

;; Copyright (C) 2022 semenInRussia

;; Author: semenInRussia <hrams205@gmail.com>
;; Version: 0.1
;; Homepage: https://github.com/semeninrussia/emacs.el

;;; Commentary:

;; Insert any LaTeX snippet into buffer.

;;; Code:

(require 'cdlatex)


(defvar my-latex-insert-any-history nil)

;;;###autoload
(defun my-latex-insert-any (key)
  "Expand a `cdlatex' command with KEY, and insert the following LaTeX snippet.

In interactive mode read that key"
  (interactive
   (list
    (completing-read "What to insert? "
                     (mapcar #'car cdlatex-command-alist-comb)
                     nil 'require-math nil
                     'my-latex-insert-any-history)))
  (let ((exp (assoc key cdlatex-command-alist-comb)))
    (insert (nth 2 exp))
    ;; call the function if there is one defined
    (cond
     ((not (nth 3 exp))
      ;; do nothing
      nil)
     ;; if we have args
     ((nth 4 exp)
      (apply (nth 3 exp) (nth 4 exp)))
     ;; just call function
     (t
      (funcall (nth 3 exp))))))

(provide 'my-latex-insert)
;;; my-latex-insert.el ends here
