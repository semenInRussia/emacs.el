;;; my-lib.el --- My small library of stupid functions -*- lexical-binding: t -*-

;;; Commentary:

;; My small library

;;; Code:

(require 'dash)
(require 'f)
(require 's)

;;;###autoload
(defun repeat-at-last-keystroke ()
  "Define in the temporary keymap at last pressed keystroke `this-command'."
  (one-shot-keybinding (char-to-string (event-basic-type last-input-event))
                       this-command))

;;;###autoload
(defun one-shot-keybinding (key command)
  "Bind KEY with COMMAND to one key hitting."
  (set-transient-map (let ((map (make-sparse-keymap)))
                       (define-key map (kbd key) command)
                       map)
                     t))

;;;###autoload
(defmacro time-it (form &optional iters)
  "Return the average time to evaluate FORM ITERS time.

ITERATIONS defaults to 1"
  (or iters (setq iters 1))
  `(let ((started (current-time)))
     (--dotimes ,iters ,form)
     (/ (float-time (time-since started)) ,iters)))

;;;###autoload
(defmacro which-faster (iters &rest things)
  "Print name of the most fast things from given THINGS.

Also print average time to one iteration of each thing's call (do ITERS
calls for each thing)

Each thing is binding of name of thing (just a symbol without quote) and form
which should be evaluated"
  `(let ((times
          (list
           ,@(--map
              `(cons ',(car it) (time-it ,(cadr it) ,iters))
              things))))
     (--each
         (--sort (< (cdr it) (cdr other)) times)
       (message "Thing `%s' took `%s's" (car it) (cdr it)))
     (--sort (< (cdr it) (cdr other)) times)))

(defun my-current-year ()
  "Return the current year."
  (format-time-string "%Y"))

(provide 'my-lib)
;;; my-lib.el ends here
