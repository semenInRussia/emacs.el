;;; my-lib.el --- My small library of stupid functions -*- lexical-binding: t -*-

;;; Commentary:

;; My small library

;;; Code:

(declare-function sp-get-enclosing-sexp "smartparens")
(declare-function sp-get-sexp "smartparens")
(declare-function sp-get "smartparens")

(require 'dash)
(require 'f)
(require 's)
(require 'just)


(defun my-symbol-append (&rest symbols)
  "Get symbol which has `symbol-name' as concatenation of the each of SYMBOLS."
  (->> symbols (-map 'symbol-name) (apply 's-concat) (intern)))

(defun my-major-mode-to-hook (mm)
  "Return hook for major-mode (MM): `python-mode' => `python-mode-hook'."
  (my-symbol-append mm '-hook))

(defun my-major-mode-to-map (mm)
  "Return map for major-mode (MM): `python-mode' => `python-mode-map'."
  (my-symbol-append mm '-map))

(defun repeat-at-last-keystroke ()
  "Define in the temporary keymap at last pressed keystroke `this-command'."
  (one-shot-keybinding (char-to-string (event-basic-type last-input-event))
                       this-command))

(defun one-shot-keybinding (key command)
  "Bind KEY with COMMAND to one key hitting."
  (set-transient-map (let ((map (make-sparse-keymap)))
                       (define-key map (kbd key) command)
                       map)
                     t))

(defun my-alist-union (alist1 alist2 &optional testfn)
  "Return union of ALIST1 and ALIST2, if has same keys, set to value of ALIST2.

Using TESTFN in functions sush as `assoc' or `alist-get'"
  (->>
   alist1
   (--remove (assoc (car it) alist2 testfn))
   (append alist2)))

(defun my-regexp-opt-of-regexp (regexps)
  "Return the regexp, which will be match to the one of given REGEXPS."
  (concat "\\(?:" (s-join "\\|" regexps) "\\)"))

(defmacro time-it (form &optional iters)
  "Return the average time to evaluate FORM ITERS time.

ITERATIONS defaults to 1"
  (or iters (setq iters 1))
  `(let ((started (current-time)))
     (--dotimes ,iters ,form)
     (/ (float-time (time-since started)) ,iters)))

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
