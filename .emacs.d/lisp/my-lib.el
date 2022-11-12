;;; my-lib.el --- My small library

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

;; My small library

;;; Code:

(require 'dash)
(require 's)
(require 'just)

(defun my-alist-p (obj)
  "Return t, when OBJ is `alist'."
  (and
   (listp obj)
   (not (null obj))
   (consp (car obj))))

(defun my-symbol-append (&rest symbols)
  "Get symbol which has `symbol-name' as concatenation of the each of SYMBOLS."
  (->> symbols (-map 'symbol-name) (apply 's-concat) (intern)))

(defun my-major-mode-to-hook (major-mode)
  "Return hook for MAJOR-MODE: python-mode => python-mode-hook."
  (my-symbol-append major-mode '-hook))

(defun my-major-mode-to-map (major-mode)
  "Return map for MAJOR-MODE: python-mode => python-mode-map."
  (my-symbol-append major-mode '-map))

(defun my-map-to-major-mode (map)
  "Return `major-mode' of MAP: python-mode-map => `python-mode'."
  (->> map (symbol-name) (s-chop-suffix "-map") (intern)))

(defun my-max (lst) "Max of the LST." (and lst (apply 'max lst)))

(defun my-humanize-string (str)
  "Humanaize STR.  For example: just-word -> Just Word."
  (->> str (s-replace "-" " ") (s-titleized-words)))

(defun my-normalize-string (str)
  "Humanaize STR for computer.  For example: Just Word -> just-word."
  (->> str (s-replace " " "-") (s-downcase)))

(defun select-current-line ()
  "Select as region current line."
  (interactive)
  (just-mark-region-between-movements 'beginning-of-line
                                      'end-of-line))

(defun my-call-interactivelly-or-funcall (symbol)
  "If SYMBOL function is command, `call-interactively', otherwise `funcall'."
  (if (commandp symbol) (call-interactively symbol) (funcall symbol)))

(defmacro define-key-when (fun-name map key def pred)
  "Define to KEY in MAP DEF when PRED return t or run old command.

Instead of KEY will command FUN-NAME"
  (declare (indent 0))
  (let ((old-def (lookup-key (eval map) (eval key))))
    `(unless (eq
              (lookup-key ,map ,key)
              ',fun-name)
       (defun ,fun-name ()
         ,(s-lex-format "Run `${old-def}' or `${def}'.")
         (interactive)
         (if (my-call-interactivelly-or-funcall ,pred)
             (my-call-interactivelly-or-funcall ,def)
           (my-call-interactivelly-or-funcall ',old-def)))
       (define-key ,map ,key #',fun-name))))

(defun repeat-at-last-keystroke ()
  "Define in the tempory keymap at last pressed keystroke `this-command'."
  (one-shot-keybinding
   (char-to-string (event-basic-type last-input-event))
   this-command))

(defun one-shot-keybinding (key command)
  "Bind KEY with COMMAND to one key hitting."
  (set-transient-map
   (let ((map (make-sparse-keymap)))
     (define-key map (kbd key) command)
     map)
   t))

(defun my-use-skeleton (skeleton-path destination replacements)
  "Copy SKELETON-PATH to DESTINATION and do REPLACEMENTS."
  (f-copy skeleton-path destination)
  (my-replace-all-in-dir replacements destination))

(defun my-replace-all-in-dir (replacements dir)
  "Replace each `car' of REPLACEMENTS to respective `cdr' in each file of DIR."
  (->
   dir
   (f-files)
   (--each (my-replace-all-in-file replacements it))))

(defun my-replace-all-in-file (replacements filename)
  "Replace each `car' of REPLACEMENTS to respective `cdr' in file at FILENAME."
  (f-write
   (s-replace-all replacements (f-read filename))
   'utf-8
   filename))

(defun my-goto-lisp-sexp-begin (start-name)
  "Go to backward beginning of Lisp sexp which start with START-NAME."
  (--when-let
      (search-backward-regexp
       (rx "(" (zero-or-more whitespace) (regexp start-name))
       nil t)
    (forward-char)
    it))

(defun my-goto-lisp-sexp-end (start-name)
  "Go to end of the backward Lisp sexp which start with START-NAME.
End of Lisp sexp is point before the last closed paren"
  (my-goto-lisp-sexp-begin start-name)
  (sp-get (sp-get-enclosing-sexp) (goto-char :end-in)))

(defun my-mark-lisp-sexp-inner (start-name)
  "Mark the inner of the Lisp sexp which start with function START-NAME."
  (my-goto-lisp-sexp-begin start-name)
  (forward-char -1)
  (sp-get (sp-get-sexp) (just-mark-region :beg-in :end-in)))

(defun my-in-lisp-sexp-p (start-name &optional pt)
  "Get t, When cursor at PT placed in Lisp sexp which start with START-NAME."
  (save-excursion
    (if pt (goto-char pt) (setq pt (point)))
    (and
     (my-goto-lisp-sexp-begin start-name)
     (sp-get (sp-get-enclosing-sexp) (< :beg pt :end)))))

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
     (cdr)
     (-last-item)
     (s-split "?")
     (-first-item))
   ""))

(defun my-url-p (str)
  "Return non-nil, if STR is URL."
  (--some (s-prefix-p it str) my-url-prefixes))

(defun my-read-image-url ()
  "Read the URL of a image from the user.

If copied text is a URL, then return.  If region is active, then return a text
in the region.  Otherwise, read a URL from the minibuffer."
  (or
   (my-url-from-kill-ring)
   (just-text-in-region)
   (read-string "Enter URL for image, please: ")))

(defun my-read-url ()
  "Read the URL of from the user.

If copied text is a URL, then return.  If region is active, then return a text
in the region.  Otherwise, read a URL from the minibuffer."
  (or
   (my-url-from-kill-ring)
   (just-text-in-region)
   (read-string "URL, please: ")))

(defun my-url-from-kill-ring ()
  "If the last element of the kill ring is a URL, get it, otherwise get nil."
  (let ((copied (current-kill 0)))
    (and (my-url-p copied) copied)))

(defun my-read-string-or-nil ;nofmt
    (prompt &optional initial-input history default-value inherit-input-method)
  "Read string from the minibuffer, if the user type nothing, return nil.

Pass PROMPT, INITIAL-INPUT, HISTORY, DEFAULT-VALUE, INHERIT-INPUT-METHOD to
`read-string'"
  (let ((input
         (read-string prompt initial-input history default-value
                      inherit-input-method)))
    (unless (s-blank-p input) input)))

(defun my-buffer-file-name-base ()
  "Return a name of file, opened in the current buffer.

If buffer hasn't file then return nil."
  (and (buffer-file-name) (f-base (buffer-file-name))))

(defun my-incs (s)
  "If S is string that can be converted into number, then return incremented.

Otherwise nil"
  (and                                  ;nofmt
   (stringp s)
   (s-numeric-p s)
   (-> s (string-to-number) (1+) (number-to-string))))

(defun my-alist-union (alist1 alist2 &optional testfn)
  "Return union of ALIST1 and ALIST2, if has same keys, set to value of ALIST2.

Using TESTFN in functions sush as `assoc' or `alist-get'"
  (->>
   alist1
   (--remove (assoc (car it) alist2))
   (append alist2)))

(defun my-regexp-opt-of-regexp (regexps)
  "Return the regexp, which will be match to the one of taked REGEXPS."
  (concat "\\(?:" (s-join "\\|" regexps) "\\)"))

(defun my-inc-filename (path)
  "Increment filename of PATH and return updated.

For example a/b/1.exe should be a/b/2.exe"
  (let ((dirname (f-dirname path))
        (base (f-base path))
        (ext (f-ext path)))
    (->                                ;nofmt
     dirname
     (f-join (my-incs base))
     (f-swap-ext ext))))

(provide 'my-lib)
;;; my-lib.el ends here
