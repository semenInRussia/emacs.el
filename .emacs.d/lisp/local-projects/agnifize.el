;;; agnifize.el --- Convert a semen python source code to agnia style code -*- lexical-binding: t; -*-

;; Copyright (C) 2022 semenInRussia

;; Author: semenInRussia <hrams205@gmail.com>
;; Version: 0.0.1
;; Package-Requires: ((emacs "24.1") (dash "2.18.0") (s "1.12.0"))

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

;; Convert a semen python source code to agnia style code.

;;; Code:

(require 'dash)
(require 's)
(require 'just)

;;;###autoload
(defun agnifize-dwim ()
  "Agnifize that you mean.

By default agnifize the current buffer.  If a region is selected and active
then agnifize source code between its bounds.  If do in a `dired' buffer, then
try agnifize marked files."
  (interactive)
  (cond
   ((use-region-p)
    (agnifize-region (region-beginning) (region-end)))
   ((eq major-mode 'dired-mode)
    (--each
        (dired-get-marked-files)
      (message "Agnifize file: %s" it)
      (agnifize-file it)))
   (t (agnifize-buffer))))

(defun agnifize-file (filename)
  "Visit file with FILENAME and agnifize it."
  (with-temp-buffer
    (insert-file-contents filename)
    (agnifize-buffer)
    (f-write (buffer-string) 'utf-8 filename)))

(defun agnifize-buffer ()
  "Agnifize the current buffer."
  (interactive)
  (agnifize-region (point-min) (point-max)))

(defun agnifize-region (&optional beg end)
  "Change a semen python code using Agnia coding style in a region.

A region begins with BEG and ends with END"
  (interactive)
  (agnifize--change-comments beg end)
  (agnifize--delete-empty-lines beg end)
  (agnifize--minimize-bin-ops beg end)
  (agnifize--delete-input-prompts beg end)
  (agnifize--all-variables-to-agnia-style beg end))

(defvar agnifize--one-symbol-varnames
  '("a" "b" "c" "d" "x" "y" "n" "q" "k" "l" "m" "u" "o" "p")
  "List of the names for variables which agnia can use.

Each of the should be consist only one character")

(defun agnifize--all-variables-to-agnia-style (&optional beg end)
  "Rename every variable name using agnia style in region between BEG and END."
  (interactive)
  (--map
   (agnifize--rename (car it) (cdr it))
   (-zip-pair
    (agnifize--variables beg end)
    agnifize--one-symbol-varnames)))

(defun agnifize--variables (beg end)
  "Search in region between BEG and END python variables, return alist."
  (->>
   (buffer-substring-no-properties beg (min end (point-max)))
   (s-match-strings-all "^ *\\([a-zA-Z0-9_]\\{2,\\}\\) *= *")
   (-map '-second-item)
   (-uniq)))

(defun agnifize--rename (old new &optional beg end)
  "Rename a python variable called OLD with NEW in region between BEG and END."
  (or beg (setq beg (point-min)))
  (or end (setq end (point-max)))
  (save-excursion
    (goto-char beg)
    (replace-regexp-in-region (rx bow (literal old) eow) new beg end)))

(defvar agnfize--binary-op-regexp
  (regexp-opt
   '("==" "+" "*" "!=" "//" "/" "%" "-" "=" ">" "<" ">=" "<="))
  "Regexp which indicates a python binary operation.")

(defun agnifize--minimize-bin-ops (&optional beg end)
  "Delete spaces around binary operations like + and == in region between BEG and END."
  (interactive)
  (save-excursion
    (goto-char beg)
    (replace-regexp-in-region
     (rx
      (one-or-more " ")
      (group (regexp agnfize--binary-op-regexp))
      (one-or-more " "))
     "\\1"
     beg
     (min end (point-max)))))

(defun agnifize--change-comments (&optional beg end)
  "Delete all comments from the region between BEG and END."
  (or beg (setq beg (point-min)))
  (or end (setq end (point-max)))
  ;; here I don't use built-in comment functions, because I should remove
  ;; dependency from `python-mode' which has a slow speed-up
  (just-for-each-line* beg end
    (when (search-forward "#" (point-at-eol) t)
      (backward-char)
      (just-delete-chars-backward " ")
      (kill-line))))

(defun agnifize--delete-empty-lines (beg end)
  "Remove empty lines from region between BEG and END."
  (interactive)
  (just-for-each-line* beg
      (min (point-max) end)
    (when (and (not (bobp)) (just-line-is-whitespaces-p))
      (delete-char 1))))

(defun agnifize--delete-input-prompts (&optional beg end)
  "Delete in region between BEG and END python promptes which passed to input."
  (->>
   (buffer-substring-no-properties beg (min end (point-max)))
   (s-match-strings-all "input(\\(\".*?\"\\))")
   (--map
    (save-excursion
      (replace-string-in-region
       (-second-item it)
       "" beg
       (min end (point-max)))))))

(provide 'agnifize)
;;; agnifize.el ends here
