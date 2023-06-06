;;; my-org-options.el --- Functions to change the options of an `org-mode' file -*- lexical-binding: t; -*-

;; Copyright (C) 2023 semenInRussia

;; Author: semenInRussia <hrams205@gmail.com>
;; Version: 0.0.1
;; Package-Requires: ((dash "2.18.0") (s "1.12.0"))

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

;; Functions to change the options of an `org-mode' file.

;;; Code:
(require 'transient)
(require 'dash)
(require 'just)
(require 's)
(require 'my-lib)

(defvar my-org-options-map
  (make-sparse-keymap)
  "Map for setting `org-mode' options.")

(defvar my-org-options-transient)

;;;###autoload(autoload 'my-org-options-transient "my-org-options")
(transient-define-prefix my-org-options-transient
  "Transient for setting of the `org-mode' options."
  [["Set Info"
    ("a" "Set Author" my-org-options-author)
    ("c" "Set Creator" my-org-options-creator)
    ("e" "Set Email" my-org-options-email)
    ("l" "Set Language" my-org-options-language)
    ("t" "Set Title" my-org-options-title)
    ]
   ["Export With..."
    ("'" "Export With Smart Quotes" my-org-options-smart-quotes)
    ("A" "Export With Author" my-org-options-with-author)
    ("DEL"
     "Export With Archived Trees"
     my-org-options-with-archived-trees)
    ("C" "Export With Clock" my-org-options-with-clocks)
    ("D" "Export With Drawers" my-org-options-with-drawers)
    ("E" "Export With Email" my-org-options-with-email)
    ("RET"
     "Export With Preserve-Breaks"
     my-org-options-with-preserve-breaks)
    ("S" "Export With String" my-org-options-with-special-string)
    ("T" "Export With Table of Contents" my-org-options-with-toc)
    ("^"
     "Export With Supersripts"
     my-org-options-with-sub-supersripts)
    ("*" "Export With Emphasize" my-org-options-with-emphasize)
    ("n" "Export with Sections Numbers" my-org-options-with-emphasize)
    ]])

(defun my-org-options-author (author)
  "Set #+AUTHOR option for the current `org-mode' option to AUTHOR."
  (interactive "sAuthor name, please: ")
  (my-org-set-option "AUTHOR" author))

(defun my-org-options-creator (creator)
  "Commands to set attribute #+CREATOR of `org-mode'."
  (interactive "sCreator, please: ")
  (my-org-set-option "CREATOR" creator))

(defun my-org-options-date (date)
  "Commands to set attribute #+DATE of `org-mode'."
  (interactive "sDate, please: ")
  (my-org-set-option "DATE" date))

(defun my-org-options-email (email)
  "Commands to set attribute #+EMAIL of `org-mode'."
  (interactive "sEMail, please: ")
  (my-org-set-option "EMAIL" email))

(defun my-org-options-language (language)
  "Commands to set attribute #+LANGUAGE of `org-mode'."
  (interactive (list (read-string "Language, please: " "ru")))
  (my-org-set-option "LANGUAGE" language))

(defun my-org-options-title (title)
  "Commands to set attribute #+TITLE of `org-mode'."
  (interactive (list (read-string "Title, please: ")))
  (my-org-set-option "TITLE" title))

(defun my-org-options-smart-quotes (smart-quotes)
  "If SMART-QUOTES is t, add ':t #+OPTIONS, otherwise ':nil"
  (interactive (list (yes-or-no-p "With smart quotes? ")))
  (my-org-set-one-of-options "'" smart-quotes))

(defun my-org-options-with-special-string (with-special-string)
  "If WITH-SPECIAL-STRING is t, add -:t #+OPTIONS, otherwise -:nil"
  (interactive (list (yes-or-no-p "With special string? ")))
  (my-org-set-one-of-options "-" with-special-string))

(defun my-org-options-with-preserve-breaks (with-preserve-breaks)
  "If WITH-PRESERVE-BREAKS is t, add \\n:t #+OPTIONS, otherwise \\n:nil"
  (interactive (list (yes-or-no-p "With preserve breaks? ")))
  (my-org-set-one-of-options "\\n" with-preserve-breaks))

(defun my-org-options-with-sub-supersripts (with-sub-supersripts)
  "If WITH-PRESERVE-SUB-SUPERSRIPTS  is t, add ^:t to #+OPTIONS."
  (interactive (list (yes-or-no-p "With sub supersripts? ")))
  (my-org-set-one-of-options "^" with-sub-supersripts))

(defun my-org-options-with-toc (with-toc)
  "If WITH-TOC is non-nil, then add toc:t to #+OPTIONS, otherwise toc:nil."
  (interactive (list (yes-or-no-p "With table of contents? ")))
  (my-org-set-one-of-options "toc" with-toc))

(defun my-org-options-with-archived-trees (arch)
  "If ARCH is t, add arch:t to #+OPTIONS, otherwise arch:nil."
  (interactive (list (yes-or-no-p "With archived trees? ")))
  (my-org-set-one-of-options "arch" arch))

(defun my-org-options-with-author (author)
  "If AUTHOR is t, add author:t to #+OPTIONS, otherwise author:nil."
  (interactive (list (yes-or-no-p "Export with author? ")))
  (my-org-set-one-of-options "author" author))

(defun my-org-options-with-clocks (clocks)
  "If CLOCKS is t, add c:t to #+OPTIONS, otherwise c:nil."
  (interactive (list (yes-or-no-p "Export with clocks? ")))
  (my-org-set-one-of-options "c" clocks))

(defun my-org-options-with-drawers (drawers)
  "If DRAWERS is t, add d:t to #+OPTIONS, otherwise d:nil."
  (interactive (list (yes-or-no-p "Export with drawers? ")))
  (my-org-set-one-of-options "d" drawers))

(defun my-org-options-with-email (email)
  "If DRAWERS is t, add email:t to #+OPTIONS, otherwise email:nil."
  (interactive (list (yes-or-no-p "Export with email? ")))
  (my-org-set-one-of-options "email" email))

(defun my-org-set-one-of-options (opt val)
  "Set one of options named OPT in form #+TITLE to VAL."
  (my-org-set-options (list (cons opt val))))

(defun my-org-set-options (alist)
  "Set #+OPTIONS to values and keys from ALIST.

  For example: \\='((\"'\" . nil) (\"*\" . t)), set #+OPTIONS to
  ':nil *:t"
  (let ((options (my-org-get-options)))
    (->>
     (my-alist-union options alist 'string-equal)
     (--map (format "%s:%s" (car it) (cdr it)))
     (s-join " ")
     (my-org-set-option "OPTIONS"))))

(defun my-org-get-options ()
  "Get value of #+OPTIONS as an alist.

For example with #+OPTIONS: ':nil *:t, return is
\\='((\"'\" . nil) (\"*\" . t))"
  (let ((options-string (my-org-get-option-value "OPTIONS")))
    (-some->> options-string
      (s-split " ")
      (--map (s-split ":" it))
      (--map (cons (car it) (-second-item it))))))

(defun my-org-get-option-value (option-name)
  "Get one of options #+OPTIONS with OPTION-NAME."
  (save-excursion
    (when (my-org-goto-option option-name)
      (->>
       (just-text-at-line)
       (s-chop-prefix (s-concat "#+" option-name ":"))
       (s-trim)))))

(defun my-org-goto-option (option)
  "Search `org-mode' OPTION with form as #+TITLE in `org-mode' buffer.

  If not found return nil."
  (goto-char (point-min))
  (let ((init-pos (point))
        (search-result (search-forward (s-concat "#+" option) nil t)))
    (unless search-result (goto-char init-pos))
    search-result))

(defun my-org-set-option (option value)
  "Set an option called OPTION sush as #+TITLE to VALUE."
  (interactive "sName of the option: \nsValue, please: ")
  (goto-char (point-min))
  (when (my-org-goto-option option)
    (delete-region (pos-bol) (pos-eol)))
  (insert "#+" option ": " value "\n"))

(provide 'my-org-options)
;;; my-org-options.el ends here
