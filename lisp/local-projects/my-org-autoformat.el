;;; my-org-autoformat --- Format `org-mode' code after every keystroke -*- lexical-binding: t -*-

;; Copyright (C) 2023 semenInRussia

;; Author: semenInRussia <hrams205@gmail.com>

;;; Commentary:

;; Format `org-mode' code after every keystroke.  It includes the following
;; features:

;; - capitalize the first symbol after list item (it skips checkboxes)
;; - capitalize the first symbol in an `org-mode' heading (it skips TODO and
;;   DONE labels)
;; - capitalize the first symbol in file attribues like #+TITLE or
;;   #+AUTHOR capitalize the first symbol of a sentence

;; It built over the `my-autoformat' engine.  So you can easily remove useless
;; to you functions and add your own

;;; Code:

(require 'my-autoformat)

(require 'my-lib)
(require 'just)


;;;###autoload(add-hook 'org-mode-hook (lambda () (require 'my-org-autoformat)))
(my-autoformat-bind-for-major-mode
 'org-mode
 'my-org-sentence-capitalization
 'my-org-list-item-capitalization
 'my-org-heading-capitalization)

(defcustom my-org-list-labels-regexps
  '("\\+" "-" "[0-9]+\\.")
  "List of the regexp prefixes indicates a label of a `org-mode' list item.

Label is thing which just decorates a list, but it's not item content, for
example in the following list

- a
- b
- c

Label is \"-\""
  :group 'my
  :type '(repeat string))

(defcustom my-org-keywords
  '("TODO" "DONE")
  "List of the `org-mode' keywords sush as TODO or DONE."
  :group 'my
  :type '(repeat string))

(defcustom my-org-list-label-regexp
  (my-regexp-opt-of-regexp my-org-list-labels-regexps)
  "Regexp indicates a item of a `org-mode' list.

Label is thing which just decorates a list, but it's not item content, for
example in the following list

- a
- b
- c

Label is \"-\", you should consider that spaces before label shouldn't be in the
regexp"
  :group 'my
  :type '(repeat string))

(defcustom my-org-list-item-checkbox-regexp
  "\\[.\\]"
  "Regexp indicates a `org-mode' checkbox."
  :group 'my
  :type 'regexp)

(defcustom my-org-list-item-prefix-regexp
  (rx line-start
      (0+ " ")
      (regexp my-org-list-label-regexp)
      (? (1+ " ") (regexp my-org-list-item-checkbox-regexp))
      (0+ " "))
  "Regexp indicates a list item."
  :group 'my
  :type 'regexp)

(defun my-org-sentence-capitalization ()
  "Capitalize first letter of a sentence in the `org-mode'."
  (interactive)
  (cond
   ((just-call-on-prev-line*
     (or
      (just-line-is-whitespaces-p)
      (my-org-heading-p)
      (my-org-properties-end-p)
      (my-org-list-item-p)))
    (my-autoformat-sentence-capitalization t))
   ((just-call-on-prev-line* (equal (pos-bol) (point-min)))
    (my-autoformat-sentence-capitalization))
   (t
    (just-call-on-backward-char*
     (and
      (looking-back my-autoformat-sentence-end nil)
      (looking-at-p "[[:alpha:]]")
      (upcase-char 1))))))

(defun my-org-heading-p ()
  "Return t, when the cursor located at a `org-mode' heading text."
  ;; NOTE: don't handle cases when bold word located at the beginning of the
  ;; line.  For example:
  ;;
  ;; *bold word*
  ;;
  ;; this function in the above case return t, but excepted nil
  (just-line-prefix-p "*"))

(defun my-org-list-item-capitalization ()
  "Capitalize first letter of a itemized list item."
  (interactive)
  (just-call-on-backward-char*
   (and
    (looking-at-p "[[:alpha:]]")
    (my-org-list-item-p)
    (looking-back my-org-list-item-prefix-regexp nil)
    (upcase-char 1))))

(defun my-org-list-item-p ()
  "Return t, when the cursor located at an item of a `org-mode' list."
  (just-line-regexp-prefix-p my-org-list-item-prefix-regexp))

(defun my-org-properties-end-p ()
  "Get t, if the point placed at the end of `org-mode' subtree properties."
  (string-equal (just-text-at-line nil t) ":END:"))

(defun my-org-heading-capitalization ()
  "Capitalize first letter of a `org-mode' heading.

When `org-mode' heading has any keyword (like to TODO or DONE) first letter
demotes a first letter after keyword word."
  (interactive "d")
  (when (just-call-on-backward-char*
         (and
          (my-org-heading-p)
          (looking-at-p "[[:alpha:]]")
          (progn
            (skip-chars-backward " ")
            (my-org-skip-backward-keyword)
            (skip-chars-backward " *")
            (bolp))))
    (upcase-char -1)))

(defun my-org-skip-backward-keyword ()
  "If right at the cursor placed `org-mode' keyword, then skipt it."
  (when (member (thing-at-point 'symbol) my-org-keywords)
    (backward-sexp)))

(provide 'my-org-autoformat)
;;; my-org-autoformat.el ends here
