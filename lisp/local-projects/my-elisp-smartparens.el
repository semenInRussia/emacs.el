;;; my-elisp-smartparens.el --- Some configuration for parens insertion with `smartparens` for Emacs Lisp -*- lexical-binding: t; -*-

;; Copyright (C) 2024 semenInRussia

;;; Commentary:

;; Some configuration for parens insertion with `smartparens` for
;; Emacs Lisp.

;;; Code:

(require 'smartparens)

(defun sp-lisp-invalid-hyperlink-p (_id action _context)
  "Test if there is an invalid hyperlink in a Lisp docstring.
ID, ACTION, CONTEXT."
  (when (eq action 'navigate)
    ;; Ignore errors due to us being at the start or end of the
    ;; buffer.
    (ignore-errors
      (or
       ;; foo'|bar
       (and (looking-at "\\sw\\|\\s_")
            ;; do not consider punctuation
            (not (looking-at "[?.,;!]"))
            (save-excursion
              (backward-char 2)
              (looking-at "\\sw\\|\\s_")))
       ;; foo|'bar
       (and (save-excursion
              (backward-char 1)
              (looking-at "\\sw\\|\\s_"))
            (save-excursion
              (forward-char 1)
              (looking-at "\\sw\\|\\s_")
              ;; do not consider punctuation
              (not (looking-at "[?.,;!]"))))))))

(sp-with-modes sp-lisp-modes
  ;; disable ', it's the quote character!
  (sp-local-pair "'" nil :actions nil))

(sp-with-modes '(emacs-lisp-mode)
  ;; also only use the pseudo-quote inside strings where it serve as
  ;; hyperlink.
  (sp-local-pair "`" "'"
                 :when '(sp-in-string-p
                         sp-in-comment-p)
                 :unless '(sp-lisp-invalid-hyperlink-p)
                 :skip-match (lambda (ms _mb _me)
                               (cond
                                ((equal ms "'")
                                 (or (sp-lisp-invalid-hyperlink-p "`" 'navigate '_)
                                     (not (sp-point-in-string-or-comment))))
                                (t (not (sp-point-in-string-or-comment)))))))

(provide 'my-elisp-smartparens)
;;; my-elisp-smartparens.el ends here
