;;; my-rust-editing.el --- SOme functions useful for editing Rust file -*- lexical-binding: t -*-
;; semenInRussia 2024

;;; Commentary:
;;; Code:
(require 'dash)
(require 'just)
(require 's)
(require 'my-lib)

(declare-function embrace-add-pair-regexp "embrace.el")
(declare-function embrace-add-pair "embrace.el")

(defcustom my-rust-maybe-pub-words
  '(async fn mod struct enum type trait)
  "List of the symbols indicating words which can be public in Rust."
  :type '(repeat symbol)
  :group 'my)

(defun my-rust-toggle-pub ()
  "Toggle public/private scope of the current rust function/imple/struct."
  (interactive)
  (let ((line-start (pos-bol)))
    (save-excursion
      (end-of-line)
      (or
       ;; try search a keyword in the current line
       (--first
        (search-backward-regexp
         (s-concat (symbol-name it) " ")
         line-start t)
        my-rust-maybe-pub-words)
       ;; otherwise try search a keyword in the current buffer
       (--first
        (search-backward-regexp
         (s-concat (symbol-name it) " ")
         nil t)
        my-rust-maybe-pub-words))
      (if (looking-back "pub *" nil)
          (just-delete-word -1)
        (insert "pub ")))
    (repeat-at-last-keystroke)))

(defun my-rust-find-Cargo.toml-in-directory (&optional dir)
  "Find closest Cargo.toml in the DIR and return path to it."
  (interactive)
  (setq dir (or dir default-directory))
  (let ((cargo.toml (f-join dir "Cargo.toml")))
    (if (f-exists-p cargo.toml)
        cargo.toml
      (my-rust-find-Cargo.toml-in-directory (f-parent dir)))))

(defun my-rust-visit-Cargo.toml ()
  "Visit Cargo.toml file of current rust crate."
  (interactive)
  (find-file (my-rust-find-Cargo.toml-in-directory)))

(defun my-rust-embrace-hook ()
    "Add parens to `embrace' parens for `rust-mode'."
    (interactive)
    (embrace-add-pair ?v "Vec<" ">")
    (embrace-add-pair ?d "dbg!(" ")")
    (embrace-add-pair ?b "Box<" ">")
    (embrace-add-pair ?o "Option<" ">")
    (embrace-add-pair ?r "Result<" ">")
    (embrace-add-pair-regexp ?p
                             "print\\(ln\\)?!(\".*?\"," ")"
                             (lambda ()
                               (interactive)
                               (cons
                                (concat
                                 "println!("
                                 (read-string
                                  "Template to format string: "
                                  "\"{}\"")
                                 ", ")
                                ");"))))

(provide 'my-rust-editing)
;;; my-rust-editing.el ends here
