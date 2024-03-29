;;; my-rust.el --- My configuration for rust

;; Copyright (C) 2022, 2023 Semen Khramtsov

;; Author: Semen Khramtsov <hrams205@gmail.com>
;; Version: 0.1
;; URL: https://github.com/semenInRussia/emacs.el

;;; Commentary:

;; My configuration for rust

;;; Code:

(require 'my-leaf)
(require 'dash)
(require 's)
(require 'just)
(require 'my-lib)
(require 'f)

(declare-function embrace-add-pair-regexp "embrace.el")
(declare-function embrace-add-pair "embrace.el")

(autoload 'my-rust-find-Cargo.toml-in-directory "my-rust.el")


(defcustom my-rust-maybe-pub-words
  '(async fn mod struct enum type trait)
  "List of the symbols indicating words which can be public in Rust."
  :type '(repeat symbol)
  :group 'my)

(leaf rust-mode
  :ensure t
  :bind (:rust-mode-map
         ("C-c M-p" . 'my-rust-toggle-pub)
         ("C-c C-t" . 'my-rust-visit-Cargo.toml)
         ("C-c C-m" . 'rust-toggle-mutability))
  :config                               ;nofmt
  (add-hook 'rust-mode-hook #'my-lsp-ensure)

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

  (leaf embrace
    :after embrace
    :hook (rust-mode-hook . my-rust-embrace-hook)
    :config
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
                                  ");"))))))

(provide 'my-rust)
;;; my-rust.el ends here
