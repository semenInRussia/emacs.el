;;; my-rust.el --- My configuration for rust

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

;; My configuration for rust

;;; Code:
(autoload 'my-rust-find-Cargo.toml-in-directory "my-rust.el")

(leaf rust-mode
  :ensure t
  :hook ((rust-mode-hook . my-rust-whitespace-mode)
         (rust-mode-hook . lsp-bridge-mode))
  :major-mode-map rust
  :bind (:my-rust-local-map
         ("p" . 'my-rust-toggle-pub)
         ("t" . 'my-rust-visit-Cargo.toml)
         ("m" . 'rust-toggle-mutability))
  :config                               ;nofmt
  (defcustom my-rust-maybe-pub-words
    '(async fn mod struct enum type trait)
    "List of the symbols indicating words which can be public in Rust."
    :type '(repeat symbol)
    :group 'my)

  (defun my-rust-toggle-pub ()
    "Toggle public/private scope of the current rust function/imple/struct."
    (interactive)
    (let ((line-start (point-at-bol)))
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
    :config                             ;nofmt

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

    (defun my-rust-whitespace-mode ()
      "Change the `whitespace-mode' for `rust-mode'."
      (interactive)
      (setq-local whitespace-line-column 100)
      whitespace-line-column)))

(provide 'my-rust)
;;; my-rust.el ends here
