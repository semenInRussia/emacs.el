;;; my-devdocs.el --- My config for `devdocs'

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

;; My config for `devdocs'

;;; Code:
(require 'my-leaf)
(require 'dash)


(leaf devdocs
  :ensure t
  :hook ((python-mode-hook . my-devdocs-python-hook)
         (emacs-lisp-mode-hook . my-devdocs-emacs-lisp-hook)
         (rust-mode-hook . my-devdocs-rust-hook)
         (c++-mode-hook . my-devdocs-c++-hook)
         (LaTeX-mode-hook . my-devdocs-latex-hook)
         (haskell-mode-hook . my-devdocs-haskell-hook))
  :fast-exec (("Install DevDocs Docset" 'devdocs-install)
              ("Delete DevDocs Docset" 'devdocs-delete))
  :bind ("C-c d" . 'devdocs-lookup)
  :config                               ;nofmt

  (defun my-devdocs-python-hook ()
    "Set docsets of `devdocs' for `python-mode'."
    (setq-local devdocs-current-docs '("python~3.11")))

  (defun my-devdocs-latex-hook ()
    "Set docsets of `devdocs' for `latex-mode'."
    (setq-local devdocs-current-docs '("latex")))

  (defun my-devdocs-emacs-lisp-hook ()
    "Set docsets of `devdocs' for `emacs-lisp-mode'."
    (setq-local devdocs-current-docs '("elisp")))

  (defun my-devdocs-rust-hook ()
    "Set docsets of `devdocs' for `rust-mode'."
    (setq-local devdocs-current-docs '("rust")))

  (defun my-devdocs-c++-hook ()
    "Set docsets of `devdocs' for `c++-mode'."
    (setq-local devdocs-current-docs '("gcc~12_cpp" "cpp")))

  (defun my-devdocs-haskell-hook ()
    "Set docsets of `devdocs'for `haskell-mode'."
    (setq-local devdocs-current-docs '("haskell~9"))))

(provide 'my-devdocs)
;;; my-devdocs.el ends here
