;;; my-feature.el --- My configuration of `feature' -*- lexical-binding: t; -*-

;; Copyright (C) 2022 semenInRussia

;; Author: semenInRussia <hrams205@gmail.com>
;; Version: 0.1
;; Homepage: https://github.com/semeninrussia/emacs.el

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

;; My configuration of `feature-mode'.

;;; Code:

(require 'my-leaf)


;; NOTE that here support only `ecukes' now.
;; `ecukes' is cucumber for `emacs-lisp-mode'
(leaf feature-mode
  :ensure (feature-mode :repo "michaelklishin/cucumber.el" :host github)
  :hook (feature-mode-hook . my-feature-mode-hook)
  :bind (:feature-mode-map
         ("RET" . newline-and-indent)
         ("M-RET" . my-feature-add-and-statement))
  :config
  (defun my-feature-mode-hook ()
    "My hook of the `feature-mode'."
    (setq-local outline-regexp "\\( *Feature:\\| *Scenario:\\)"))

  (defun my-feature-add-and-statement ()
    "Add a \"And\" feature statement, like to statement at the current line."
    (interactive)
    (end-of-line)
    (insert "\n" (buffer-substring-no-properties (pos-bol) (pos-eol)))
    (forward-line 1)))

(provide 'my-feature)
;;; my-feature.el ends here
