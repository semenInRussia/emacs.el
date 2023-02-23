;;; my-company-wordfreq.el --- My configuration of `company-wordfreq' -*- lexical-binding: t; -*-

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

;; My configuration of `company-wordfreq'.

;;; Code:

(defcustom my-company-wordfreq-major-modes
  '(latex-mode markdown-mode org-mode text-mode)
  "List of the major modes, in which should work `company-wordfreq'."
  :type '(repeat symbol)
  :group 'my)

(leaf company-wordfreq
  :ensure t
  :config                               ;nofmt

  (defun company-wordfreq--candidates (prefix)
    "Fetches the candidates matching PREFIX."
    (->> "rg ^"
         (s-append prefix)
         (s-append " ")
         (s-append (f-full company-wordfreq-path))
         ;; (shell-command)
         ;; (s-split "\n")
         ))

  (defun my-company-wordfreq-setup ()
    "Setup to work `company-wordfreq'."
    (interactive)
    (add-to-list company-backends '(company-wordfreq))
    (setq-local company-transformers nil)))

(provide 'my-company-wordfreq)
;;; my-company-wordfreq.el ends here
