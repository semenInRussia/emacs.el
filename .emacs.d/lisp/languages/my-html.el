;;; my-html.el --- My configuration for html

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

;; My configuration for html

;;; Code:
(defcustom my-html-modes
  '(web-mode mhtml-mode)
  "List of `html` major modes."
  :group 'my
  :type '(repeat symbol))

(defun my-html-modes-hooks ()
  "Return list from the hooks for each of `my-html-modes'."
  (-map 'my-major-mode-to-hook my-html-modes))

(defun my-html-modes-maps ()
  "Return list from the maps for each of `my-html-modes'."
  (-map 'my-major-mode-to-map my-html-modes))

(leaf mhtml-mode
  :ensure t
  :mode "\\.html$"
  :major-mode-map `(html ,my-html-modes)
  :hook (mhtml-mode-hook . lsp-bridge-mode)
  :config                               ;nofmt
  (leaf auto-rename-tag
    :ensure t
    :hook `(,(my-html-modes-hooks)
            . auto-rename-tag-mode))

  (leaf tagedit
    :ensure t
    :bind `(,(--map
              `(,it
                :package ,(my-map-to-major-mode it)
                ([remap sp-kill-hybrid-sexp] . tagedit-kill)
                ([remap sp-join-sexp]        . tagedit-join-tags)
                ([remap sp-raise-sexp]       . tagedit-raise-tag)
                ([remap sp-splice-sexp]      . tagedit-splice-tag)
                ([remap sp-change-enclosing]  . tagedit-kill-attribute))
              (my-html-modes-maps))))

  (leaf emmet-mode :ensure t :hook mhtml-mode-hook)

  (leaf impatient-mode
    :ensure t
    :defun impatient-mode
    :bind (:my-html-local-map
           :package mhtml-mode
           ("e" . my-enable-impatient-mode))
    :config                             ;nofmt
    (defun my-enable-impatient-mode ()
      "Enable `impatient-mode' open page of the file in the web browser."
      (interactive)
      (impatient-mode +1)
      (imp-visit-buffer))))

(provide 'my-html)
;;; my-html.el ends here
