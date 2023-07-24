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

(require 'my-leaf)
(require 'dash)
(require 'custom)


(defvar my-html-supported-modes
  '(web-mode mhtml-mode)
  "List of `html` major modes."
  ;; :group 'my
  ;; :type '(repeat symbol)
  )

(defun my-html-supported-modes-hooks ()
  "Return list from the hooks for each of `my-html-supported-modes'."
  (-map 'my-major-mode-to-hook my-html-supported-modes))

(defun my-html-supported-modes-maps ()
  "Return list from the maps for each of `my-html-supported-modes'."
  (-map 'my-major-mode-to-map my-html-supported-modes))

(leaf mhtml-mode
  :mode "\\.html$"
  :hook (mhtml-mode-hook . my-lsp-ensure)
  :config                               ;nofmt
  (leaf auto-rename-tag
    :ensure (auto-rename-tag :repo "jcs-elpa/auto-rename-tag" :host github))

  (leaf tagedit
    :ensure (tagedit :repo "magnars/tagedit" :host github)
    ;;    :bind `(,(--map
    ;; `(,it
    ;;   :package ,(my-map-to-major-mode it)
    ;;   ([remap sp-kill-hybrid-sexp] . tagedit-kill)
    ;;   ([remap sp-join-sexp]        . tagedit-join-tags)
    ;;   ([remap sp-raise-sexp]       . tagedit-raise-tag)
    ;;   ([remap sp-splice-sexp]      . tagedit-splice-tag)
    ;;   ([remap sp-change-enclosing]  . tagedit-kill-attribute))
    ;; (my-html-supported-modes-maps))))
    )
  (leaf emmet-mode
    :ensure (emmet-mode :repo "smihica/emmet-mode" :host github)
    :hook mhtml-mode-hook)

  (leaf impatient-mode
    :ensure (impatient-mode :repo "skeeto/impatient-mode" :host github)
    :defun (imp-visit-buffer impatient-mode)
    :bind (:html-mode-map
           :package mhtml-mode
           ("C-c C-e" . my-enable-impatient-mode))
    :config                             ;nofmt
    (defun my-enable-impatient-mode ()
      "Enable `impatient-mode' open page of the file in the web browser."
      (interactive)
      (impatient-mode +1)
      (imp-visit-buffer))))

(provide 'my-html)
;;; my-html.el ends here
