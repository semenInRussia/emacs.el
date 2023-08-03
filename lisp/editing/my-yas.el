;;; my-yas.el --- My configuration for the `yasnippet'

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

;; My configuration for the `yasnippet'

;;; Code:


(require 'my-leaf)
(require 's)
(require 'dash)


(leaf yasnippet
  :ensure (yasnippet :repo "joaotavora/yasnippet")
  :defun (yas--table-hash
          yas--filter-templates-by-condition
          yas--namehash-templates-alist)
  :global-minor-mode yas-global-mode
  :custom `((yas-snippet-dirs .
                              ',(list
                                 (locate-user-emacs-file "snippets")))
            (yas-wrap-around-region . t)))

(declare-function cape--table-with-properties "cape")
(declare-function cape--bounds "cape")
(declare-function cape-interactive "cape")

(with-eval-after-load 'cape
  (defvar my-yas--capf-properties
    (list :annotation-function (lambda (_) " Snippet ")
          :company-kind (lambda (_) 'snippet)
          :exit-function 'my-yas-capf--exit
          :company-docsig 'my-yas-capf--docsig
          :exclusive 'no)
    "Completion extra properties for `my-yas-capf'.")

  (defun my-yas-capf (&optional interactive)
    "Completion at point for `yasnippet'.

If INTERACTIVE is true, show the completion where suggested only snippets."
    (interactive (list t))
    (if interactive
        (cape-interactive #'my-yas-capf)
      (when-let (snippets (yas-active-keys))
        (let ((bounds (cape--bounds 'symbol)))
          `(,(car bounds) ,(cdr bounds)
            ,(cape--table-with-properties snippets :category 'snippet)
            ,@my-yas--capf-properties)))))

  (defun my-yas-capf--docsig (key)
    "Snippet content for `corfu' which show it in the echo area.

It takes the KEY (trigger) of snippet, because user type it and `corfu'
manipulate with it to show helpful things"
    (->
     ;; the first snippets table
     (yas--get-snippet-tables major-mode)
     (car)
     ;; fetch snippets with a given key
     (yas--fetch key)
     ;; choose the first
     (car)
     (cdr)
     ;; get its content
     (yas--template-content)))

  (defun my-yas-capf--exit (name status)
    "Exit from `my-yas-capf'."
    (and
     (eq status 'finished)
     (yas-expand)))

  (add-hook
   'corfu-mode-hook
   (defun my-yas-capf-setup ()
     "Add capf for `yasnippet'."
     (add-hook 'completion-at-point-functions 'my-yas-capf 30 'local))))

(provide 'my-yas)
;;; my-yas.el ends here
