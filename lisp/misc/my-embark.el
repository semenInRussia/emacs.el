;;; my-embark.el --- My configuration of `embark' -*- lexical-binding: t; -*-

;; Copyright (C) 2023 semenInRussia

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

;; My configuration of `embark'.

;;; Code:

(require 'my-leaf)


;; TODO: put it to other more right place
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)


(leaf embark
  :defvar (embark-keymap-alist marginalia-prompt-categories)
  :defun (magit-status . magit)
  :ensure t
  :bind (("C-." . embark-act)
         ("C-M-." . embark-dwim)
         (:minibuffer-mode-map
          :package minibuffer           ; built-in
          ("C-," . my-embark-act-noexit)
          ("C-<" . my-embark-act-all-noexit)
          ("C->" . embark-act-all)
          ("C-S-m" . embark-export)
          ("C-M-m" . embark-collect))
         (:embark-general-map
          ("G" . my-embark-google-search))
         (:embark-file-map
          ("G" . my-embark-magit-status)))

  :config

  (defun my-embark-act-noexit ()
    "Do `embark-act' without exit from the minibuffer."
    (interactive)
    (let ((embark-quit-after-action nil))
      (call-interactively #'embark-act)))

  (defun my-embark-act-all-noexit ()
    "Do `embark-act-all' without exit from the minibuffer."
    (interactive)
    (let ((embark-quit-after-action nil))
      (call-interactively #'embark-act-all)))

  ;; some additional actions

  ;; was grabbed from the offical wiki
  (defun my-embark-google-search (term)
    "Open google.com to search a given TERM."
    (interactive "sSearch Term: ")
    (browse-url
     (format "http://google.com/search?q=%s" term)))

  ;; was grabbed from the offical wiki
  (defun my-embark-magit-status (file)
    "Run `magit-status` on repo containing the embark target."
    (interactive "GFile: ")
    (magit-status (locate-dominating-file file ".git")))

  ;; was grabbed from the offical wiki
  (with-eval-after-load 'straight
    (defvar-keymap embark-straight-map
      :parent embark-general-map
      "u" #'straight-visit-package-website
      "r" #'straight-get-recipe
      "i" #'straight-use-package
      "c" #'straight-check-package
      "F" #'straight-pull-package
      "f" #'straight-fetch-package
      "p" #'straight-push-package
      "n" #'straight-normalize-package
      "m" #'straight-merge-package)

    (add-to-list 'embark-keymap-alist '(straight . embark-straight-map))

    (with-eval-after-load 'marginalia
      (add-to-list 'marginalia-prompt-categories '("recipe\\|package" . straight)))))

;; support of agnifize.el: my small Emacs package
(leaf agnifize
  :bind ((:embark-file-map
          :package embark
          ("Q" . 'agnifize-file))
         (:embark-buffer-map
          :package embark
          ("Q" . 'agnifize-buffer))
         (:embark-region-map
          :package embark
          ("Q" . 'agnifize-region))))

;; a = "djeidje "
;; name = "Semen"


(provide 'my-embark)
;;; my-embark.el ends here