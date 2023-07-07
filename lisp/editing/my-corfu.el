;;; my-corfu.el.el --- My configuration of `corfu' -*- lexical-binding: t; -*-

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

;; My configuration of `corfu'.  I choose `corfu' over `company' because
;; `company' have a big load time (about 9 secs on my computer) while `corfu'
;; 3 secs

;;; Code:

(require 'my-leaf)


(leaf corfu
  :ensure (corfu
           :repo "minad/corfu"
           :files ("*.el" "extensions/*"))
  :global-minor-mode global-corfu-mode
  :custom (;; by default 2 but 1 one is better
           (corfu-auto-prefix . 1)
           ;; by default to run `corfu' you should press `C-M-i'
           (corfu-auto . t)
           ;; I don't like 0sec, because it bad for yasnippets
           (corfu-auto-delay . 0.4)
           ;; the default value (15) is really small
           (corfu-min-width . 40))
  :config
  (leaf corfu-echo
    :require t
    :global-minor-mode corfu-echo-mode))

(leaf kind-icon
  :ensure t
  :require t
  :defun kind-icon-margin-formatter
  :defvar corfu-margin-formatters
  :custom (kind-icon-default-style . '(:padding 0
                                       :stroke 0
                                       :margin 0
                                       :radius 0
                                       :height 0.5
                                       :scale 1))
  :config (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

(leaf cape
  :ensure t
  :require t
  :defun (cape-symbol cape-dabbrev cape-file cape-elisp-block cape-history
                      cape-keyword cape-sgml cape-tex cape-abbrev cape-symbol)
  :config
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-elisp-block)
  (add-to-list 'completion-at-point-functions #'cape-history)
  (add-to-list 'completion-at-point-functions #'cape-keyword)
  (add-to-list 'completion-at-point-functions #'cape-sgml)
  ;; (add-to-list 'completion-at-point-functions #'cape-tex)
  (add-to-list 'completion-at-point-functions #'cape-abbrev)
  (add-to-list 'completion-at-point-functions #'cape-symbol)
  (add-to-list 'completion-at-point-functions #'cape-symbol)
  ;; (add-to-list 'completion-at-point-functions #'cape-line)
  )

;;; my-corfu.el ends here
