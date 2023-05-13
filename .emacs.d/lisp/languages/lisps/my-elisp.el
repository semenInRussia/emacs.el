;;; my-elisp.el --- My configuration of the elisp

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

;; My configuration of the `emacs-lisp-mode'

;;; Code:
(leaf elisp-mode
  :major-mode-map (elisp
                   :modes (emacs-lisp-mode ;nofmt
                           inferior-emacs-lisp-mode
                           lisp-interaction-mode)
                   :parent my-lisp-map)                             ; nofmt
  :hook (emacs-lisp-mode-hook . paxedit-mode)
  :config                               ;nofmt
  (leaf inspector
    :ensure t
    :bind (:my-elisp-local-map
           :package elisp-mode
           ("i" . inspector-inspect-last-sexp)))

  (leaf my-elisp-class-fields
    :bind (:emacs-lisp-mode-map
           :package elisp-mode
           ("M-RET" . my-elisp-new-field-of-class))))

(leaf ert
  :unless (fboundp 'debugger-make-xrefs)
  (defun debugger-make-xrefs ()
    "Just mock for the original function, which I lost."
    nil))

(leaf package-lint
  :ensure t
  :after flycheck
  :config (leaf flycheck-package        ;nofmt
            :ensure t
            :config (flycheck-package-setup)))

(leaf emr
  :ensure t
  :bind (:xah-fly-command-map
         :package xah-fly-keys
         ("SPC /" . emr-show-refactor-menu)))

(leaf elfmt
  :ensure (elfmt :host github :repo "riscy/elfmt")
  :bind (:emacs-lisp-mode-map
         :package elisp-mode
         ([remap my-format-expression] . 'elfmt-sexp)))

(leaf suggest :ensure t)

(leaf mocker :ensure t :doc "A library for testing `elisp' with mocks")

(leaf my-elisp-embrace
  :hook (emacs-lisp-mode-hook . my-embrace-emacs-lisp-mode-hook))

(leaf debug)
  ;:config (advice-add 'debugger-quit :after 'xah-fly-command-mode-activate))

(provide 'my-elisp)
;;; my-elisp.el ends here
