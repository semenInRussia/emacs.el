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

(require 'my-leaf)

(require 'my-lisp)
(require 's)
(require 'dash)


(leaf elisp-mode
  :config
  (add-hook 'emacs-lisp-mode 'paxedit-mode)

  (leaf inspector
    :ensure t
    :bind (:emacs-lisp-mode-map
           :package elisp-mode
           ("C-c C-i" . inspector-inspect-last-sexp)))

  (leaf paredit
    :ensure t
    :hook emacs-lisp-mode-hook)

  (leaf eros
    :ensure t
    :hook emacs-lisp-mode-hook))

(leaf suggest :ensure t)

(leaf mocker :ensure t :doc "A library for testing `elisp' with mocks")

(leaf my-elisp-embrace
  :hook (emacs-lisp-mode-hook . my-embrace-emacs-lisp-mode-hook))

(provide 'my-elisp)
;;; my-elisp.el ends here
