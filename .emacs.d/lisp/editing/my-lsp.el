;;; my-lsp.el --- My configuration of `lsp' -*- lexical-binding: t; -*-

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

;; My configuration of `lsp'.

;;; Code:

(defcustom my-lsp-supported-modes
  '(rust-mode LaTeX-mode)
  "List of the major modes working with `lsp'."
  :type '(repeat symbol)
  :group 'me)

(defun my-lsp-supported-modes-hooks ()
  "Return lust of major modes hooks working with `lsp'."
  (-map 'my-major-mode-to-hook my-lsp-supported-modes))

(leaf lsp-mode
  :ensure t
  :hook `(,(my-lsp-supported-modes-hooks)
          . lsp)
  :custom `(;; Performance
            (gc-cons-threshold . 100000000)
            (read-process-output-max . ,(* 1024 1024)) ;; 1mb
            ;; Features
            (lsp-lens-enable . nil))
  :bind ((:lsp-mode-map
          ([remap xref-find-definitions] . 'lsp-find-definition))
         (:xah-fly-command-map
          :package xah-fly-keys
          ("SPC , ," . 'lsp-find-references)))
  :config                               ;nofmt
  (leaf helm-lsp
    :ensure t
    :bind ((:lsp-mode-map
            ([remap helm-imenu] . 'helm-lsp-workspace-symbol))
           (:xah-fly-command-map
            :package xah-fly-keys
            ("SPC SPC x" . 'helm-lsp-diagnostics)
            ("SPC SPC 7" . 'lsp-rename)
            ("SPC SPC RET" . 'helm-lsp-code-actions)))))

(provide 'my-lsp)
;;; my-lsp.el ends here
