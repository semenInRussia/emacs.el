;;; my-org-roam.el --- My configuration of `org-roam' -*- lexical-binding: t; -*-

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

;; My configuration of `org-roam'.

;;; Code:
(require 'my-leaf)
(require 'f)

(leaf emacsql
  :ensure t)

(leaf org-roam
  :ensure t
  :init (f-mkdir "~/org-roam")
  :commands org-roam-ui-mode
  :defun org-roam-db-autosync-mode
  :bind (("C-c z f"   . org-roam-node-find)
         ("C-c z t t" . org-roam-tag-add)
         ("C-c z t d" . org-roam-tag-remove)
         ("C-c z s s" . org-roam-ref-add)
         ("C-c z s d" . org-roam-ref-remove)
         ("C-c z a a" . org-roam-alias-add)
         ("C-c z a d" . org-roam-alias-remove)
         ("C-c z o"   . org-roam-buffer-toggle)
         ("C-c z j"   . org-roam-node-insert))
  :custom (org-roam-mode-sections . '(org-roam-backlinks-section))
  :config                               ;nofmt
  (org-roam-db-autosync-mode t)

  ;; (require 'org-roam-export)
  ;; (require 'org-roam-protocol)

  (leaf simple-httpd
    :ensure t)

  (leaf org-roam-ui
    :ensure t
    :defun org-roam-ui-mode
    :config (org-roam-ui-mode t)))

(provide 'my-org-roam)
;;; my-org-roam.el ends here
