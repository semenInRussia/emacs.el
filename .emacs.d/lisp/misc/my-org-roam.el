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

(leaf org-roam
  :ensure t
  :global-minor-mode org-roam-db-autosync-mode
  :init (f-mkdir "~/org-roam")
  :bind (:xah-fly-command-map
         :package xah-fly-keys
         ("SPC z f" . org-roam-node-find)
         ("SPC z s" . org-roam-ref-add)
         ("SPC z o" . org-roam-buffer-toggle)
         ("SPC z j" . org-roam-node-insert)))

(provide 'my-org-roam)
;;; my-org-roam.el ends here