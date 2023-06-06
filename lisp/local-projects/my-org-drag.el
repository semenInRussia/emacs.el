;;; my-org-drag.el --- Some commands to drag items in `org-mode' -*- lexical-binding: t; -*-

;; Copyright (C) 2023 $6

;; Author: $6 <hrams205@gmail.com>
;; Version: 0.0.1
;; Package-Requires: ((dash "2.18.0") (s "1.12.0"))

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

;; Some commands to drag items in `org-mode'.

;;; Code:

(require 'org)
(require 'just)

(declare-function my-org-list-item-p "my-org.el")

(defun my-drag-org-right ()
  "Try drag anything `org-mode' thing to right."
  (interactive)
  (when (my-drag-org-p) (org-metaright) t))

(defun my-drag-org-left ()
  "Try drag anything `org-mode' thing to left."
  (interactive)
  (when (my-drag-org-p) (org-metaleft) t))

(defun my-drag-org-up ()
  "Try drag anything `org-mode' thing to up."
  (interactive)
  (when (my-drag-org-p) (org-metaup) t))

(defun my-drag-org-down ()
  "Try drag anything `org-mode' thing to down."
  (interactive)
  (when (my-drag-org-p) (org-metadown) t))

(defun my-drag-org-p ()
  "Return t, when draggers for `org-mode' should work."
  (and
   (eq major-mode 'org-mode)
   (or
    (my-org-mode-in-heading-start-p)
    (my-org-list-item-p)
    (org-at-table-p))))

(defun my-org-mode-in-heading-start-p ()
  "Return t, when the current position being at a `org-mode' heading."
  (interactive "d")
  (and (not (org-in-src-block-p)) (just-line-prefix-p "*")))

(provide 'my-org-drag)
;;; my-org-drag.el ends here
