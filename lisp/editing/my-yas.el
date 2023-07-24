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

(leaf yasnippet
  :doc "Load `fast-exec' keymaps for `yasnippet'."
  :after fast-exec
  :config                               ;nofmt
  (defun yas--fetch (table key)
    "Fetch templates in TABLE by KEY.

Return a list of cons (NAME . TEMPLATE) where NAME is a
string and TEMPLATE is a `yas--template' structure."
    (let* ((key (s-downcase key))
	   (keyhash (yas--table-hash table))
	   (namehash (and keyhash (gethash key keyhash))))
      (when namehash
	(yas--filter-templates-by-condition
	 (yas--namehash-templates-alist namehash))))))

(provide 'my-yas)
;;; my-yas.el ends here
