;;; my-yas.el --- my-yas

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

;;; Code:
(use-package yasnippet
    :ensure t
    :config (yas-global-mode +1)
    :custom
    (yas-snippet-dirs '("~/.emacs.d/snippets"))
    (yas-wrap-around-region t))

(defun yas--fetch (table key)
  "Fetch templates in TABLE by KEY.

Return a list of cons (NAME . TEMPLATE) where NAME is a
string and TEMPLATE is a `yas--template' structure."
  (let* ((key (s-downcase key))
         (keyhash (yas--table-hash table))
         (namehash (and keyhash (gethash key keyhash))))
    (when namehash
      (yas--filter-templates-by-condition
       (yas--namehash-templates-alist namehash)))))

(defun fast-exec-my-yas-keys ()
  "Get some useful keymaps of  `fast-exec' for my-yas."
  (fast-exec/some-commands ("Yasnippet Edit Snippet" 'yas-visit-snippet-file)))

(fast-exec/register-keymap-func 'fast-exec-my-yas-keys)
(fast-exec/reload-functions-chain)

(provide 'my-yas)
;;; my-yas.el ends here