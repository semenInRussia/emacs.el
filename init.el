;;; init.el --- Initialize emacs lisp code for my emacs -*- lexical-binding: t; -*-

;; Copyright (C) 2023

;; Author: semenInRussia <hrams205@gmail.com>
;; Version: 0.0.1

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

;; Initialize Elisp code for my Emacs.

;;; Code:

(require 'cl-lib)

(let ((dirs (directory-files-recursively "~/.emacs.d/lisp" ".*" t)))
  (mapc (lambda (dir)
          (when (file-directory-p dir)
            (add-to-list 'load-path dir)))
        dirs))

(add-to-list 'load-path
             (locate-user-emacs-file "lisp/local-projects"))

(setq custom-file (expand-file-name "custom.el" user-emacs-directory)
      inhibit-startup-message "~/.emacs.d/GoodLuck.txt"
      inhibit-message "Good Luck"
      initial-major-mode 'fundamental-mode)

(when (file-exists-p custom-file)
  (load custom-file))

(add-to-list 'load-path "~/.emacs.d/lisp")

(load "~/.emacs.d/lisp/local-projects/my-autoload.el")

(defvar my-modules-el-file "~/.emacs.d/dist/my-modules.el")

(unless (file-exists-p (file-name-directory my-modules-el-file))
  (make-directory (file-name-directory my-modules-el-file)))

(add-to-list 'load-path (file-name-directory my-modules-el-file))

(require 'my-modules)

(defgroup my nil "Group for all my config files." :group 'tools)

(provide 'init)
;;; init.el ends here
