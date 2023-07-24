;;; init.el --- Initialize Emacs Lisp code for my Emacs -*- lexical-binding: t; -*-

;; Copyright (C) 2023

;; Author: semenInRussia <hrams205@gmail.com>

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Initialize Elisp code for my Emacs.

;;; Code:

(require 'cl-lib)
(require 'subr-x)  ;; for `string-remove-prefix'

;; every custom variable of my config have the following group
(defgroup my nil "Group for all my config files." :group 'tools)

;; add every directory of my config to the `load-path'
(add-to-list 'load-path "~/.emacs.d/lisp")
(let ((dirs (directory-files-recursively "~/.emacs.d/lisp" ".*" t)))
  (mapc (lambda (dir)
          (when (file-directory-p dir)
            (add-to-list 'load-path dir)))
        dirs))

;; local-projects is my own small "packages" which aren't so big to create real packages
(add-to-list 'load-path (locate-user-emacs-file "lisp/local-projects"))
(load "~/.emacs.d/lisp/local-projects/my-autoload")

;; don't use .emacs.d for custom.el which I don't use
;;
;; in the most of configurations, after it Emacs load custom.el, but I fount it
;; a bit useless.  I prefer `setq' over `custom'
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

;; the most part of the config located inside "~/.emacs.d/lisp" I join all .el
;; files into the my-modules file for fast start up
(defvar my-modules-el-file "~/.emacs.d/dist/my-modules.el")

(unless (file-exists-p (file-name-directory my-modules-el-file))
  (user-error "File \"my-modules.el\" didn't created, suggest use --modules option"))

(add-to-list 'load-path (file-name-directory my-modules-el-file))

(let ((file-name-handler-alist nil))
  (require 'my-modules))

(provide 'init)
;;; init.el ends here
