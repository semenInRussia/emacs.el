;;; my-writing-config.el --- My configuration for the writing other configuration -*- lexical-binding: t; -*-

;; Copyright (C) 2022 semenInRussia

;; Author: semenInRussia <hrams205@gmail.com>
;; Version: 0.1
;; Package-Requires: ((dash "2.18.0") (s "1.12.0"))
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

;; My configuration for the writing other configuration.

;;; Code:
(require 'my-leaf)

(require 'dash)
(require 's)

(declare-function inspector-inspect "inspector.el")

(defun my-new-config-module (module-name &optional directory)
  "Create a new configuration file named MODULE-NAME in the DIRECTORY.

DIRECTORY defaults to ~/.emacs.d/lisp/"
  (interactive "sName of the configuration module: \nDDirectory: ")
  (setq directory (or directory user-emacs-directory))
  (->>                                  ;nofmt
   module-name
   (s-append ".el")
   (s-prepend "my-")
   (s-prepend directory)
   (find-file))
  (insert
   (s-replace
    "writing-config"
    module-name
    (format
     ";;; my-writing-config.el --- My configuration of `writing-config' -*- lexical-binding: t; -*-

;; Copyright (C) %s semenInRussia

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

;; My configuration of `writing-config'.

;;; Code:

(leaf writing-config)

(provide 'my-writing-config)
;;; my-writing-config.el ends here"
     (format-time-string "%Y"))))
  (search-backward "(leaf "))

(with-eval-after-load 'fast-exec
  (fast-exec-bind
   'writing-config
   (fast-exec-make-some-commands
    ("New Config Module" 'my-new-config-module))))

(leaf ecukes
  :ensure t
  :bind (:my-feature-local-map :package feature-mode ("e" . ecukes))
  :hook (ecukes-mode-hook . my-feature-mode-hook)
  :config (leaf espuds :ensure t :require t))

(leaf leaf
  :bind ("C-x M-f" . 'leaf-find))

(defun my-bench ()
  "Show bench analysis for Emacs startup."
  (interactive)
  (switch-to-buffer "*Messages*")
  (->>
   (buffer-substring-no-properties (point-min) (point-max))
   (s-lines)
   (--keep
    (-when-let
        ((_ module duration)
         (s-match "‘\\(.*?\\)’ module took \\(.*?\\)sec" it))             ;nofmt
      (cons module (string-to-number duration))))
   (--sort (> (cdr it) (cdr other)))
   (inspector-inspect))
  (rename-buffer "*My Bench*"))

(defun my-do-autoload-for-local-projects-files ()
  "If the opened file is a \"local projects\", make the directory autoloads."
  (interactive)
  (let ((dir (f-full (locate-user-emacs-file "lisp/local-projects/")))
        (out (f-full (locate-user-emacs-file "lisp/local-projects/my-autoload.el")))
        flycheck-files)
    (when (string-prefix-p dir (f-full (buffer-file-name)))
      (->>
       dir
       (f-files)
       (--filter (or
                  (s-prefix-p "flycheck_" (f-base it))
                  (s-suffix-p ".elc" it)))
       (setq flycheck-files))
      (loaddefs-generate dir
                         out
                         (cons out flycheck-files)))))

(add-hook 'after-save-hook 'my-do-autoload-for-local-projects-files)

(eval-when-compile
  (require 'dash)
  (require 's)
  (require 'f))

(defun my-copy-files (files dest)
  "Copy all FILES into the directory DEST."
  (--each files
    (if (f-dir-p it)
        (ignore-errors
          (copy-directory it
                          (f-join dest (f-filename it))))
      (copy-file it
                 (f-join dest (f-filename it))
                 'ok-if-exists))))

(provide 'my-writing-config)
;;; my-writing-config.el ends here
