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

(require 'dash)
(require 's)

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
    ";;; my-writing-config.el --- My configuration of `writing-config' -*- lexical-binding: t; -*-

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

;; My configuration of `writing-config'.

;;; Code:

(leaf writing-config)

(provide 'my-writing-config)
;;; my-writing-config.el ends here"))
  (search-backward "(leaf "))

(eval-after-load 'fast-exec
  '(progn
     (fast-exec-bind writing-config
       (fast-exec-make-some-commands
        ("New Config Module" 'my-new-config-module)))))

(leaf leaf                            ;nofmt
  :bind (:xah-fly-command-map         ;nofmt
         :package xah-fly-keys
         ("SPC SPC j" . leaf-find)))

(leaf leaf-convert                     ;nofmt
  :bind (:my-elisp-local-map
         :package elisp-mode
         ("l" . my-leaf-convert-region)
         ("v" . my-leaf-convert-clipboard))
  :defun (my-writing-config . (my-leaf-convert-from-string-to-string))
  :config                               ;nofmt
  (defun my-leaf-convert-region (beg end)
    "Read a sexp beetween BEG and END points convert it to `leaf' and replace."
    (interactive "r")
    (let* ((starting-sexp               ;nofmt
            (just-text-in-region))
           (leaf-sexp
            (my-leaf-convert-from-string-to-string starting-sexp)))
      (delete-region beg end)
      (insert leaf-sexp)))

  (defun my-leaf-convert-from-string-to-string (string)
    "Convert a Lisp source as STRING to source using `leaf' as string."
    (->>
     string
     (s-prepend "(progn")
     (s-append ")")
     (read)
     (cons 'leaf-convert)
     (eval)
     (format "%s")))

  (defun my-leaf-convert-clipboard ()
    "Read a sexp from the kill ring, convert it to `leaf' format insert it."
    (interactive)
    (insert (my-leaf-convert-from-string-to-string (current-kill 0)))))

(leaf ecukes
  :ensure t
  :bind (:my-feature-local-map :package feature-mode ("e" . ecukes))
  :hook (ecukes-mode-hook . my-feature-mode-hook)
  :config (leaf espuds :ensure t :require t))

(provide 'my-writing-config)
;;; my-writing-config.el ends here
