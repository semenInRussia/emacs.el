;;; my-straight.el --- My config for support of the `straight' -*- lexical-binding: t; -*-

;; Copyright (C) 2022 semenInRussia

;; Author: semenInRussia <hrams205@gmail.com>
;; Version: 0.1

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

;; My config for support of the `straight'.

;;; Code:

(declare-function straight-use-package "straight.el")


(eval-and-compile
  ;; `eval-and-compile' really install the package in compile time,
  ;; it's important, because `my-leaf' needs in `straight-use-package' to install
  ;; itself and `leaf' needed in the rest config, because `leaf' macro
  (defvar my-straight-packages-already-installed-p (not (member "--install" command-line-args)))
  (defvar bootstrap-version)
  (setq straight-find-executable "C:\\tools\\find.exe")
  (setq straight-check-for-modifications nil)
  (let ((bootstrap-file
         (expand-file-name
          "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
        (bootstrap-version 6))
    (unless (file-exists-p bootstrap-file)
      (with-current-buffer (url-retrieve-synchronously
                            "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
                            'silent 'inhibit-cookies)
        (goto-char (point-max))
        (eval-print-last-sexp)))
    (load bootstrap-file nil 'nomessage))

  (declare-function straight--convert-recipe "straight.el")
  (declare-function straight--add-package-to-load-path "straight.el")
  (declare-function straight--add-package-to-info-path "straight.el")
  (declare-function straight--file "straight.el")
  (declare-function straight--load-package-autoloads "straight.el")
  (declare-function straight--compute-dependencies "straight.el")

  ;; don't build packages, think that they're already installed
  (unless my-straight-packages-already-installed-p
    (advice-add 'straight--package-might-be-modified-p :override 'ignore))

  ;; but after init, new packages can be installed, so packages can be
  ;; (re)builded
  (add-hook 'after-init-hook
            (lambda ()
              (advice-remove 'straight--package-might-be-modified-p 'ignore)))

  (defun my-straight-load-package (melpa-style-recipe)
    "Load a package with a given MELPA-STYLE-RECIPE.

Loading a package means load autoloads (if they exists) and add a package source
root into `load-path'.  It doesn't install package and doesn't generate
autoloads file, if one of them wasn't installed/genrated, then nothing happened,
you should ensure that it was installed with `straight-use-package'.  Also it
doesn't load source code, only autoloads.  Also it tries add TeXinfo
documentation into info load path, if provided of course.

To load whole package (not only autoloads) use `require', after call this
function that add package to the load path `load-path'.

MELPA-STYLE-RECIPE specifies a package, suggest you check offical straight
documentation.

If called interactively, then choose a package from the installed packages, that
placed at the straight build directory"
    (interactive
     ;; this interactive statement copied from `straight-use-package' impl
     (list
      (intern
       (completing-read
        "Which recipe? "
        (cddr ;; it skips "." and ".." dirs
         (directory-files (straight--file
                           straight-base-dir
                           "straight"
                           straight-build-dir)))))))
    (unless my-straight-packages-already-installed-p
      (setq straight-find-executable "C:\\tools\\find.exe")
      (straight-use-package melpa-style-recipe))))

(provide 'my-straight)
;;; my-straight.el ends here
