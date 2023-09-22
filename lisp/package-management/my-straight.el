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


(eval-when-compile
  ;; `eval-when-compile' really install the package in compile time, it's
  ;; important, because `my-leaf' needs in `straight-use-package' to install
  ;; itself and `leaf' needed in the rest config, because `leaf' macro.
  (defvar bootstrap-version)
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
    (load bootstrap-file nil 'nomessage)))

(provide 'my-straight)
;;; my-straight.el ends here
