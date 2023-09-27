;;; my-straight.el --- My config for support of the `straight' -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2023 semenInRussia

;; Author: semenInRussia <hrams205@gmail.com>
;; Version: 0.1

;;; Commentary:

;; My config for support of the `straight'.

;;; Code:

(declare-function straight-use-package "straight.el")


(eval-when-compile
  ;; `eval-when-compile' really install the package in compile time, it's
  ;; important, because `my-leaf' needs in `straight-use-package' to install
  ;; itself and `leaf' needed in the rest config, because `leaf' macro.
  (defvar bootstrap-version)
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
