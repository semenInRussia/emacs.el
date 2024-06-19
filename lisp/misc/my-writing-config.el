;;; my-writing-config.el --- My configuration for the writing other configuration -*- lexical-binding: t; -*-
;; Copyright (C) 2022-2024 semenInRussia
;; Author: semenInRussia <hrams205@gmail.com>

;;; Commentary:
;; My configuration for the writing other configuration.

;;; Code:
(require 'my-leaf)

(require 'dash)
(require 'f)
(require 's)
(require 'my-lib)

(declare-function inspector-inspect "inspector.el")
(declare-function my-build-config "my-build-config")
(declare-function my-byte-compile-local-projects-autoloads "my-config-funcs.el")

(defun my-new-config-module (module-name &optional directory)
  "Create a new configuration file named MODULE-NAME in the DIRECTORY.

DIRECTORY defaults to ~/.emacs.d/lisp/"
  (interactive "sName of the configuration module: \nDDirectory: ")
  (setq directory (or directory user-emacs-directory))
  (->> module-name
       (s-append ".el")
       (s-prepend "my-")
       (s-prepend directory)
       find-file)
  (insert
   (s-replace
    "writing-config"
    module-name
    (format
     ";;; my-writing-config.el --- My configuration of writing-config -*- lexical-binding: t; -*-

;; Copyright (C) %s semenInRussia
;; Author: semenInRussia <hrams205@gmail.com>

;;; Commentary:
;; My configuration of writing-config.

;;; Code:
(require 'my-leaf)

(leaf writing-config)

(provide 'my-writing-config)
;;; my-writing-config.el ends here"
     (my-current-year))))
  (search-backward "(leaf "))

(leaf ecukes
  :ensure t
  :bind (:feature-mode-map
         :package feature-mode
         ("C-c C-e" . ecukes))
  :hook (ecukes-mode-hook . my-feature-mode-hook)
  :config (leaf espuds :ensure t :require t))

(leaf leaf
  :bind ("C-x M-f" . 'leaf-find))

(defvar my-local-project-was-updated nil
  "Variable is non-nil if one of my \"local-project\" was edited.

If before exit Emacs this variable is non-nil byte-compile
local-projects autoloads.

Will be changed automatically if you use
`my-do-autoload-for-local-projects-files'")

(add-hook 'after-save-hook 'my-do-autoload-for-local-projects-files)
(add-hook 'kill-emacs-hook
          (defun my-maybe-byte-compile-local-projects-autoloads ()
            (when my-local-project-was-updated
              (my-byte-compile-local-projects-autoloads))))

(defun my-restart-build ()
  "Restart Emacs with rebuild the config before."
  (interactive)
  (my-build-config)
  (restart-emacs))

(defvar-keymap my-config-map
  ;; Build and Restart
  "b" #'my-build-config
  "r" #'restart-emacs
  "R" #'my-restart-build
  "B" #'my-restart-build

  "n" #'my-new-config-module
  "t" 'my-require-times)
(global-set-key (kbd "C-c e") my-config-map)

;; settings of byte-compile remarks (warnings)
(with-eval-after-load 'bytecomp
  (setq byte-compile-warnings 'all))

(provide 'my-writing-config)
;;; my-writing-config.el ends here
