;;; my-writing-config.el --- My configuration for the writing other configuration -*- lexical-binding: t; -*-
;; Copyright (C) 2022-2024 semenInRussia
;; Author: semenInRussia <hrams205@gmail.com>

;;; Commentary:
;; My configuration for the writing other configuration.

;;; Code:
(require 'my-leaf)

(declare-function inspector-inspect "inspector.el")
(declare-function my-autoautoload-local-mode "my-config-funcs")
(declare-function my-build-config "my-config-funcs.el")
(declare-function my-byte-compile-local-projects-autoloads "my-config-funcs.el")
(declare-function my-new-config-module "my-config-funcs")
(declare-function my-restart-build "my-config-funcs")


(leaf ecukes
  :ensure t
  :bind (:feature-mode-map
         :package feature-mode
         ("C-c C-e" . ecukes))
  :hook (ecukes-mode-hook . my-feature-mode-hook)
  :config (leaf espuds :ensure t :require t))

(leaf leaf
  :bind ("C-x M-f" . 'leaf-find))

;; (add-hook 'after-save-hook 'my-do-autoload-for-local-projects-files)
(add-hook 'emacs-lisp-mode #'my-autoautoload-local-mode)

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
