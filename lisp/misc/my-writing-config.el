;;; my-writing-config.el --- My configuration for the writing configuration -*- lexical-binding: t; -*-
;; Copyright (C) 2022-2025 semenInRussia

;;; Commentary:
;; My configuration for the writing configuration.

;;; Code:
(require 'my-leaf)

(declare-function my-autoautoload-local-mode "my-config-funcs")
(declare-function my-build-config "my-config-funcs.el")
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
(add-hook 'emacs-lisp-mode-hook #'my-autoautoload-local-mode)

(defvar-keymap my-config-map
  ;; Build and Restart
  "b" #'my-build-config
  "r" #'restart-emacs
  "R" #'my-restart-build
  "B" #'my-restart-build
  ;; create new configuration file with template
  "n" #'my-new-config-module
  ;; sometimes it's useful to check times that modules took time
  "t" #'my-require-times
  ;; pam (package manager)
  "RET" #'pam-install-everything-mode
  "p" #'pam-use-package)

(global-set-key (kbd "C-c e") my-config-map)

;; settings of byte-compile remarks (warnings)
(with-eval-after-load 'bytecomp
  (setq byte-compile-warnings 'all))

(provide 'my-writing-config)
;;; my-writing-config.el ends here
