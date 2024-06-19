;;; my-yas.el --- My configuration for the `yasnippet'

;; Copyright (C) 2022-2024 semenInRussia
;; Author: semenInRussia <hrams205@gmail.com>
;; URL: https://github.com/semenInRussia/emacs.el

;; This file is not part of GNU Emacs.

;;; Commentary:

;; My configuration for the `yasnippet'

;;; Code:
(require 'my-leaf)
(require 's)
(require 'dash)


(defvar my-snippets-dir (locate-user-emacs-file "snippets")
  "The directory in which I save snippets for `yasnippet'.")


(leaf yasnippet
  :ensure (yasnippet :repo "joaotavora/yasnippet")
  :defun (yas--fetch
          yas--filter-templates-by-condition
          yas--get-snippet-tables
          yas--namehash-templates-alist
          yas--table-hash
          yas--template-content
          yas-active-keys
          yas-expand
          yas-reload-all)
  :bind ("C-=" . yas-insert-snippet)
  ;; don't use `yas-global-mode', prefer local minor modes
  ;; :global-minor-mode yas-global-mode
  :hook ((prog-mode-hook . yas-minor-mode)
         (text-mode-hook . yas-minor-mode))
  :defvar yas-snippet-dirs
  :custom (yas-wrap-around-region . t)
  :config
  (setq yas-snippet-dirs (list my-snippets-dir))
  ;; don't load snippets instantly after a file opened, wait some AFK
  ;; time
  (run-with-idle-timer 1 nil #'yas-reload-all))

;; a completion for snippets with `cape' (capf)
(leaf my-yas-capf
  :defun cape--properties-table cape--bounds cape-interactive
  :after yasnippet corfu
  :defun my-yas-capf
  :init (add-hook
         'corfu-mode-hook
         (defun my-yas-capf-setup ()
           "Add capf for `yasnippet'."
           (add-hook 'completion-at-point-functions 'my-yas-capf 30 'local))))

(provide 'my-yas)
;;; my-yas.el ends here
