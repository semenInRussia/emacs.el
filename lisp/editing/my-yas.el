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
;; (leaf my-yas-capf
;;   :after yasnippet corfu
;;   :defun my-yas-capf
;;   :init (add-hook
;;          'corfu-mode-hook
;;          (defun my-yas-capf-setup ()
;;            "Add capf for `yasnippet'."
;;            (add-hook 'completion-at-point-functions #'my-yas-capf 30 'local))))

(leaf embark
  :doc "Support of `embark' for `yasnippet', for example when I hit C-=."
  :after marginalia embark
  :defvar (marginalia-prompt-categories
           embark-keymap-alist
           embark-general-map)
  :defun (yas-visit-snippet-file . yasnippet)
  :config
  (defvar-keymap my-yasnippet-actions
    :parent embark-general-map
    "v" #'yas-visit-snippet-file
    "I" #'yas-insert-snippet)

  (add-to-list 'embark-keymap-alist '(snippet . my-yasnippet-actions))
  (add-to-list 'marginalia-prompt-categories '("Choose a snippet" . snippet)))

;; (eval-when-compile
;;   (require 'marginalia))
;; (leaf marginalia
;;   :defer-config
;;   (declare-function yas--template-name "yasnippet")
;;   (declare-function yas--template-group "yasnippet")
;;   (defun my-marginalia-annotate-snippet (key)
;;     "Annotate `yasnippet' snippet with KEY with name and type."
;;     (when (boundp 'yas--tables)
;;       (let* ((snip (cdar (yas--fetch (gethash major-mode yas--tables) key)))
;;              (name (and snip (yas--template-name snip)))
;;              (group (and snip (yas--template-group snip))))
;;         (marginalia--fields
;;          ((concat group
;;                   #(" " 0 1 (display (space :align-to center))))
;;           :face 'marginalia-documentation)
;;          (name
;;           :face 'marginalia-documentation)))))

;;   (add-to-list 'marginalia-annotator-registry
;;                '(snippet my-marginalia-annotate-snippet builtin none)))

(provide 'my-yas)
;;; my-yas.el ends here
