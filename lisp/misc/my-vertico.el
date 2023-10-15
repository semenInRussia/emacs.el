;;; my-vertico.el --- Settings of `vertico': the modern completion -*- lexical-binding: t -*-
;;; Copyright (c) 2023

;;; Commentary:

;; Settings of `vertico': the modern completion.

;;; Code:

(require 'my-leaf)


(leaf vertico
  :ensure (vertico :host github
                   :repo "minad/vertico"
                   :files ("*.el" "extensions/*.el"))
  :commands vertico--advice
  :defun vertico-directory-tidy
  :custom ((vertico-count . 6)
           (enable-recursive-minibuffers . t))
  ;; it's part of `vertico-mode'
  :init
  (advice-add 'completing-read-default :around #'vertico--advice)
  (advice-add 'completing-read-multiple :around #'vertico--advice)

  :defun vertico-mode
  :config (vertico-mode t)

  :config
  ;; I press `M-delete' to go the up directory inside of `vertico'
  ;; and TAB to enter into the directory.
  (leaf vertico-directory
    :bind (:vertico-map
           :package vertico
           ;; instead I press TAB
           ;; ("RET" . vertico-directory-enter)
           ("DEL" . vertico-directory-delete-char)
           ("M-DEL" . vertico-directory-delete-word)))

  ;; scroll, mouse inside the `vertico' buffer
  (leaf vertico-mouse
    :global-minor-mode vertico-mouse-mode)

  (leaf nerd-icons
    :ensure t)

  (leaf nerd-icons-completion
    :ensure t
    :commands nerd-icons-completion-mode
    ;; `marginalia' and this both use the same way to display info inside `vertico',
    ;; if i load it before `marginalia', then it wasn't working
    :hook marginalia-mode-hook)

  ;; show a bit of additional info inside the `vertico' `minibuffer'
  (leaf marginalia
    :ensure t
    :global-minor-mode marginalia-mode)

  ;; when I type "~/", the rest text is deleted
  (add-hook 'rfn-eshadow-update-overlay-hook #'vertico-directory-tidy))

(provide 'my-vertico)
;;; my-vertico.el ends here
