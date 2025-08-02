;;; my-vertico.el --- Settings of `vertico': the modern completion -*- lexical-binding: t -*-
;;; Copyright (c) 2023-2025

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
  :bind (:vertico-map
         ("C-M-n" . vertico-next-group)
         ("C-M-p" . vertico-previous-group))
  :init
  ;; it's part of `vertico-mode'
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
    :global-minor-mode t)

  (leaf nerd-icons
    :ensure t)

  (leaf nerd-icons-completion
    :ensure t
    :commands nerd-icons-completion-mode
    ;; `marginalia' and this both use the same way to display info inside `vertico',
    ;; if i load it before `marginalia', then it wasn't working
    :hook marginalia-mode-hook
    :config (add-to-list
             'nerd-icons-completion-category-icons
             '(snippet nerd-icons-octicon "nf-oct-copy" nerd-icons-lred)))

  ;; show a bit of additional info inside the `vertico' `minibuffer'

  (leaf marginalia
    :ensure t
    :global-minor-mode t)

  ;; when I type "~/", the rest text is deleted
  (add-hook 'rfn-eshadow-update-overlay-hook #'vertico-directory-tidy))

;; don't view long items on several lines
(defun my-truncate-lines ()
  "Now truncate lines inside the buffer."
  (interactive)
  (visual-line-mode -1))

(add-hook 'minibuffer-mode-hook #'my-truncate-lines)

(provide 'my-vertico)
;;; my-vertico.el ends here
