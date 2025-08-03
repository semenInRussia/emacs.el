;;; my-latex.el --- My config for LaTeX

;; Copyright (C) 2022-2025 semenInRussia

;;; Commentary:

;; My config for LaTeX.

;;; Code:

(require 'my-leaf)
(require 'f)
(require 'dash)
(require 'smartparens)


(leaf auctex
  :ensure (auctex :repo "emacs-straight/auctex" :host github)
  :mode ("\\.tex$" . latex-mode))

(leaf tex-mode
  :defun (texmathp
          ((er/mark-LaTeX-math er/mark-LaTeX-inside-environment) . expand-region))
  :bind (:LaTeX-mode-map
         :package tex-mode
         ("C-y" . yank))
  :config
  (leaf xenops
    :ensure t
    :hook LaTeX-mode-hook
    :custom (xenops-math-image-scale-factor . 2))

  ;; (leaf my-latex-insert
  ;;   :hook (LaTeX-mode-hook . my-latex-expansion-mode))

  (leaf cdlatex
    :ensure (cdlatex :repo "cdominik/cdlatex" :host github)
    :hook (LaTeX-mode-hook . turn-on-cdlatex)
    :bind (:cdlatex-mode-map
           ("<tab>" . cdlatex-tab))
    :custom ((cdlatex-math-modify-alist
              . '((?q "\\sqrt" nil t nil nil)
                  (?u "\\breve" "\\uline" t nil nil)
                  (?v "\\vec" nil t nil nil)))))

  (leaf my-latex-insert
    :bind (:latex-mode-map
           :package tex-mode
           ("C-=" . my-latex-insert-any)))

  (leaf my-latex-math-spaces
    :hook latex-mode-hook)

  ;; (leaf latex-r
  ;;   :load-path "~/projects/latex-r"
  ;;   :bind (:latex-mode-map
  ;;          :package latex
  ;;          ("C-c M-n" . 'latex-r-cycle-math-parens)
  ;;          ("C-c C-s" . 'latex-r-split-environment)))

  ;; (defun my-latex-disable-auto-fill ()
  ;;   "Disable `auto-fill-mode'."
  ;;   (interactive)
  ;;   (auto-fill-mode 0))
  )

(provide 'my-latex)
;;; my-latex.el ends here
