;;; my-latex.el --- My config for LaTeX -*- lexical-binding: t -*-

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
  :mode ("\\.tex$" . latex-mode)
  :defer-config
  (add-hook 'LaTeX-mode-hook 'visual-line-mode))

(leaf xenops
  :ensure t
  :custom (xenops-math-image-scale-factor . 2)
  :bind (:LaTeX-mode-map
         :package tex-mode
         ("C-c C-p" . xenops-mode)))

;; (leaf my-latex-insert
;;   :hook (LaTeX-mode-hook . my-latex-expansion-mode))

(leaf cdlatex
  :ensure (cdlatex :repo "cdominik/cdlatex" :host github)
  :hook (LaTeX-mode-hook . turn-on-cdlatex)
  :bind (:cdlatex-mode-map
         ("<tab>" . cdlatex-tab)
         ("C-'" . cdlatex-math-modify))
  :custom ((cdlatex-math-modify-alist
            . '((?q "\\sqrt" nil t nil nil)
                (?u "\\breve" "\\uline" t nil nil)
                (?v "\\vec" nil t nil nil)))))

(leaf my-latex-insert
  :bind (:latex-mode-map
         :package tex-mode
         ("C-=" . my-latex-insert-any)))

;; (leaf my-latex-math-spaces
;;   :hook latex-mode-hook)

;; (leaf latex-r
;;   :load-path "~/projects/latex-r"
;;   :bind (:latex-mode-map
;;          :package latex
;;          ("C-c M-n" . 'latex-r-cycle-math-parens)
;;          ("C-c C-s" . 'latex-r-split-environment)))

(provide 'my-latex)
;;; my-latex.el ends here
