;;; my-latex.el --- My config for LaTeX

;; Copyright (C) 2022 Semen Khramtsov

;; Author: Semen Khramtsov <hrams205@gmail.com>
;; Version: 0.1
;; URL: https://github.com/semenInRussia/emacs.el

;;; Commentary:

;; My config for LaTeX.

;;; Code:

(require 'my-leaf)
(require 'f)
(require 'dash)
(require 'smartparens)


(defvar latex-documentclasses
  '("article"
    "reoport"
    "book"
    "proc"
    "minimal"
    "slides"
    "memoir"
    "letter"
    "beamer")
  "List of the names for built-in LaTeX documentclasses.")

(add-hook 'LaTeX-mode-hook 'my-latex-disable-auto-fill)

(leaf auctex
  :ensure (auctex :repo "emacs-straight/auctex" :host github)
  :mode ("\\.tex$" . latex-mode))

(leaf tex-mode
  :defun ((er/mark-LaTeX-math er/mark-LaTeX-inside-environment) . expand-region)
  :bind (:latex-mode-map
         :package tex-mode
         ("C-c C-@"  . my-latex-mark-inside-environment-or-math)
         ("C-c C-\\" . my-latex-equation-to-split)
         ("C-c C-w"  . my-latex-kill-section))
  :config
  (leaf xenops
    :ensure (xenops :repo "dandavison/xenops")
    :hook LaTeX-mode-hook
    :custom (xenops-math-image-scale-factor . 2))

  ;; (leaf my-latex-insert
  ;;   :load-path* "lisp/languages/latex/"
  ;;   :hook (LaTeX-mode-hook . my-latex-expansion-mode))

  (leaf laas
    :ensure (laas :repo "tecosaur/LaTeX-auto-activating-snippets" :host github)
    :hook LaTeX-mode-hook
    :aas (laas-mode
          :cond #'texmathp
          ;; Some Physics Units
          "As" "\\mathrm{А}"
          "Vs"  "\\mathrm{В}"
          "Oms"  "\\mathrm{Ом}"
          "cls" "^\\circ C"

          ;; Some Physics Sheet
          "eqv" "\\mathrm{Экв.}"

          ;; Some Cool Symbols
          "trg" "\\triangle"
          "agl" "\\angle"
          "grd" "^\\circ"))

  (leaf cdlatex
    :ensure (cdlatex :repo "cdominik/cdlatex" :host github)
    :hook (LaTeX-mode-hook  . turn-on-cdlatex)
    :bind (:cdlatex-mode-map
           ("<tab>" . cdlatex-tab)
           (";" . my-latex-dollar))
    :custom ((cdlatex-math-modify-alist
              .
              '((?q "\\sqrt" nil t nil nil)
                (?u "\\breve" "\\uline" t nil nil)
                (?v "\\vec" nil t nil nil)))))

  (leaf my-latex-insert
    :bind (:latex-mode-map
           :package tex-mode
           ("C-=" . my-latex-insert-any)))

  (leaf my-latex-embrace
    :after embrace
    :defun my-embrace-LaTeX-mode-hook
    :defun (embrace-LaTeX-mode-hook . embrace)
    :config
    (add-hook 'LaTeX-mode-hook #'embrace-LaTeX-mode-hook)
    (add-hook 'LaTeX-mode-hook #'my-embrace-LaTeX-mode-hook)
    (when (eq major-mode 'latex-mode)
      (embrace-LaTeX-mode-hook)
      (my-embrace-LaTeX-mode-hook)))

  (leaf my-latex-math-spaces
    :hook latex-mode-hook)

  ;; (leaf latex-r
  ;;   :load-path "~/projects/latex-r"
  ;;   :bind (:latex-mode-map
  ;;          :package latex
  ;;          ("C-c M-n" . 'latex-r-cycle-math-parens)
  ;;          ("C-c C-s" . 'latex-r-split-environment)))

  (defun my-latex-disable-auto-fill ()
    "Disable `auto-fill-mode'."
    (interactive)
    (auto-fill-mode 0))

  ;; (leaf my-latex-drag
  ;;   :after my-drag
  ;;   :defun ((add-up-dragger add-down-dragger) . my-drag)
  ;;   :commands (my-latex-try-drag-left-list-item
  ;;              my-latex-try-drag-right-list-item)
  ;;   ;; eval after `my-drag' is loaded
  ;;   :init
  ;;   (add-up-dragger 'my-latex-try-drag-left-list-item)
  ;;   (add-down-dragger 'my-latex-try-drag-right-list-item))

  (setopt TeX-fold-macro-spec-list '(("{1}" ("emph")))))

(provide 'my-latex)
;;; my-latex.el ends here
