;;; my-aas.el --- My configuration of the `auto-activating-snippets' -*- lexical-binding: t -*-
;; Copyright (C) 2022-2025 semenInRussia

;;; Commentary:
;; My configuration of the `auto-activating-snippets'

;;; Code:
(require 'my-leaf)

(leaf aas
  :ensure t
  ;; don't use global mode, because `aas' is used with me only inside
  ;; a few amount of major modes
  ;;
  ;; :global-minor-mode aas-global-mode
  ;;
  ;; instead do it:
  :hook ((latex-mode-hook . aas-mode)
         (TeX-latex-mode-hook . aas-mode)))

(leaf laas
  :ensure t
  :hook LaTeX-mode-hook
  :defun ((aas-set-snippets . aas)
          (texmathp . texmathp)
          laas-wrap-previous-object
          laas-object-on-left-condition)
  :config
  (aas-set-snippets 'laas-mode
    :cond #'texmathp
    ;; Some Physics Units
    ;; "As" "\\mathrm{А}"
    ;; "Vs"  "\\mathrm{В}"
    ;; "Oms"  "\\mathrm{Ом}"
    "cls" "^\\circ C"
    "oo" "\\circ"

    ;; Some Physics Sheet
    ;; "eqv" "\\mathrm{Экв.}"

    ;; Some Cool Symbols
    "trg" "\\triangle"
    "agl" "\\angle"
    "grd" "^\\circ"
    "xor" "\\oplus"
    "ss" "\\subset"
    "lor" "\\lor"
    "land" "\\land"
    "opp" "\\circ"
    "sum" "\\sum"
    "prod" "\\prod"
    "intt" "\\int"
    "BB" (lambda () (interactive) (yas-expand-snippet "{$0 \\choose }"))
    "^" (lambda () (interactive) (insert "^{}") (forward-char -1))
    "mod" (lambda () (interactive) (insert "\\pmod{}") (forward-char -1))
    :cond #'laas-object-on-left-condition
    "bb" (lambda () (interactive) (laas-wrap-previous-object "mathbb"))))

(provide 'my-aas)
;;; my-aas.el ends here
