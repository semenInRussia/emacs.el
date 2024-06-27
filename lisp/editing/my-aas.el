;;; my-aas.el --- My configuration of the `auto-activating-snippets'

;; Copyright (C) 2022-2024 semenInRussia

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
          (texmathp . texmathp))
  :config
  (aas-set-snippets 'laas-mode
    :cond #'texmathp
    ;; Some Physics Units
    ;; "As" "\\mathrm{А}"
    ;; "Vs"  "\\mathrm{В}"
    ;; "Oms"  "\\mathrm{Ом}"
    "cls" "^\\circ C"

    ;; Some Physics Sheet
    ;; "eqv" "\\mathrm{Экв.}"

    ;; Some Cool Symbols
    "trg" "\\triangle"
    "agl" "\\angle"
    "grd" "^\\circ"
    "xor" "\\oplus"))

(provide 'my-aas)
;;; my-aas.el ends here
