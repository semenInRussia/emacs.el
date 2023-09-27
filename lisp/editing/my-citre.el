;;; my-citre.el --- Support of citre: the full ide which is built over `ctags' -*- lexical-bindings: t -*-

;; Copyright (C) 2023

;;; Commentary:

;; Support of citre: the full ide which is built over `ctags'.
;;
;; It provides auto-completion, find definition and other features like the
;; modern LSPs

;;; Code:

(require 'my-leaf)


(leaf citre
  :ensure t
  :custom (citre-tag-reference-mark . "")
  :bind (;; C-c u is the `citre' prefix
         ("C-c u u" . 'citre-update-this-tags-file)
         ("C-c u n" . 'citre-create-tags-file)
         ;; `citre-peek' is the useful command that show the definition of the
         ;; command/variable in the small popup window below the cursor
         ;;
         ;; bound to "C-c u d"
         ("C-c u d" . 'citre-peek))
  :config (require 'citre-common-tag))

(provide 'my-citre.el)
;;; my-citre.el ends here
