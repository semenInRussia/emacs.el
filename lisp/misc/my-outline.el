;;; my-outline.el --- My configuration of `outline' -*- lexical-binding: t; -*-
;; Copyright (C) 2022-2023 semenInRussia

;;; Commentary:

;; My configuration of `outline'.

;;; Code:
(require 'my-leaf)
(require 'my-lib)


(leaf outline
  :bind (:outline-minor-mode-map
         ("S-TAB" . outline-cycle)))

(provide 'my-outline)
;;; my-outline.el ends here
