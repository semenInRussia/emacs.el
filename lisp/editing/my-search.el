;;; my-search.el --- My configuration of the search

;; Copyright (C) 2022-2024 semenInRussia

;;; Commentary:

;; My configuration of the search

;;; Code:
(require 'my-leaf)


;; a bit cooler `isearch'
(leaf ctrlf
  :ensure (ctrlf :repo "radian-software/ctrlf" :host github)
  :bind ("C-s" . ctrlf-forward-default))

;; a bit cooller `query-replace'
(leaf visual-regexp
  :ensure (visual-regexp :repo "benma/visual-regexp.el" :host github)
  ;; i'm use % in `meow', it's call it
  :bind ("M-%" . vr/query-replace))

;; amazing ripgrep integration, but i use `consult-ripgrep' in the most of cases.
;;
;; `deadgrep' is useful when I need to find all occurences of word
;; (for example "hot dog") and check every occurence
(leaf deadgrep
  :ensure (deadgrep :repo "Wilfred/deadgrep" :host github)
  :bind ("C-c S" . deadgrep))

;; see also `my-consult': `consult-line', `consult-ripgrep'

(provide 'my-search)
;;; my-search.el ends here
