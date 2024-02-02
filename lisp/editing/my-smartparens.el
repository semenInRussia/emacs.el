;;; my-smartparens.el --- My configuration for the `smartparens'

;; Copyright (C) 2022-2024 semenInRussia

;;; Commentary:

;; My configuration for the `smartparens': insert smartparens smart way

;;; Code:

(require 'my-leaf)
(require 'my-lib)


(leaf smartparens
  :ensure (smartparens :repo "Fuco1/smartparens" :host github)
  :global-minor-mode smartparens-global-mode
  :defun (sp-clone-sexp sp-use-paredit-bindings)
  :defvar sp-lisp-modes
  :bind (:smartparens-mode-map
         ("C-k" . 'sp-kill-hybrid-sexp)
         ("C-c DEL" . 'sp-change-enclosing))
  :config
  ;; emacs is lisp hacking environment, so we set up some most common
  ;; lisp modes too
  (sp-use-paredit-bindings)

  (defun my-sp-clone ()
    (interactive)
    (sp-clone-sexp)
    (repeat-at-last-keystroke))

  (defun delete-only-1-char ()
    "Delete only 1 character before point."
    (interactive)
    (backward-char)
    (delete-char 1)))

(leaf smartparens
  :defvar (sp--html-modes sp-lisp-modes)
  :config
  (eval-after-load 'cc-mode                  '(require 'smartparens-c))
  (eval-after-load 'clojure-mode             '(require 'smartparens-clojure))
  (eval-after-load 'crystal-mode             '(require 'smartparens-crystal))
  (eval-after-load 'elixir-mode              '(require 'smartparens-elixir))
  (eval-after-load 'elixir-ts-mode           '(require 'smartparens-elixir))
  (eval-after-load 'enh-ruby-mode            '(require 'smartparens-ruby))
  (eval-after-load 'erlang-mode              '(require 'smartparens-erlang))
  (eval-after-load 'ess                      '(require 'smartparens-ess))
  (eval-after-load 'go-mode                  '(require 'smartparens-go))
  (eval-after-load 'haskell-interactive-mode '(require 'smartparens-haskell))
  (eval-after-load 'haskell-mode             '(require 'smartparens-haskell))
  (--each sp--html-modes
    (eval-after-load it                      '(require 'smartparens-html)))
  (eval-after-load 'latex                    '(require 'smartparens-latex))
  (eval-after-load 'lua-mode                 '(require 'smartparens-lua))
  (eval-after-load 'markdown-mode            '(require 'smartparens-markdown))
  (--each '(python-mode python)
    (eval-after-load it                      '(require 'smartparens-python)))
  (eval-after-load 'org                      '(require 'smartparens-org))
  (eval-after-load 'racket-mode              '(require 'smartparens-racket))
  (eval-after-load 'rst                      '(require 'smartparens-rst))
  (eval-after-load 'ruby-mode                '(require 'smartparens-ruby))
  (eval-after-load 'rust-mode                '(require 'smartparens-rust))
  (eval-after-load 'rustic                   '(require 'smartparens-rust))
  (eval-after-load 'scala-mode               '(require 'smartparens-scala))
  (eval-after-load 'swift-mode               '(require 'smartparens-swift))
  (eval-after-load 'tex-mode                 '(require 'smartparens-latex))
  (eval-after-load 'text-mode                '(require 'smartparens-text))
  (eval-after-load 'tuareg                   '(require 'smartparens-ml))
  (eval-after-load 'fsharp-mode              '(require 'smartparens-ml))
  (eval-after-load 'unisonlang-mode          '(require 'smartparens-unison))
  (--each '(js js2-mode)
    (eval-after-load it                      '(require 'smartparens-javascript))))

(provide 'my-smartparens)
;;; my-smartparens.el ends here
