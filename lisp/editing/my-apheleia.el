;;; my-apheleia.el --- My configuration of the `apheleia': auto format of source code after save

;; Copyright (C) 2022 Semen Khramtsov

;; Author: Semen Khramtsov <hrams205@gmail.com>
;; Version: 0.1
;; URL: https://github.com/semenInRussia/emacs.el

;;; Commentary:

;; My configuration of the `apheleia': format source on save.

;;; Code:

(require 'my-leaf)
(require 'f)

(defvar uncrustify-cfg-file (f-full "~/uncrustify.cfg"))


(leaf apheleia
  :ensure (apheleia :repo "radian-software/apheleia"
                    :host github)
  :defvar (apheleia-formatters apheleia-mode-alist)
  :hook (;; I construct a hook list from the `apheleia-mode-alist' variable
         ;; it's better than a `apheleia-global-mode', because it will be loaded after the
         ;; major mode is entered
         ;;
         ;; php-mode has to come before cc-mode
         (php-mode-hook . apheleia-mode)
         ;; json-mode has to come before javascript-mode (aka-hook js-mode)
         (json-mode-hook . apheleia-mode)
         (json-ts-mode-hook . apheleia-mode)
         ;; rest are alphabetical
         (asm-mode-hook . apheleia-mode)
         (awk-mode-hook . apheleia-mode)
         (bash-ts-mode-hook . apheleia-mode)
         (bazel-mode-hook . apheleia-mode)
         (beancount-mode-hook . apheleia-mode)
         (c++-ts-mode-hook . apheleia-mode)
         (caddyfile-mode-hook . apheleia-mode)
         (cc-mode-hook . apheleia-mode)
         (c-mode-hook . apheleia-mode)
         (c-ts-mode-hook . apheleia-mode)
         (c++-mode-hook . apheleia-mode)
         (caml-mode-hook . apheleia-mode)
         (cmake-mode-hook . apheleia-mode)
         (cmake-ts-mode-hook . apheleia-mode)
         (common-lisp-mode-hook . apheleia-mode)
         (crystal-mode-hook . apheleia-mode)
         (css-mode-hook . apheleia-mode)
         (css-ts-mode-hook . apheleia-mode)
         (dart-mode-hook . apheleia-mode)
         (dart-ts-mode-hook . apheleia-mode)
         (elixir-mode-hook . apheleia-mode)
         (elixir-ts-mode-hook . apheleia-mode)
         (elm-mode-hook . apheleia-mode)
         (fish-mode-hook . apheleia-mode)
         (go-mode-hook . apheleia-mode)
         (go-mod-ts-mode-hook . apheleia-mode)
         (go-ts-mode-hook . apheleia-mode)
         (graphql-mode-hook . apheleia-mode)
         (haskell-mode-hook . apheleia-mode)
         (html-mode-hook . apheleia-mode)
         (html-ts-mode-hook . apheleia-mode)
         (java-mode-hook . apheleia-mode)
         (java-ts-mode-hook . apheleia-mode)
         (js3-mode-hook . apheleia-mode)
         (js-mode-hook . apheleia-mode)
         (js-ts-mode-hook . apheleia-mode)
         (kotlin-mode-hook . apheleia-mode)
         (latex-mode-hook . apheleia-mode)
         (LaTeX-mode-hook . apheleia-mode)
         (lua-mode-hook . apheleia-mode)
         (lisp-mode-hook . apheleia-mode)
         (nasm-mode-hook . apheleia-mode)
         (nix-mode-hook . apheleia-mode)
         (perl-mode-hook . apheleia-mode)
         (purescript-mode-hook . apheleia-mode)
         (python-mode-hook . apheleia-mode)
         (python-ts-mode-hook . apheleia-mode)
         (ruby-mode-hook . apheleia-mode)
         (ruby-ts-mode-hook . apheleia-mode)
         (rustic-mode-hook . apheleia-mode)
         (rust-mode-hook . apheleia-mode)
         (rust-ts-mode-hook . apheleia-mode)
         (scss-mode-hook . apheleia-mode)
         (svelte-mode-hook . apheleia-mode)
         (terraform-mode-hook . apheleia-mode)
         (TeX-latex-mode-hook . apheleia-mode)
         (TeX-mode-hook . apheleia-mode)
         (tsx-ts-mode-hook . apheleia-mode)
         (tuareg-mode-hook . apheleia-mode)
         (typescript-mode-hook . apheleia-mode)
         (typescript-ts-mode-hook . apheleia-mode)
         (web-mode-hook . apheleia-mode)
         (yaml-mode-hook . apheleia-mode)
         (yaml-ts-mode-hook . apheleia-mode))
  :setf (;; a formatter for C++
         ((alist-get 'c++-mode apheleia-mode-alist)
          . 'uncrustify)
         ((alist-get 'c-mode apheleia-mode-alist)
          . 'uncrustify)
         ((alist-get 'uncrustify apheleia-formatters)
          . '("uncrustify" "-f" filepath "-c" uncrustify-cfg-file "-o"))
         ;; find the --edition of "rustfmt"
         ((alist-get 'rustfmt apheleia-formatters)
          . '("rustfmt" "--edition" "2021" "--quiet" "--emit" "stdout"))
         ;; use black+isort
         ((alist-get 'python-mode apheleia-mode-alist)
          . '(isort black))))

(provide 'my-apheleia)
;;; my-apheleia.el ends here
