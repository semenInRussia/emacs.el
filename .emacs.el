(load "~/init.el")
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-show-quick-access t nil nil "Customized with use-package company")
 '(custom-safe-themes
   '("028c226411a386abc7f7a0fba1a2ebfae5fe69e2a816f54898df41a6a3412bb5" "1704976a1797342a1b4ea7a75bdbb3be1569f4619134341bd5a4c1cfb16abad4" "23c806e34594a583ea5bbf5adf9a964afe4f28b4467d28777bcba0d35aa0872e" "1d5e33500bc9548f800f9e248b57d1b2a9ecde79cb40c0b1398dec51ee820daf" "cbdf8c2e1b2b5c15b34ddb5063f1b21514c7169ff20e081d39cf57ffee89bc1e" "da186cce19b5aed3f6a2316845583dbee76aea9255ea0da857d1c058ff003546" "a9a67b318b7417adbedaab02f05fa679973e9718d9d26075c6235b1f0db703c8" "4a5aa2ccb3fa837f322276c060ea8a3d10181fecbd1b74cb97df8e191b214313" "7a7b1d475b42c1a0b61f3b1d1225dd249ffa1abb1b7f726aec59ac7ca3bf4dae" "e19ac4ef0f028f503b1ccafa7c337021834ce0d1a2bca03fcebc1ef635776bea" "c4063322b5011829f7fdd7509979b5823e8eea2abf1fe5572ec4b7af1dd78519" "2673172038d5c7c6cb73e72dbc7f9f2a8ff66ad6cfe87d213b4ede4d1963e983" "15c821db2a9d52d6f27d0251256a75d4ec681f03dd6beaf4cda2fdae3ca170f0" "04e572ffdfa97caca9725c87a6138062b6a9428a88072969b26fea07f34dfdf9" "0069f9fc3e624e7d0a6deb780be540122c718bba6db4e0bf74cb6be33fd0f24f" "3cdcf69aa157c058aa8ed16a8e29f1b988c24bcf2f2bf9473afe078436051a09" "0466adb5554ea3055d0353d363832446cd8be7b799c39839f387abb631ea0995" default))
 '(doom-modeline-mode t nil nil "Customized with use-package doom-modeline")
 '(frame-brackground-mode 'dark)
 '(helm-completion-style 'helm)
 '(package-selected-packages
   '(deft xenops scribble-mode racket-mode csharp-mode yaml-mode aggresive-indent-mode helm-mode-manager org-cliplink counsel org-ql helm-org-ql visual-ascii-mode helm-spotify helm-sotify ht org-db drag-stuff drug-stuff org-show ox-clip org-roam stupid-indent-mode el-fly-indent-mode datetime cdlatex minions mood-line simple-modeline aggressive-fill-paragraph aggressive-fill-paragraph-mode elfmt helm-rg cowsay run-command-recipes friendly-shell-command friendly-shell flycheck-rust racer forge markdown-toc pdf-tools pacmacs imenu-anywhere auto-yasnippet page-break-lines syntax-subword super-save css-eldoc elisp-format company-go preview-latex emmet-mode emment-mode json-mode js2-mode web-mode js-comint js-mint edit-indirect markdown-mode command-log-mode haskell-mode ansi package-build shut-up epl git commander cask buttercup google-translate aggressive-indent aggresive-indent org-bullets ## afternoon-theme flycheck-pycheckers flycheck yasnippet-classic-snippets yasnippet-snippets which-key use-package smartparens multiple-cursors magit helm expand-region doom-themes doom-modeline company ace-window))
 '(safe-local-variable-values
   '((lexical-bindings . t)
     (eval progn
      (require 'projectile)
      (puthash
       (projectile-project-root)
       "emacs -batch -f package-initialize -L . -f buttercup-run-discover" projectile-test-cmd-map)))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(blamer-face
   ((t :foreground "#7a88cf" :background nil :height 140 :italic t))))
