(load "~/init.el")
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-show-quick-access t nil nil "Customized with use-package company")
 '(custom-safe-themes
   '("15c821db2a9d52d6f27d0251256a75d4ec681f03dd6beaf4cda2fdae3ca170f0" "04e572ffdfa97caca9725c87a6138062b6a9428a88072969b26fea07f34dfdf9" "0069f9fc3e624e7d0a6deb780be540122c718bba6db4e0bf74cb6be33fd0f24f" "3cdcf69aa157c058aa8ed16a8e29f1b988c24bcf2f2bf9473afe078436051a09" "0466adb5554ea3055d0353d363832446cd8be7b799c39839f387abb631ea0995" default))
 '(frame-brackground-mode 'dark)
 '(helm-completion-style 'helm)
 '(package-selected-packages
   '(emmet-mode emment-mode json-mode js2-mode web-mode js-comint js-mint edit-indirect markdown-mode command-log-mode haskell-mode ansi package-build shut-up epl git commander cask buttercup google-translate aggressive-indent aggresive-indent org-bullets ## afternoon-theme flycheck-pycheckers flycheck yasnippet-classic-snippets yasnippet-snippets which-key use-package smartparens multiple-cursors magit helm expand-region doom-themes doom-modeline company ace-window))
 '(safe-local-variable-values
   '((eval progn
      (require 'projectile)
      (puthash
       (projectile-project-root)
       "emacs -batch -f package-initialize -L . -f buttercup-run-discover" projectile-test-cmd-map)))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
