(load "~/init.el")
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("3cdcf69aa157c058aa8ed16a8e29f1b988c24bcf2f2bf9473afe078436051a09" "0466adb5554ea3055d0353d363832446cd8be7b799c39839f387abb631ea0995" default))
 '(package-selected-packages
   '(json-mode js2-mode web-mode js-comint js-mint edit-indirect markdown-mode command-log-mode haskell-mode ansi package-build shut-up epl git commander cask buttercup google-translate aggressive-indent aggresive-indent org-bullets ## afternoon-theme flycheck-pycheckers flycheck yasnippet-classic-snippets yasnippet-snippets which-key use-package smartparens multiple-cursors magit helm expand-region doom-themes doom-modeline company ace-window))
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
