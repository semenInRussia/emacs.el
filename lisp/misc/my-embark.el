;;; my-embark.el --- My configuration of `embark' -*- lexical-binding: t; -*-

;; Copyright (C) 2023 semenInRussia

;; Author: semenInRussia <hrams205@gmail.com>
;; Version: 0.1
;; Homepage: https://github.com/semeninrussia/emacs.el

;;; Commentary:

;; My configuration of `embark'.

;;; Code:

(require 'my-leaf)


;; TODO: put it to other more right place
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)


(leaf embark
  :defvar (embark-keymap-alist marginalia-prompt-categories)
  :ensure t
  :bind (("C-." . embark-act)
         ("C-M-." . embark-dwim)
         (:minibuffer-mode-map
          :package minibuffer           ; built-in
          ("C-," . my-embark-act-noquit)
          ("C-<" . my-embark-act-all-noexit)
          ("C->" . embark-act-all)
          ("C-S-m" . embark-export)
          ("C-M-m" . embark-collect))
         (:embark-general-map
          ("." . my-embark-google-search)))

  ;; eval after `embark' was loaded
  :config

  (defun my-embark-act-noquit ()
    "Run action but don't quit the minibuffer afterwards."
    (interactive)
    (let ((embark-quit-after-action nil))
      (embark-act)))

  (defun my-embark-act-all-noexit ()
    "Do `embark-act-all' without exit from the minibuffer."
    (interactive)
    (let ((embark-quit-after-action nil))
      (embark-act-all)))

  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none))))

  ;;; SOME ADDITIONAL ACTIONS

  ;; googling a thing
  ;;
  ;; was grabbed from the offical wiki
  (defun my-embark-google-search (term)
    "Open google.com to search a given TERM."
    (interactive "sSearch Term: ")
    (browse-url
     (format "http://google.com/search?q=%s" term)))

  ;; support of `straight'
  ;;
  ;; was grabbed from the offical wiki
  (with-eval-after-load 'straight
    (defvar-keymap embark-straight-map
      :parent embark-general-map
      "u" 'straight-visit-package-website
      "r" 'straight-get-recipe
      "i" 'straight-use-package
      "c" 'straight-check-package
      "F" 'straight-pull-package
      "f" 'straight-fetch-package
      "p" 'straight-push-package
      "n" 'straight-normalize-package
      "m" 'straight-merge-package)

    (add-to-list 'embark-keymap-alist '(straight . embark-straight-map))

    (with-eval-after-load 'marginalia
      (add-to-list 'marginalia-prompt-categories '("recipe\\|package" . straight)))))

;; support of agnifize.el: my small Emacs package
;; to make a regular Python code into bad code (my sister agnia write bad code)
(leaf agnifize
  :bind ((:embark-file-map
          :package embark
          ("Q" . 'agnifize-file))
         (:embark-buffer-map
          :package embark
          ("Q" . 'agnifize-buffer))
         (:embark-region-map
          :package embark
          ("Q" . 'agnifize-region))))

(provide 'my-embark)
;;; my-embark.el ends here
