;;; my-embark.el --- My configuration of `embark' -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2024 semenInRussia

;; Author: semenInRussia <hrams205@gmail.com>
;; Version: 0.1
;; Homepage: https://github.com/semeninrussia/emacs.el

;;; Commentary:

;; My configuration of `embark'.

;;; Code:

(require 'my-leaf)
(require 'dash)


;; TODO: put it to other more right place
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)


(leaf embark
  :defvar (embark-keymap-alist marginalia-prompt-categories)
  :ensure t
  :bind (("C-." . embark-act)
         ("C-M-." . embark-dwim)
         (:minibuffer-mode-map
          :package minibuffer
          ("C->" . embark-act-all)
          (">" . embark-become))
         (:embark-general-map
          ("." . my-embark-google-search))
         (:embark-file-map
          ("2" . my-find-file-below)
          ("3" . my-find-file-right)
          ("5" . find-file-other-frame))
         (:embark-buffer-map
          ("2" . my-switch-to-buffer-below)
          ("3" . my-switch-to-buffer-right)
          ("5" . switch-to-buffer-other-frame)))

  ;; eval after `embark' was loaded
  :config

  (eval-and-compile
    (defun my--and-after (g f)
      "Expand to d(x) = [g() f(x)]"
      (lambda (x)
        (funcall g)
        (funcall f x))))

  (cl-flet ((op (g f)
	            (lambda (x)
		            (funcall g)
		            (funcall f x))))
    (defalias 'my-find-file-right (op 'split-window-right
                                      'find-file-other-window))
    (defalias 'my-find-file-below (op 'split-window-below
                                      'find-file-other-window))

    (defalias 'my-switch-to-buffer-right (op 'split-window-right
                                             'switch-to-buffer-other-window))
    (defalias 'my-switch-to-buffer-below (op 'split-window-below
                                             'switch-to-buffer-other-window)))

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
