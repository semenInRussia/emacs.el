;;; my-embark.el --- My configuration of `embark' -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2025 semenInRussia

;;; Commentary:

;; My configuration of `embark'.

;;; Code:

(require 'my-leaf)
(require 'dash)

(defvar-local vertico--input nil)  ; needed for `embark--vertico-indicator'


;; TODO: put it to other more right place
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)


(leaf embark
  :ensure t
  :defvar embark-keymap-alist marginalia-prompt-categories embark-indicators
  :custom (prefix-help-command . #'embark-prefix-help-command)
  :bind (("C-." . embark-act)
         ("C-M-." . embark-dwim)
         (:minibuffer-mode-map
          :package minibuffer
          ("C->" . embark-act-all)
          (">" . embark-become))
         (:embark-general-map
          ("." . my-embark-google-search))
         (:embark-region-map
          ("j" . join-line))
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
  (setq embark-indicators
        '(embark--vertico-indicator
          embark-minimal-indicator ; default is embark-mixed-indicator
          embark-highlight-indicator
          embark-isearch-highlight-indicator))

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

(leaf minibuffer
  :bind (:minibuffer-local-map
         ("M-." . #'my-embark-preview))
  :defvar embark-quit-after-action
  :config
  (defun my-embark-preview ()
    "Previews candidate in `vertico' buffer, unless it's a consult command."
    (interactive)
    (unless (bound-and-true-p consult--preview-function)
      (save-selected-window
        (let ((embark-quit-after-action nil))
          (embark-dwim))))))

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
