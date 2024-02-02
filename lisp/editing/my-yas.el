;;; my-yas.el --- My configuration for the `yasnippet'

;; Copyright (C) 2022-2024 semenInRussia

;; Author: Semen Khramtsov <hrams205@gmail.com>
;; Version: 0.1
;; URL: https://github.com/semenInRussia/emacs.el

;; This file is not part of GNU Emacs.

;;; Commentary:

;; My configuration for the `yasnippet'

;;; Code:


(require 'my-leaf)
(require 's)
(require 'dash)


(defvar my-snippets-dir (locate-user-emacs-file "snippets")
  "The directory in which I save snippets for `yasnippet'.")


(leaf yasnippet
  :ensure (yasnippet :repo "joaotavora/yasnippet")
  :defun (yas--table-hash
          yas--filter-templates-by-condition
          yas--namehash-templates-alist)
  ;; don't use `yas-global-mode', prefer local minor modes
  ;; :global-minor-mode yas-global-mode
  :hook ((prog-mode-hook . yas-minor-mode)
         (text-mode-hook . yas-minor-mode))
  :custom (yas-wrap-around-region . t)
  :config
  (setq yas-snippet-dirs (list my-snippets-dir))
  ;; don't load snippets instantly after a file opened, wait some AFK
  ;; time
  (run-with-idle-timer 3 nil #'yas-reload-all))

;; a completion for snippets with `cape' (capf)

(declare-function cape--table-with-properties "cape")
(declare-function cape--bounds "cape")
(declare-function cape-interactive "cape")

(with-eval-after-load 'cape
  (defvar my-yas--capf-properties
    (list :annotation-function (lambda (_) " Snippet ")
          :company-kind (lambda (_) 'snippet)
          :exit-function 'my-yas-capf--exit
          :company-docsig 'my-yas-capf--docsig
          :exclusive 'no)
    "Completion extra properties for `my-yas-capf'.")

  (defun my-yas-capf (&optional interactive)
    "Completion at point for `yasnippet'.

If INTERACTIVE is true, show the completion where suggested only snippets."
    (interactive (list t))
    (if interactive
        (cape-interactive #'my-yas-capf)
      (when-let (snippets (yas-active-keys))
        (let ((bounds (cape--bounds 'symbol)))
          `(,(car bounds) ,(cdr bounds)
            ,(cape--table-with-properties snippets :category 'snippet)
            ,@my-yas--capf-properties)))))

  (defun my-yas-capf--docsig (key)
    "Snippet content for `corfu' which show it in the echo area.

It takes the KEY (trigger) of snippet, because user type it and `corfu'
manipulate with it to show helpful things"
    (->
     ;; the first snippets table
     (yas--get-snippet-tables major-mode)
     (car)
     ;; fetch snippets with a given key
     (yas--fetch key)
     ;; choose the first
     (car)
     (cdr)
     ;; get its content
     (yas--template-content)))

  (defun my-yas-capf--exit (name status)
    "Exit from `my-yas-capf'."
    (and
     (eq status 'finished)
     (yas-expand)))

  (with-eval-after-load 'corfu
    (add-hook
     'corfu-mode-hook
     (defun my-yas-capf-setup ()
       "Add capf for `yasnippet'."
       (add-hook 'completion-at-point-functions 'my-yas-capf 30 'local)))))

(provide 'my-yas)
;;; my-yas.el ends here
