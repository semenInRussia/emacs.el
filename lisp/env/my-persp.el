;;; my-persp.el --- My configuration of `persp-mode': workspaces for Emacs -*- lexical-binding: t; -*-
;; Copyright (C) 2023 semenInRussia

;;; Commentary:
;; My configuration of `persp-mode': workspaces for Emacs.

;;; Code:
(require 'dash)
(require 'my-leaf)


(defun my-persp-switch-by-number (num)
  "Switch to the perspective with a given NUM.

Perspectives sorted by create time.  Note that the first perspective
has a number 1, not 0"
  (interactive (list (read-number "Number of a perspective: ")))
  (->> (persp-names-current-frame-fast-ordered)
       (nth (1- num))
       persp-switch))

(defmacro my-bind-persp-switch ()
  "."
  `(progn
     ,@(--mapcat       ; Bind M-<digit> to switch to other perspective
        `((defun ,(intern (format "my-persp-switch-%s" it)) ()
            ,(format "Switch to %s perspective." it)
            (interactive)
            (my-persp-switch-by-number ,it))
          (keymap-global-set ,(format "M-%s" it)
                             #',(intern (format "my-persp-switch-%s" it))))
        '(1 2 3 4 5 6 7 8 9))))

(defvar persp-key-map)
(defun my-persp--use-keymap ()
  "Use `persp-key-map' (if `persp-mode' didn't load, do it)."
  (interactive)
  (unless (bound-and-true-p persp-mode)
    (require 'persp-mode))
  (set-transient-map persp-key-map))

(leaf persp-mode
  :ensure t
  :defun persp-mode persp-set-keymap-prefix
  :config (persp-mode +1)
  ;; change prefix from the default "C-c p" to "C-c ,"
  :defvar persp-mode-map persp-key-map persp-keymap-prefix
  :bind ("C-c ," . my-persp--use-keymap)
  :custom `((persp-keymap-prefix . ,(kbd "C-c ,"))
            ;; if `persp-auto-resume-time' <= 0, then `persp-mode'
            ;; don't load all auto-saved perspectives at startup, if
            ;; you need in them, do `persp-load-state-from-file'
            (persp-auto-resume-time . 0)
            (persp-auto-save-opt . 0))
  :config (persp-set-keymap-prefix persp-keymap-prefix)
  ;; some my custom functions
  :defun (persp-names-current-frame-fast-ordered persp-switch)
  :init (my-bind-persp-switch))

(defvar persp-key-map)
(with-eval-after-load 'persp-mode
  (define-key mode-specific-map "," persp-key-map))

(leaf consult
  :after consult persp-mode
  :defun (consult--buffer-state
          consult--customize-put
          my-persp-buffer-names
          persp-buffer-list-restricted)
  :defvar consult-buffer-sources
  :config
  (consult--customize-put '(consult--source-buffer) :default nil)
  (consult--customize-put '(consult--source-buffer) :hidden nil)

  (defun my-persp-buffer-names ()
    "Return the list of the current perspective buffers names."
    (mapcar #'buffer-name (persp-buffer-list-restricted)))

  (defvar my-consult--source-perspective
    (list :name     "Perspective"
          :narrow   ?s
          :category 'buffer
          :state    #'consult--buffer-state
          :default  t
          :items    #'my-persp-buffer-names))

  (add-to-list 'consult-buffer-sources 'my-consult--source-perspective))

;;; my-persp.el ends here
(provide 'my-persp)
