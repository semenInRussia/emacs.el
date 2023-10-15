;;; my-persp.el --- My configuration of `persp-mode': workspaces for Emacs -*- lexical-binding: t; -*-

;; Copyright (C) 2023 semenInRussia

;;; Commentary:

;; My configuration of `persp-mode': workspaces for Emacs.

;;; Code:

(require 'my-leaf)


(leaf persp-mode
  :ensure t
  ;; enable (enable `persp-mode', only when user either hit one of `persp-mode' commands, which are started
  ;; on C-c or enable `persp-mode' via M-x.
  :commands persp-mode
  :config (persp-mode 'enable)
  ;; change prefix from the default "C-c p" to "C-c ,"
  :custom `((persp-keymap-prefix . ,(kbd "C-c ,"))
            (persp-load-buffer-functions . nil))
  :preface
  (defvar persp-key-map nil)
  (define-prefix-command 'persp-key-map)
  :bind (:persp-key-map
         :package subr-x
         ("n" . persp-next)
         ("p" . persp-prev)
         ("s" . persp-frame-switch)
         ("S" . persp-window-switch)
         ("r" . persp-rename)
         ("c" . persp-copy)
         ("C" . persp-kill)
         ("z" . persp-save-and-kill)
         ("a" . persp-add-buffer)
         ("b" . persp-switch-to-buffer)
         ("t" . persp-temporarily-display-buffer)
         ("i" . persp-import-buffers)
         ("I" . persp-import-win-conf)
         ("k" . persp-remove-buffer)
         ("K" . persp-kill-buffer)
         ("w" . persp-save-state-to-file)
         ("W" . persp-save-to-file-by-names)
         ("l" . persp-load-state-from-file)
         ("L" . persp-load-from-file-by-names)
         ("o" . (lambda ()
                  (interactive)
                  (persp-mode -1))))
  :bind-keymap (:mode-specific-map
                :package subr-x
                ("," . persp-key-map))
  :config
  (defun my-persp-swith-by-number (num)
    "Switch to the perspective with a given NUM.

Perspectives sorted by create time.  Note that the first perspective
has a number 1, not 0"
    (interactive (list (read-number "Number of a perspective: ")))
    (->>
     (persp-names-current-frame-fast-ordered)
     (nth (1- num))
     persp-switch))

  (->>
   '(1 2 3 4 5 6 7 8 9)
   (--mapcat
    `((defun ,(intern (concat "my-persp-swith-" (number-to-string it))) ()
        ,(format "Switch to %s perspective." it)
        (interactive)
        (my-persp-swith-by-number ,it))
      (define-key persp-mode-map
                  (kbd ,(concat "M-" (number-to-string it)))
                  ',(intern (concat "my-persp-swith-" (number-to-string it))))))
   (cons 'progn)
   eval))

(leaf consult
  :after consult persp-mode
  :config
  (consult-customize consult--source-buffer :hidden nil :default nil)

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
