;;; my-eldoc.el --- My configuration of the `eldoc'

;; Copyright (C) 2022-2024 Semen Khramtsov

;; Author: Semen Khramtsov <hrams205@gmail.com>
;; Version: 0.1
;; URL: https://github.com/semenInRussia/emacs.el

;;; Commentary:

;; My configuration of the `eldoc': show documentation for symbol

;;; Code:

(require 'my-leaf)

;; use `eldoc' with `flycheck' instead of echo area
(leaf flycheck
  :after (eldoc eldoc-box)
  ;; some things for byte-compiler
  :defvar
  (flycheck-mode
   flycheck-display-errors-function
   flycheck-help-echo-function)
  :defun
  (flycheck-error-group
   flycheck-error-id
   flycheck-error-message
   flycheck-error-level
   flycheck-overlay-errors-at)
  :defer-config
  (add-hook 'flycheck-mode-hook #'my-flycheck-prefer-eldoc)
  (when flycheck-mode (my-flycheck-prefer-eldoc)))

(defun my-flycheck-eldoc (callback &rest _ignored)
  "Print flycheck messages at point by calling CALLBACK."
  (when-let ((flycheck-errors (and flycheck-mode (flycheck-overlay-errors-at (point)))))
    (mapc
     (lambda (err)
       (let ((level (flycheck-error-level err)))
         (funcall callback
                  (format "%s:  %s"
                          (propertize
                           (pcase level
                             ('info
                              "I"
                              ;; (nerd-icons-codicon "nf-cod-info")
                              )
                             ('error
                              "E"
                              ;; (nerd-icons-codicon "nf-cod-error")
                              )
                             ('warning
                              "W"
                              ;; (nerd-icons-codicon "nf-cod-warning")
                              )
                             (_ level))
                           'face (pcase level
                                   ('info
                                    'flycheck-error-list-info)
                                   ('error
                                    'flycheck-error-list-error)
                                   ('warning
                                    'flycheck-error-list-warning)
                                   (_ 'font-lock-doc-face)))
                          (flycheck-error-message err))

                  :thing (or (flycheck-error-id err)
                             (flycheck-error-group err)))))
     flycheck-errors)))

(defun my-flycheck-prefer-eldoc ()
  "Prefer `eldoc' over the echo area for `flycheck'."
  (interactive)
  (add-hook 'eldoc-documentation-functions #'my-flycheck-eldoc nil t)
  (setq eldoc-documentation-strategy 'eldoc-documentation-compose-eagerly)
  (setq flycheck-display-errors-function nil)
  (setq flycheck-help-echo-function nil))

;;; use beautifull documentation popup
(leaf eldoc
  :custom ((eldoc-box-clear-with-C-g . t)
           (eldoc-idle-delay . 1.0)))



(leaf eldoc-box
  :ensure (eldoc-box :repo "casouri/eldoc-box" :host github)
  :defvar eldoc-box-clear-with-C-g
  :commands (eldoc-box--eldoc-message-function
             eldoc-box--eldoc-display-function
             eldoc-box-quit-frame)
  :custom ((eldoc-box-fringe-use-same-bg . nil)
           ;; press C-g if need
           (eldoc-box-cleanup-interval . 3)
           (eldoc-box-clear-with-C-g . t)))

(defun my-eldoc-box--enable ()
  "Enable eldoc-box hover.
Intended for internal use.

This is full copy of `eldoc-box--enable', this is more cooler because
we don't need in load eldoc while it isn't needed.  light-weight"
  (if (not (boundp 'eldoc-display-functions))
      (add-function :before-while (local 'eldoc-message-function)
                    #'eldoc-box--eldoc-message-function)

    (setq-local eldoc-box--old-eldoc-functions
                eldoc-display-functions)
    (setq-local eldoc-display-functions
                (cons 'eldoc-box--eldoc-display-function
                      (remq 'eldoc-display-in-echo-area
                            eldoc-display-functions))))
  (when eldoc-box-clear-with-C-g
    (advice-add #'keyboard-quit :before #'eldoc-box-quit-frame)))

(add-hook 'eldoc-mode-hook 'my-eldoc-box--enable)

;;; use `eldoc' with `eglot'

(provide 'my-eldoc)
;;; my-eldoc.el ends here
