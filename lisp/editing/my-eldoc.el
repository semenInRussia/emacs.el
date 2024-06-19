;;; my-eldoc.el --- My configuration of the `eldoc'

;; Copyright (C) 2022-2024 semenInRussia

;; Author: semenInRussia <hrams205@gmail.com>
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
  (setq eldoc-documentation-strategy 'eldoc-documentation-compose-eagerly
        flycheck-display-errors-function nil
        flycheck-help-echo-function nil))

;;; use beautiful documentation popup
(leaf eldoc
  :ensure (eldoc :type built-in)
  :custom (eldoc-idle-delay . 0.2))

(leaf eldoc-box
  :ensure (eldoc-box
           :repo "casouri/eldoc-box"
           :host github)
  :when (display-graphic-p)
  :commands (eldoc-box--eldoc-message-function
             eldoc-box--eldoc-display-function)
  :bind (("C-h C-k" . eldoc-box-quit-frame)
         ("C-h C-v" . my-scroll-eldoc-box-frame))
  :defvar (eldoc-box--buffer
           eldoc-box-clear-with-C-g
           eldoc-box-max-pixel-width)
  :custom (;; (eldoc-box-fringe-use-same-bg . nil)
           (eldoc-box-cleanup-interval . 30)
           (eldoc-box-clear-with-C-g . t))
  :init
  (setq eldoc-box-max-pixel-width
        (/ (frame-outer-width) 3))
  (defun my-scroll-eldoc-box-frame ()
    "Scroll the `eldoc-box' frame."
    (interactive)
    (with-selected-window (get-buffer-window eldoc-box--buffer t)
      (scroll-up)))

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

  (add-hook 'eldoc-mode-hook 'my-eldoc-box--enable))

;;; use `eldoc' with `eglot'

(provide 'my-eldoc)
;;; my-eldoc.el ends here
