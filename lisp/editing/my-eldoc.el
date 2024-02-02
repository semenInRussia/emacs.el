;;; my-eldoc.el --- My configuration of the `eldoc'

;; Copyright (C) 2022 Semen Khramtsov

;; Author: Semen Khramtsov <hrams205@gmail.com>
;; Version: 0.1
;; URL: https://github.com/semenInRussia/emacs.el

;;; Commentary:

;; My configuration of the `eldoc'

;;; Code:

(require 'my-leaf)

;; use `eldoc' with `flycheck' instead of echo area
(defvar flycheck-mode)
(defvar flycheck-display-errors-function)
(defvar flycheck-help-echo-function)

(declare-function flycheck-error-group "flycheck.el")
(declare-function flycheck-error-id "flycheck.el")
(declare-function flycheck-error-message "flycheck.el")
(declare-function flycheck-error-level "flycheck.el")
(declare-function flycheck-overlay-errors-at "flycheck.el")


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

(with-eval-after-load 'flycheck
  (add-hook 'flycheck-mode-hook #'my-flycheck-prefer-eldoc)
  (when flycheck-mode
    (my-flycheck-prefer-eldoc)))

;;; use beautifull documentation popup
(eval-and-compile
  (leaf eldoc
    :require t
    :custom ((eldoc-box-clear-with-C-g . t)
             (eldoc-idle-delay . 1.0)))

  (leaf eldoc-box
    :ensure (eldoc-box :repo "casouri/eldoc-box" :host github)
    :require t
    :custom (eldoc-box-fringe-use-same-bg . t)
    :bind ("C-h C-k" . 'eldoc-box-quit-frame)))

;; (setq eldoc-box-position-function
;;       #'my-eldoc-box--bottom-corner-position-function)

;; (defun my-eldoc-box--bottom-corner-position-function (width _)
;;   "The default function to set childframe position.
;; Used by `eldoc-box-position-function'.
;; Position is calculated base on WIDTH and HEIGHT of childframe text window"
;;   (pcase-let ((`(,_offset-l ,offset-r ,offset-t) eldoc-box-offset))
;;     (cons (- (frame-outer-width (selected-frame)) width offset-r)
;;           ;; y position + v-offset
;;           offset-t)))

(define-global-minor-mode global-eldoc-box-hover-mode
  eldoc-box-hover-mode
  eldoc-box-hover-mode)

(global-eldoc-box-hover-mode)

;;; use `eldoc' with `eglot'

(provide 'my-eldoc)
;;; my-eldoc.el ends here
