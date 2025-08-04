;;; my-init.el --- My init.el -*- lexical-binding: t -*-

;;; Commentary:

;; My init.el.  Here I write some things which are can be added to other topics.

;;; Code:
(require 'my-leaf)

(setq user-full-name "semenInRussia"
      user-mail-address "hrams205@gmail.com")

;; Scrolling more OK
(setq scroll-margin 0
      scroll-preserve-screen-position t
      next-screen-context-lines 3)

(setq make-backup-files         nil)
(setq auto-save-list-file-name  nil)
(defalias 'yes-or-no-p 'y-or-n-p)

;; Russian input method
;; ---
;; now I can press `C-\\' and language I writing will be changed
(with-eval-after-load 'my-modules
  (setq-default default-input-method "russian-computer")
  (setq default-file-name-coding-system 'utf-8)
  (setq default-keyboard-coding-system 'utf-8)
  (setq buffer-file-coding-system 'utf-8))

;;; just make PPTX (powerpoint) file that can't be read
(defun my-new-fake-pptx-file ()
  "Make this buffer, fake presentation with format (.pptx)."
  (interactive)
  (->> "~/broken.pptx" f-read insert)
  (text-mode))

;;; I try to decrease the Emacs startup time
(defun my-display-startup-time ()
  "Show the time Emacs took before view the *scratch* buffer."
  (message
   "Emacs loaded in %s with %d garbage collections."
   (format
    "%.2f seconds"
    (float-time (time-subtract after-init-time before-init-time)))
   gcs-done))

(auto-save-mode -1)

(add-hook 'emacs-startup-hook #'my-display-startup-time)

;;; smooth scrolling
(add-hook 'emacs-startup-hook #'pixel-scroll-precision-mode)

(leaf sudo-edit
  :ensure t)

(leaf which-key
  :ensure t
  :global-minor-mode t
  :custom ((which-key-show-transient-maps . t))
  :defun which-key-setup-side-window-bottom
  :config (which-key-setup-side-window-bottom))

(provide 'my-init)
;;; my-init.el ends here
