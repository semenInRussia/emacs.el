;;; my-init.el --- My init.el -*- lexical-binding: t -*-

;;; Commentary:

;; My init.el.  Here I write some things which are can be added to other topics.

;;; Code:
(require 'my-leaf)

;;; Info about me
(setq user-full-name "semenInRussia"
      user-mail-address "hrams205@gmail.com")

;;; Make scrolling more OK
(add-hook 'emacs-startup-hook #'pixel-scroll-precision-mode)
(setq scroll-margin 0
      scroll-preserve-screen-position t
      next-screen-context-lines 3)

;;; Avoid backups or lockfiles
(setq make-backup-files nil
      auto-save-list-file-name nil
      create-lockfiles nil
      make-backup-files nil)

;;; Auto save

;; Enable auto-save to safeguard against crashes or data loss. The
;; `recover-file' or `recover-session' functions can be used to restore
;; auto-saved data.
(setq auto-save-default nil
      auto-save-no-message t)

;; Do not auto-disable auto-save after deleting large chunks of
;; text.
(setq auto-save-include-big-deletions t)

(setq tramp-auto-save-directory (expand-file-name "tramp-autosave/" user-emacs-directory))

;; Auto save options
(setq kill-buffer-delete-auto-save-files t)

;; Remove duplicates from the kill ring to reduce clutter
(setq kill-do-not-save-duplicates t)

;; when ask yes or no, you can press y or n
(setq use-short-answers t)
(advice-add 'yes-or-no-p :override #'y-or-n-p)


;;; Russian input method
;;; ---
;;; now I can press `C-\\' and language I writing will be changed
(with-eval-after-load 'my-modules
  (setq-default default-input-method "russian-computer")
  (setq default-file-name-coding-system 'utf-8)
  (setq default-keyboard-coding-system 'utf-8)
  (setq buffer-file-coding-system 'utf-8))

;;; I try to decrease the Emacs startup time
(defun my-display-startup-time ()
  "Show the time Emacs took before view the *scratch* buffer."
  (message
   "Emacs loaded in %s with %d garbage collections."
   (format
    "%.2f seconds"
    (float-time (time-subtract after-init-time before-init-time)))
   gcs-done))

(add-hook 'emacs-startup-hook #'my-display-startup-time)


;;; UTF-8 coding
(prefer-coding-system 'utf-8)
(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

;;; sometimes i need to open only one file with sudo
(leaf sudo-edit
  :ensure t)

;;; it helps you when you hit complex key combinations
(leaf which-key
  :ensure t
  :global-minor-mode t
  :custom ((which-key-show-transient-maps . t))
  :defun which-key-setup-side-window-bottom
  :config (which-key-setup-side-window-bottom))

;;; Undo/redo

(setq undo-limit (* 13 160000)
      undo-strong-limit (* 13 240000)
      undo-outer-limit (* 13 24000000))

;;; Keep the cursor out of the read-only portions of the.minibuffer
(setq minibuffer-prompt-properties
      '(read-only t intangible t cursor-intangible t face minibuffer-prompt))
(add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

;;; Mouse

(setq mouse-yank-at-point nil)
(when (and (display-graphic-p) (fboundp 'context-menu-mode))
  (add-hook 'after-init-hook #'context-menu-mode))

;;; Cursor

;; The blinking cursor is distracting and interferes with cursor settings in
;; some minor modes that try to change it buffer-locally (e.g., Treemacs).
(when (bound-and-true-p blink-cursor-mode)
  (blink-cursor-mode -1))

;; Don't blink the paren matching the one at point, it's too distracting.
(setq blink-matching-paren nil)

;; Do not extend the cursor to fit wide characters
(setq x-stretch-cursor nil)

(provide 'my-init)
;;; my-init.el ends here
