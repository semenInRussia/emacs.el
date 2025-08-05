;;; small-init.el --- Configuration file to very small minimal Emacs without dependecies -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2025 hrams205@gmail.com

;;; Commentary:

;; Configuration file to very small minimal Emacs without dependecies ever.

;;; Code:

(setq user-full-name "semenInRussia"
      user-mail-address "hrams205@gmail.com")

;; Load fonts
(defun font-installed-p (name)
  "Return non-nil if the font with the name NAME is exist."
  (find-font (font-spec :name name)))

(let ((size 17)
      (fonts
       '("JetBrains Mono"
         "JetBrains Mono Nerd Font"
         "FiraCode"
         "FiraCode Nerd Font"
         "Cascadia Code"
         "Cascadia Code Nerd Font"
         "Cascadia Code NF")))
  (let ((font (seq-find #'font-installed-p fonts)))
    (setf (alist-get 'font default-frame-alist)
          (format "%s-%s" font size))))

;; Layout
(setq inhibit-startup-screen t
      inhibit-startup-message t
      inhibit-startup-echo-area-message t
      initial-scratch-message nil
      ;; don't use system things, only Emacs
      use-file-dialog nil
      use-dialog-box nil)

(let ((h 20)
      (w 75))
  (setf
   (alist-get 'width default-frame-alist) w
   (alist-get 'height default-frame-alist) h
   (alist-get 'width initial-frame-alist) w
   (alist-get 'height initial-frame-alist) h))

;;; Disable UI elements early
;;;
;; HACK: I intentionally avoid calling `menu-bar-mode', `tool-bar-mode', and
;;   `scroll-bar-mode' because they do extra work to manipulate frame variables
;;   that isn't necessary this early in the startup process.
(setq default-frame-alist
      (append
       '((menu-bar-lines . 0)
         (tool-bar-lines . 0)
         (vertical-scroll-bars))
       default-frame-alist))

;; And set these to nil so users don't have to toggle the modes twice to
;; reactivate them.
(setq menu-bar-mode nil
      tool-bar-mode nil
      scroll-bar-mode nil)

;; completing read (read buffer, file, theme)
(fido-vertical-mode t)
(setq completion-styles '(flex))
(keymap-global-set "C-x C-b" 'switch-to-buffer)

(global-display-line-numbers-mode t)

;; Theme
(setq modus-themes-bold-constructs t)
(setq modus-themes-italic-constructs nil)
(setq modus-themes-region '(accent))
(setq font-lock-maximum-decoration t)
(add-hook 'after-init-hook #'global-hl-line-mode)
(load-theme 'modus-operandi-tinted :no-confirm)

;;; Editing

(electric-pair-mode t)

(defun open-line-saving-indent ()
  "Inserting new line, saving position and inserting new line."
  (interactive)
  (newline)
  (unless (string= "" (string-trim (thing-at-point 'line t)))
    (indent-according-to-mode))
  (forward-line -1)
  (end-of-line)
  (delete-horizontal-space t))

(defun my-beginning-of-line-text-or-visual-line ()
  "I think the command name explain everything."
  (interactive)
  (goto-char
   (max (save-excursion
          (beginning-of-line-text)
          (point))
        (save-excursion
          (beginning-of-visual-line)
          (point)))))

(repeat-mode t)

(keymap-global-set "C-a" 'my-beginning-of-line-text-or-visual-line)
(keymap-global-set "C-o" 'open-line-saving-indent)

(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(setq-default fill-column 80)
(keymap-set prog-mode-map "RET" #'newline-and-indent)

;; (global-whitespace-mode t)

(defun my-delete-window-frame (&optional window)
  "Delete the current window or frame if the window is one exists in frame."
  (interactive)
  (condition-case nil
      (delete-window window)
    (error (if (and tab-bar-mode
                    (> (length (funcall tab-bar-tabs-function)) 1))
               (tab-bar-close-tab)
             (delete-frame)))))

(defvar-keymap my-prev-next-buf-map
  :repeat (:enter (next-buffer previous-buffer))
  "n" #'next-buffer
  "p" #'previous-buffer)

(keymap-global-set "M-0" #'my-delete-window-frame)
(keymap-global-set "M-o" #'other-window)
(keymap-global-set "C-x C-p" #'previous-buffer)
(keymap-global-set "C-x C-n" #'next-buffer)

;;; Dired
(with-eval-after-load 'dired
  (eval-when-compile
    (require 'dired))
  (add-hook 'dired-mode-hook #'dired-hide-details-mode)
  (setq dired-dwim-target t
        dired-recursive-copies 'always
        dired-recursive-copies 'always
        dired-recursive-deletes 'always
        ;; don't open a lot of buffers
        dired-kill-when-opening-new-dired-buffer t
        delete-by-moving-to-trash t
        ;; revert buffer after copy, rename, delete commands
        dired-do-revert-buffer t)
  (keymap-set dired-mode-map "j" #'forward-line)
  (keymap-set dired-mode-map "k" #'previous-line)
  (keymap-set dired-mode-map "h" 'dired-up-directory)
  (put 'dired-jump 'repeat-map nil))

;;; "C-x r j m" to jump to the Messages buffer
(set-register ?m '(buffer . "*Messages*"))

;; Just jump to saved position when file is opened.  NOTE that it's a builtin
;; power of Emacs

;; Language Server Protocol configuration (LSP)

(use-package eglot
  :custom (;; (eglot-sync-connect . 1)
           (eglot-events-buffer-size 0)
           (eglot-autoshutdown t)
           (eglot-ignored-server-capabilities
            '(;; disable code lens
              :codeLensProvider
              ;; disable inlay hints
              :inlayHintProvider
              ;; dont higlight symbol
              :documentHighlightProvider))
           (eglot-events-buffer-config 0)
           (eglot-report-progress nil))
  :bind (:map eglot-mode-map
              ("<f6>"   . eglot-rename))
  :config
  ;; set default LSP servers for all supported languages
  (defvar eglot-server-programs)  ; make compiler happier
  ;; python (pyright)
  (setf (alist-get '(python-mode python-ts-mode) eglot-server-programs
                   nil nil 'equal)
        '("pyright-langserver" "--stdio"))

  (fset 'jsonrpc--log-event #'ignore))

(with-eval-after-load 'python
  (add-hook 'python-mode-hook 'eglot-ensure))

;; recentf : check recent visited files
(recentf-mode t)
(keymap-global-set "C-c r" 'recentf)
(with-eval-after-load 'recentf
  (setq recentf-auto-cleanup 'never))

;; smooth scrolling
(add-hook 'emacs-startup-hook #'pixel-scroll-precision-mode)

;; Scrolling more OK
(setq scroll-margin 0
      scroll-preserve-screen-position t
      next-screen-context-lines 3)

(setq make-backup-files         nil)
(setq auto-save-list-file-name  nil)
(defalias 'yes-or-no-p 'y-or-n-p)

;; I try to decrease the Emacs startup time
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

;; Start a server to fast opening files in the same session
(require 'server)
(unless (server-running-p)
  (server-start))

;; (add-to-list 'load-path "~/.emacs.d/pam/")
;; (load "~/.emacs.d/pam/.el")

(provide 'small-init)
;;; small-init.el ends here
