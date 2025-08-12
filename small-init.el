;;; small-init.el --- Configuration file to very small minimal Emacs without dependecies -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2025 hrams205@gmail.com

;;; Commentary:

;; Configuration file to very small minimal Emacs without dependecies ever.

;;; Code:

;; Yes, it's me
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

(setq-default line-spacing 0.3)

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
(setq tab-always-indent 'complete)  ;; Starts completion with TAB
(eval-when-compile
  (require 'icomplete))
(setq icomplete-delay-completions-threshold 0)
(setq icomplete-compute-delay 0)
(setq icomplete-show-matches-on-no-input t)
(setq icomplete-hide-common-prefix nil)
(setq icomplete-prospects-height 10)
(setq icomplete-separator " . ")
(setq icomplete-with-completion-tables t)
(setq icomplete-in-buffer t)
(setq icomplete-max-delay-chars 0)
(setq icomplete-scroll t)
(advice-add 'completion-at-point
            :after #'minibuffer-hide-completions)

;; (defcustom icomplete-vertical-selected-prefix-marker "> "
;;   "Prefix string used to mark the selected completion candidate.
;; If `icomplete-vertical-render-prefix-marker' is t, the string
;; setted here is used as a prefix of the currently selected entry in the
;; list.  It can be further customized by the face
;; `icomplete-vertical-selected-prefix-face'."
;;   :type 'string
;;   :group 'icomplete
;;   :version "31")

;; (defcustom icomplete-vertical-unselected-prefix-marker "  "
;;   "Prefix string used on the unselected completion candidates.
;; If `icomplete-vertical-render-prefix-marker' is t, the string
;; setted here is used as a prefix for all unselected entries in the list.
;; list.  It can be further customized by the face
;; `icomplete-vertical-unselected-prefix-face'."
;;   :type 'string
;;   :group 'icomplete
;;   :version "31")

;; (defcustom icomplete-vertical-in-buffer-adjust-list t
;;   "Control whether in-buffer completion should align the cursor position.
;; If this is t and `icomplete-in-buffer' is t, and `icomplete-vertical-mode'
;; is activated, the in-buffer vertical completions are shown aligned to the
;; cursor position when the completion started, not on the first column, as
;; the default behaviour."
;;   :type 'boolean
;;   :group 'icomplete
;;   :version "31")

;; (defcustom icomplete-vertical-render-prefix-marker t
;;   "Control whether a marker is added as a prefix to each candidate.
;; If this is t and `icomplete-vertical-mode' is activated, a marker,
;; controlled by `icomplete-vertical-selected-prefix-marker' is shown
;; as a prefix to the current under selection candidate, while the
;; remaining of the candidates will receive the marker controlled
;; by `icomplete-vertical-unselected-prefix-marker'."
;;   :type 'boolean
;;   :group 'icomplete
;;   :version "31")

;; (defface icomplete-vertical-selected-prefix-face
;;   '((t :inherit font-lock-keyword-face :weight bold :foreground "cyan"))
;;   "Face used for the prefix set by `icomplete-vertical-selected-prefix-marker'."
;;   :group 'icomplete
;;   :version "31")

;; (defface icomplete-vertical-unselected-prefix-face
;;   '((t :inherit font-lock-keyword-face :weight normal :foreground "gray"))
;;   "Face used for the prefix set by `icomplete-vertical-unselected-prefix-marker'."
;;   :group 'icomplete
;;   :version "31")

;; (defun icomplete-vertical--adjust-lines-for-column (lines buffer data)
;;   "Adjust the LINES to align with the column in BUFFER based on DATA."
;;   (if icomplete-vertical-in-buffer-adjust-list
;;       (let ((column
;;              (with-current-buffer buffer
;;                (save-excursion
;;                  (goto-char (car data))
;;                  (current-column)))))
;;         (dolist (l lines)
;;           (add-text-properties
;;            0 1 `(display ,(concat (make-string column ?\s) (substring l 0 1)))
;;            l))
;;         lines)
;;     lines))

;; (defun icomplete-vertical--add-marker-to-selected (comp)
;;   "Add markers to the selected/unselected COMP completions."
;;   (if (and icomplete-vertical-render-prefix-marker
;;            (get-text-property 0 'icomplete-selected comp))
;;       (concat (propertize icomplete-vertical-selected-prefix-marker
;;                           'face 'icomplete-vertical-selected-prefix-face)
;;               comp)
;;     (concat (propertize icomplete-vertical-unselected-prefix-marker
;;                         'face 'icomplete-vertical-unselected-prefix-face)
;;             comp)))

;; (cl-defun icomplete--render-vertical
;;     (comps md &aux scroll-above scroll-below
;;            (total-space ; number of mini-window lines available
;;             (1- (min
;;                  icomplete-prospects-height
;;                  (truncate (max-mini-window-lines) 1)))))
;;   ;; Welcome to loopapalooza!
;;   ;;
;;   ;; First, be mindful of `icomplete-scroll' and manual scrolls.  If
;;   ;; `icomplete--scrolled-completions' and `icomplete--scrolled-past'
;;   ;; are:
;;   ;;
;;   ;; - both nil, there is no manual scroll;
;;   ;; - both non-nil, there is a healthy manual scroll that doesn't need
;;   ;;   to be readjusted (user just moved around the minibuffer, for
;;   ;;   example);
;;   ;; - non-nil and nil, respectively, a refiltering took place and we
;;   ;;   may need to readjust them to the new filtered `comps'.
;;   (when (and icomplete-scroll
;;              icomplete--scrolled-completions
;;              (null icomplete--scrolled-past))
;;     (cl-loop with preds
;;              for (comp . rest) on comps
;;              when (equal comp (car icomplete--scrolled-completions))
;;              do
;;              (setq icomplete--scrolled-past preds
;;                    comps (cons comp rest))
;;              (completion--cache-all-sorted-completions
;;               (icomplete--field-beg)
;;               (icomplete--field-end)
;;               comps)
;;              and return nil
;;              do (push comp preds)
;;              finally (setq icomplete--scrolled-completions nil)))
;;   ;; Then, in this pretty ugly loop, collect completions to display
;;   ;; above and below the selected one, considering scrolling
;;   ;; positions.
;;   (cl-loop with preds = icomplete--scrolled-past
;;            with succs = (cdr comps)
;;            with space-above = (- total-space
;;                                  1
;;                                  (cl-loop for (_ . r) on comps
;;                                           repeat (truncate total-space 2)
;;                                           while (listp r)
;;                                           count 1))
;;            repeat total-space
;;            for neighbor = nil
;;            if (and preds (> space-above 0)) do
;;            (push (setq neighbor (pop preds)) scroll-above)
;;            (cl-decf space-above)
;;            else if (consp succs) collect
;;            (setq neighbor (pop succs)) into scroll-below-aux
;;            while neighbor
;;            finally (setq scroll-below scroll-below-aux))
;;   ;; Halfway there...
;;   (let* ((selected (propertize (car comps) 'icomplete-selected t))
;;          (chosen (append scroll-above (list selected) scroll-below))
;;          (tuples (icomplete--augment md chosen))
;;          max-prefix-len max-comp-len lines nsections)
;;     (add-face-text-property 0 (length selected)
;;                             'icomplete-selected-match 'append selected)
;;     ;; Figure out parameters for horizontal spacing
;;     (cl-loop
;;      for (comp prefix) in tuples
;;      maximizing (length prefix) into max-prefix-len-aux
;;      maximizing (length comp) into max-comp-len-aux
;;      finally (setq max-prefix-len max-prefix-len-aux
;;                    max-comp-len max-comp-len-aux))
;;     ;; Serialize completions and section titles into a list
;;     ;; of lines to render
;;     (cl-loop
;;      for (_comp prefix suffix section) in tuples
;;      when section
;;      collect (propertize section 'face 'icomplete-section) into lines-aux
;;      and count 1 into nsections-aux
;;      for comp = (icomplete-vertical--add-marker-to-selected comp)
;;      when (get-text-property 0 'icomplete-selected comp)
;;      do (add-face-text-property 0 (length comp)
;;                                 'icomplete-selected-match 'append comp)
;;      collect (concat prefix
;;                      (make-string (max 0 (- max-prefix-len (length prefix))) ? )
;;                      (completion-lazy-hilit comp)
;;                      (make-string (max 0 (- max-comp-len (length comp))) ? )
;;                      suffix)
;;      into lines-aux
;;      finally (setq lines lines-aux
;;                    nsections nsections-aux))
;;     ;; Kick out some lines from the beginning due to extra sections.
;;     ;; This hopes to keep the selected entry more or less in the
;;     ;; middle of the dropdown-like widget when `icomplete-scroll' is
;;     ;; t.  Funky, but at least I didn't use `cl-loop'
;;     (setq lines
;;           (nthcdr
;;            (cond ((<= (length lines) total-space) 0)
;;                  ((> (length scroll-above) (length scroll-below)) nsections)
;;                  (t (min (ceiling nsections 2) (length scroll-above))))
;;            lines))
;;     (when icomplete--in-region-buffer
;;       (setq lines (icomplete-vertical--adjust-lines-for-column
;;                    lines icomplete--in-region-buffer completion-in-region--data)))
;;     ;; At long last, render final string return value.  This may still
;;     ;; kick out lines at the end.
;;     (concat " \n"
;;             (cl-loop for l in lines repeat total-space concat l concat "\n"))))

(icomplete-vertical-mode t)

(keymap-global-set "C-x C-b" 'switch-to-buffer)

;; `fido-vertical-mode' as auto complete

(setq display-line-numbers-type 'relative)
(add-hook 'prog-mode-hook #'display-line-numbers-mode)

;; Theme
(eval-and-compile
  (require-theme 'modus-themes))
(add-hook 'after-init-hook #'global-hl-line-mode)
(setq modus-themes-bold-constructs t
      modus-themes-italic-constructs nil
      modus-themes-region '(accent)
      font-lock-maximum-decoration t)
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

;; If press "C-x o" , you can hit o, to repeat this command
(repeat-mode t)

(keymap-global-set "C-a" 'my-beginning-of-line-text-or-visual-line)
(keymap-global-set "C-o" 'open-line-saving-indent)

;; Configure indentation
(setq-default indent-tabs-mode nil
              tab-width 2
              fill-column 80)
(keymap-set prog-mode-map "RET" #'newline-and-indent)

;; Windows management

(defun my-delete-window-frame (&optional window)
  "Delete the current WINDOW or frame if the window is one exists in frame."
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
  (eval-and-compile
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
;; (require 'vertico)
;; (vertico-mode)

(provide 'small-init)
;;; small-init.el ends here
;; Local Variables:
;; my-dont-reload-on-save: t
;; End:
