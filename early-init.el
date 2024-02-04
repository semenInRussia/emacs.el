(message "Make emacs fast again!!")
;; --- Donald TrAmp

(defvar gc-cons-threshold-original)
(defvar file-name-handler-alist-original)

(setq gc-cons-threshold most-positive-fixnum)

;; PERF: A second, case-insensitive pass over `auto-mode-alist' is time wasted.
(setq auto-mode-case-fold nil)

;; from https://www.reddit.com/r/emacs/comments/3kqt6e/2_easy_little_known_steps_to_speed_up_emacs_start/
(setq file-name-handler-alist-original file-name-handler-alist
      file-name-handler-alist nil)

(add-hook
 'after-init-hook
 (defun my-reset-some-hacks ()
   "Reset the effects some performance hacks to initial state."
   (setq file-name-handler-alist file-name-handler-alist-original)
   ;; PERF: But make an exception for `gc-cons-threshold', which I
   ;;   think all Emacs users and configs will benefit from. Still,
   ;;   setting it to `most-positive-fixnum' is dangerous if downstream
   ;;   does not reset it later to something reasonable, so I use 16mb
   ;;   as a best fit guess. It's better than Emacs' 80kb default.
   (setq gc-cons-threshold (* 16 1024 1024))))

;; PERF: Don't use precious startup time checking mtime on elisp bytecode.
(setq load-prefer-newer noninteractive)

;; I prefer `straight' over `packages.el'
(setq package-enable-at-startup nil)

;; PERF: A second, case-insensitive pass over `auto-mode-alist' is time wasted.
;;(setq auto-mode-case-fold nil)

;; PERF: Disable bidirectional text scanning for a modest performance boost.
;;   I've set this to `nil' in the past, but the `bidi-display-reordering's docs
;;   say that is an undefined state and suggest this to be just as good:
(setq-default bidi-display-reordering 'left-to-right
              bidi-paragraph-direction 'left-to-right)

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

;; Emacs "updates" its ui more often than it needs to, so slow it down slightly
(setq idle-update-delay 1.0)  ; default is 0.5

;; PERF: Disabling BPA makes redisplay faster, but might produce incorrect
;;   reordering of bidirectional text with embedded parentheses (and other
;;   bracket characters whose 'paired-bracket' Unicode property is non-nil).
(setq bidi-inhibit-bpa t)  ; Emacs 27+ only

;; Reduce rendering/line scan work for Emacs by not rendering cursors or regions
;; in non-focused windows.
(setq-default cursor-in-non-selected-windows nil)
(setq highlight-nonselected-windows nil)

;; And set these to nil so users don't have to toggle the modes twice to
;; reactivate them.
(setq menu-bar-mode nil
      tool-bar-mode nil
      scroll-bar-mode nil)

;; More performant rapid scrolling over unfontified regions. May cause brief
;; spells of inaccurate syntax highlighting right after scrolling, which should
;; quickly self-correct.
(setq fast-but-imprecise-scrolling t)

;; Don't ping things that look like domain names.
(defvar ffap-machine-p-known 'reject)

;; Font compacting can be terribly expensive, especially for rendering icon
;; fonts on Windows. Whether disabling it has a notable affect on Linux and Mac
;; hasn't been determined, but do it anyway, just in case. This increases memory
;; usage, however!
(setq inhibit-compacting-font-caches t)

;; Performance on Windows is considerably worse than elsewhere. We'll need
;; everything we can get.
(when (boundp 'w32-get-true-file-attributes)
  (eval '(setq w32-get-true-file-attributes nil ; decrease file IO workload
               w32-pipe-read-delay 0            ; faster IPC
               w32-pipe-buffer-size (* 64 1024))))  ; read more at a time (was 4K)

;; Introduced in Emacs HEAD (b2f8c9f), this inhibits fontification while
;; receiving input, which should help a little with scrolling performance.
(setq redisplay-skip-fontification-on-input t)
