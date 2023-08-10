;;; my-afk.el --- Load some heavy packages after some seconds of AFK -*- lexical-binding: t; -*-

;; Copyright (C) 2023 semenInRussia

;; Author: semenInRussia <hrams205@gmail.com>
;; Version: 0.1
;; Homepage: https://github.com/semeninrussia/emacs.el

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; When you in afk, load some frequently used things to don't load them after
;; (idea grabbed from the Doomemacs, but I didn't find the implementation, so
;; create my own)

;;; Code:

(defcustom my-use-afk-modules
  '(;; `consult' command `consult-buffer', really frequently useful by me,
    ;; I use it in 99% of Emacs sessions, load it when in AFK
    consult
    ;; auto-completion (`corfu')
    corfu
    cape  ; it provides some backends for auto-completion (CAPFs)
    ;; my configuration really tied with `embark'.
    ;; I use it for: change reGisTeR of the region, kill the sexp at point,
    ;; do things on the minibuffer items, browse the URL at the cursor
    ffap  ; a dependency
    embark
    ;; I frequently use `dired', but it have a bad start up time, load it in AFK
    dired
    ;; I use `ace-window' (alternative to C-x o) in 100% of cases.
    ;; I don't need to wait 1-2secs before it.
    avy        ; dep
    ace-window
    ;; lsp server (`eglot')
    imenu
    ert
    flymake
    external-completion
    jsonrpc
    xref
    eglot
    ;; `magit' is really big, so I load it in AFK
    magit-section
    with-editor
    transient
    magit-base
    magit-git
    add-log
    pcvs-util
    gmm-utils
    mail-utils
    mm-util
    mailabbrev
    mail-parse
    mm-util
    mm-bodies
    mml
    puny
    rmc
    yank-media
    mailcap
    sendmail
    message
    log-edit
    git-commit
    ;; org-mode
    org-compat
    calendar
    find-func
    format-spec
    org-keys
    ol
    oc
    org-table
    org-fold
    org-cycle
    ;; fast-exec
    fast-exec
    ;; my translator
    gts-core
    gts-implements
    gts-engine-bing
    go-translate
    ;; run-command
    run-command)
  "This is the list of modules which should be loaded after some seconds of AFK.

Emacs will load them when I am not doing anything, so I won't wait loading of
them when they really needed"
  :group 'my
  :type '(repeat (repeat symbol)))

(defcustom my-use-afk-timeout 2
  "Secs in AFK to load next heavy thind from the `my-use-afk-modules'."
  :group 'my
  :type 'number)

(defcustom my-use-afk-timeout-between-loads
  1
  "Secs between loadidng heavy modules."
  :group 'my
  :type 'number)

(defvar my-use-afk-counter 0
  "The counter that tell about the progress of loading heavy things in an AFK time.

Don't set it manually, `my-use-afk-next' will load the heavy next thing and
change the value of this variable")

(defvar my-use-afk-timer nil
  "Timer for load heavy things in AFK.")

(defun my-use-afk-next ()
  "Setup timer to load the rest heavy things after some secs of AFK.

It load the respective Emacs package from the `my-use-afk-modules' list after
`my-use-afk-timeout' seconds depends on `my-use-afk-counter', change the value
of this counter and run timer to load itself after the seconds of AFK"
  (interactive)
  (let ((module (nth my-use-afk-counter my-use-afk-modules)))
    (cond
     ((null module)                     ; all things already loaded -> stop
      (message "All heavy things have already loaded")
      (my-use-afk-stop)
      t)
     ((featurep module)      ; the current thing already loaded -> load the next
      (message "The module have already loaded: %s" module)
      (cl-incf my-use-afk-counter)
      (my-use-afk-next))
     (t                                 ; just load the current module
      (message "Load the module, because u in AFK: %s..." module)
      (unless (ignore-errors (require module))  ; can't load module
        (message "Can't load the module in afk: %s" module))
      (cl-incf my-use-afk-counter)
      (run-with-timer
       my-use-afk-timeout-between-loads
       nil
       (lambda ()
         ;; this code will runned after `my-use-afk-timeout-between-loads'
         ;; seconds after loading a heavy thing.
         ;;
         ;; here check that all time before load a heavy thing, the user wasn't
         ;; doing anything, in this case users still in AFK, so load the next
         ;; heavy thing
         (when (time-less-p
                (time-add
                 my-use-afk-timeout
                 my-use-afk-timeout-between-loads)
                (current-idle-time))
           (my-use-afk-next))))))))

(defun my-use-afk-stop ()
  "Don't using AFK time to load heavy things anymore."
  (interactive)
  (cancel-timer my-use-afk-timer))

(defun my-use-afk-first-start ()
  "Load all heavy things in AFK starting from the first one.

It useful when an error in AFK loading was occured and you need to try fix it."
  (interactive)
  (setq my-use-afk-counter 0)
  (my-use-afk-setup-timer))

(defun my-use-afk-setup-timer ()
  "Load the next heavy thing after some seconds in AFK."
  (setq my-use-afk-timer
        (run-with-idle-timer my-use-afk-timeout t 'my-use-afk-next)))

(define-minor-mode my-use-afk-mode
  "Use time when user do nothing (AFK time) to load heavy things.

It's useful, because when this mode is enabled and user doing nothing Emacs do a
hard work to don't do it when user type a text."
  :global t
  :group 'misc
  :init-value nil
  (if my-use-afk-mode
      (my-use-afk-first-start)
    (my-use-afk-stop)))

;; when Emacs is started start using AFK
(add-hook 'after-init-hook 'my-use-afk-mode)

(provide 'my-use-afk)
;;; my-use-afk.el ends here
