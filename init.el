(setq initial-buffer-choice "~/Start.org")

(require 'package)

(setq package-archives
      '(("melpa-stable" . "http://stable.melpa.org/packages/")
        ("melpa"        . "https://melpa.org/packages/")
        ("org"          . "https://orgmode.org/elpa/")
        ("elpa"         . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)

(add-to-list 'load-path "~/.emacs.d/lisp")

(add-to-list 'load-path "~/projects/fast-exec.el")
(add-to-list 'load-path "~/projects/porth-mode")
(add-to-list 'load-path "~/projects/emacs-run-command")
(add-to-list 'load-path "~/projects/simple-indention.el")

(use-package s :ensure t)

(use-package f :ensure t)

(use-package dash :ensure t :init (global-dash-fontify-mode 1))

(defun my-try-delete (path &optional force)
  "If PATH is exists isn't exists, then just do nothing, otherwise delete PATH.

If FORCE is t, a directory will be deleted recursively."
  (when (f-exists-p path)
    (f-delete path force)))

(defun my-try-move (from to)
  "Move FROM to TO, if FROM is exists."
  (when (f-exists-p from)
    (f-move from to)))

(defun my-files-with-extension (ext dir)
  "Return list of files in DIR which has extension EXT."
  (->>
   dir
   (f-files)
   (--filter (f-ext-p it ext))))

(defun my-humanize-string (s)
  "Humanize normalized S."
  (->> s (s-replace "-" " ") (s-titleize)))

(defun my-normalize-string (s)
  "Normalize humanized S for computer."
  (->>
   s
   (s-downcase)
   (s-replace " " "-")
   (s-replace "'" "")
   (s-replace "\"" "")))

(defun my-one-of-prefixes-p (prefixes s)
  "Return t, when S has one of PREFIXES."
  (->>
   prefixes
   (--some (s-prefix-p it s))))

(defun my-current-line (&optional pos)
        "Return line at POS, POS defaults to `point'."
        (setq pos (or pos (point)))
        (save-excursion
          (goto-char pos)
          (buffer-substring-no-properties
           (point-at-bol)
           (point-at-eol))))

(defun my-current-line-prefix-p (p)
  "Return t, when current text line starts with P."
  (s-prefix-p p (my-current-line)))

(defun my-current-line-is-empty-p (&optional pt)
  "Return t, when line on PT is empty."
  (setq pt (or pt (point)))
  (->>
   pt
   (my-current-line)
   (s-trim)
   (string-equal "")))

(defun if-Emacs-org-then-org-babel-tangle ()
  "If current open file is Emacs.org, then `org-babel-tangle`."
  (interactive)

  (when (s-equals? (f-filename buffer-file-name) "Emacs.org")
    (org-babel-tangle)))

(add-hook 'after-save-hook 'if-Emacs-org-then-org-babel-tangle)

(setq user-full-name    "Semen Khramtsov"
      user-mail-address "hrams205@gmail.com"
      user-birthday     "2007-01-29"
      user-name         "semenInRussia"
      user-os           "Windows" ; "Windows" or "Linux"
      )

(defun user-os-windows-p ()
  "If user have os Windows, then return t.
Info take from var `user-os`, user must set it."
  (interactive)
  (s-equals? user-os "Windows"))

(if (s-equals? (format-time-string "%Y-%m-%d") user-birthday)
    (animate-birthday-present))

(use-package yasnippet
    :ensure t
    :init
    (yas-global-mode 1)
    :custom
    (yas-snippet-dirs '("~/.emacs.d/snippets"))
    (yas-wrap-around-region t))

(use-package yasnippet-snippets
    :ensure t)

(use-package flycheck
    :ensure t
    :config
    '(custom-set-variables
      '(flycheck-display-errors-function
        #'flycheck-pos-tip-error-messages))
    (global-flycheck-mode 1))

(use-package company
    :ensure t
    :custom
    (company-idle-delay                 0.3)
    (company-minimum-prefix-length      2)
    (company-show-numbers               t)
    (company-tooltip-limit              15)
    (company-tooltip-align-annotations  t)
    (company-tooltip-flip-when-above    t)
    (company-dabbrev-ignore-case        nil)
    :config
    (add-to-list 'company-backends 'company-keywords)
    (global-company-mode 1))

(defvar company-mode/enable-yas t
  "Enable yasnippet for all backends.")

(defun company-mode/backend-with-yas (backend)
  (if (or (not company-mode/enable-yas)
          (and (listp backend) (member 'company-yasnippet backend)))
      backend
    (append (if (consp backend) backend (list backend))
            '(:with company-yasnippet))))

(setq company-backends
      (mapcar #'company-mode/backend-with-yas company-backends))

(use-package company-box
    :ensure t
    :hook (company-mode . company-box-mode))

(use-package format-all
    :ensure t)



(use-package xah-fly-keys
    :config
  (xah-fly-keys-set-layout "qwerty")
  (xah-fly-keys 1)
  (define-key xah-fly-command-map (kbd "SPC l") nil)
  (define-key xah-fly-command-map (kbd "SPC j") nil)
  (define-key xah-fly-command-map (kbd "SPC SPC") nil))

(defvar my-local-major-mode-map nil
  "My map for current `major-mode'")

(defun my-local-major-mode-map-run ()
  "Run `my-local-major-mode-map'."
  (interactive)
  (set-transient-map my-local-major-mode-map))

(define-key xah-fly-command-map (kbd "SPC l") 'my-local-major-mode-map-run)

(add-to-list 'use-package-keywords :major-mode-map)

(defun use-package-normalize/:major-mode-map (name keyword args)
  "Normalizer of :major-mode-map for `use-package'."
  (let* (map-name modes)
    (if (eq (-first-item args) t) ; All by Default
        (list (symbol-name name) (list name))
      (cl-typecase (-first-item args)
        (list (setq modes (-first-item args)))
        (symbol (setq map-name (symbol-name (-first-item args))))
        (string (setq map-name (-first-item args))))
      (cl-typecase (-second-item args)
        (list (setq modes (-first-item args)))
        (symbol (setq map-name (symbol-name (-first-item args))))
        (string (setq map-name (-first-item args))))
      (list
       (or map-name (symbol-name name))
       modes))))

(defun use-package-handler/:major-mode-map (name keyword
                                            map-name-and-modes rest state)
  (let* ((map-name (car map-name-and-modes))
         (modes (-second-item map-name-and-modes))
         (modes-hooks (--map (intern (s-append "-hook" (symbol-name it)))
                             modes))
         (map (intern (s-concat "my-" map-name "-local-map"))))
    (setq rest
          (-concat
           rest
           `(:config
             ((unless (boundp ',map)
                (define-prefix-command ',map))
              (--each ',modes-hooks
                (add-hook it
                          (lambda ()
                            (setq-local my-local-major-mode-map
                                        ',map))))))))
    (use-package-process-keywords name rest)))

(require 'fast-exec)

(fast-exec/enable-some-builtin-supports haskell-mode
                                        flycheck
                                        magit
                                        org-agenda
                                        deadgrep
                                        projectile
                                        skeletor
                                        yasnippet
                                        format-all
                                        wikinforg
                                        suggest
                                        devdocs
                                        helm-wikipedia)

(fast-exec/initialize)

(define-key xah-fly-command-map (kbd "=") 'fast-exec/exec)

(defmacro define-key-when (fun-name map key def pred)
  "Define to KEY in MAP DEF when PRED return t or run old command.
Instead of KEY will command FUN-NAME"
  (let ((old-def (key-binding key)))
    `(unless (eq (key-binding ,key) #',fun-name)
       (defun ,fun-name ()
         ,(s-lex-format "Run `${old-def}' or `${def}'.")
         (interactive)
         (call-interactively
          (if (funcall ,pred)
              ,def
            #',old-def)))
       (define-key ,map ,key #',fun-name))))

(use-package swiper-helm
    :ensure t
    :bind (:map xah-fly-command-map
                ("'" . swiper-helm)))

(use-package deadgrep
    :ensure t
    :bind (:map
           xah-fly-command-map
           ("SPC '" . deadgrep)))

(define-key xah-fly-command-map (kbd "SPC SPC r") 'projectile-replace)

(use-package imenu
    :custom (imenu-auto-rescan t))

(bind-keys :map xah-fly-command-map
           ("SPC SPC SPC" . helm-imenu))

(use-package imenu-anywhere
    :ensure t
    :bind (:map xah-fly-command-map
                ("SPC SPC n" . imenu-anywhere)))

(use-package recentf
    :config (recentf-mode 69) ; Lol!
    :bind ((:map xah-fly-command-map)
           ("SPC k f" . 'recentf-open-files))
    :hook ((recentf-dialog-mode) . 'xah-fly-insert-mode-activate))

(use-package comment-dwim-2
    :ensure t
    :bind (:map xah-fly-command-map
                ("z" . comment-dwim-2)))

(use-package rg
    :ensure t)

(use-package dumb-jump
    :ensure t
    :custom
    (dumb-jump-force-searcher 'rg)
    (dumb-jump-prefer-searcher 'rg)
    :bind (:map xah-fly-command-map ("SPC SPC ." . dumb-jump-back))
    :init
    (add-hook 'xref-backend-functions #'dumb-jump-xref-activate))

(defun my-buffer-list-or-edit-lines ()
  "Do `helm-buffer-list' or `mc/edit-lines'."
  (interactive)
  (if (use-region-p)
      (call-interactively #'mc/edit-lines)
    (call-interactively #'helm-buffers-list)))

(defun my-mark-all ()
  "If enable `multiple-cursors', then mark all like this, other mark buffer."
  (interactive)
  (if multiple-cursors-mode
      (mc/mark-all-words-like-this)
    (mark-whole-buffer)))

(defun my-bob-or-mc-align ()
  "If enable `multiple-cursors', then mark then align by regexp, other bob.
BOB - is `beginning-of-buffer'"
  (interactive)
  (if multiple-cursors-mode
      (call-interactively 'mc/vertical-align)
    (beginning-of-buffer)))

(defun my-eob-or-mc-align-with-space ()
  "If enable `multiple-cursors', then align by spaces, other bob.
EOB - is `end-of-buffer'"
  (interactive)
  (if multiple-cursors-mode
      (mc/vertical-align-with-space)
    (end-of-buffer)))

(defun my-mc-mark-like-this-or-edit-lines ()
  "If region on some lines, `mc/edit-lines' other `mc/mark-next-like-this'."
  (interactive)
  (if (and (region-active-p)
           (not (eq (line-number-at-pos (region-beginning))
                    (line-number-at-pos (region-end)))))
      (call-interactively 'mc/edit-lines)
    (call-interactively 'mc/mark-next-like-this-word)))

(use-package multiple-cursors :ensure t)

(use-package multiple-cursors
    :config
  (add-to-list 'mc--default-cmds-to-run-once 'my-mark-all)
  (add-to-list 'mc--default-cmds-to-run-once
               'my-mc-mark-like-this-or-edit-lines)
  (add-to-list 'mc--default-cmds-to-run-once
               'my-bob-or-mc-align)
  (add-to-list 'mc--default-cmds-to-run-once
               'my-eob-or-align-with-spaces)
  (add-to-list 'mc--default-cmds-to-run-once
               'my-mc-mark-like-this-or-edit-lines)
  (add-to-list 'mc--default-cmds-to-run-once
               'toggle-input-method)
  :bind
  (:map xah-fly-command-map
        ("SPC f"         . 'my-buffer-list-or-edit-lines)
        ("7"         . my-mc-mark-like-this-or-edit-lines)
        ("SPC 7"     . mc/mark-previous-like-this-word)
        ("SPC TAB 7" . mc/reverse-regions)
        ("SPC d 7"   . mc/unmark-next-like-this)
        ("SPC h"     . my-bob-or-mc-align)
        ("SPC n"     . my-eob-or-mc-align-with-space)
        ("SPC a"     . my-mark-all)))

(use-package avy
    :ensure t
    :custom (avy-background t)
    (avy-translate-char-function #'translate-char-from-russian)
    :bind ((:map xah-fly-command-map)
           ("n"     . nil) ; by default this is `isearch', so i turn
           ;; this to keymap
           ("n n"   . 'avy-goto-char)
           ("n v"   . 'avy-yank-word)
           ("n x"   . 'avy-teleport-word)
           ("n c"   . 'avy-copy-word)
           ("n 8"   . 'avy-mark-word)
           ("n d"   . 'avy-kill-word-stay)
           ("n s ;" . 'avy-insert-new-line-at-eol)
           ("n s h" . 'avy-insert-new-line-at-bol)
           ("n 5"   . 'avy-zap)
           ("n TAB" . 'avy-transpose-words)
           ("n w"   . 'avy-clear-line)
           ("n -"   . 'avy-sp-splice-sexp-in-word)
           ("n r"   . 'avy-kill-word-move)
           ("n o"   . 'avy-change-word)
           ("n 9"   . 'avy-sp-change-enclosing-in-word)
           ("n z"   . 'avy-comment-line)
           ("n t v" . 'avy-copy-region)
           ("n t d" . 'avy-kill-region)
           ("n t x" . 'avy-move-region)
           ("n t c" . 'avy-kill-ring-save-region)
           ("n ;"   . 'avy-goto-end-of-line)
           ("n h"   . 'avy-goto-begin-of-line-text)
           ("n k v" . 'avy-copy-line)
           ("n k x" . 'avy-move-line)
           ("n k c" . 'avy-kill-ring-save-whole-line)
           ("n k d" . 'avy-kill-whole-line)))

(defun translate-char-from-russian (russian-char)
  "Translate RUSSIAN-CHAR to corresponding char on qwerty keyboard.
I am use йцукенг russian keyboard."
  (cl-case russian-char
    (?й ?q)
    (?ц ?w)
    (?у ?e)
    (?к ?r)
    (?е ?t)
    (?н ?y)
    (?г ?u)
    (?ш ?i)
    (?щ ?o)
    (?з ?p)
    (?ф ?a)
    (?ы ?s)
    (?в ?d)
    (?а ?f)
    (?п ?g)
    (?р ?h)
    (?о ?j)
    (?л ?k)
    (?д ?l)
    (?я ?z)
    (?ч ?x)
    (?с ?c)
    (?м ?v)
    (?и ?b)
    (?т ?n)
    (?ь ?m)
    (t russian-char)))

(defun avy-goto-word-1-with-action (char action &optional arg beg end symbol)
  "Jump to the currently visible CHAR at a word start.
The window scope is determined by `avy-all-windows'.
When ARG is non-nil, do the opposite of `avy-all-windows'.
BEG and END narrow the scope where candidates are searched.
When SYMBOL is non-nil, jump to symbol start instead of word start.
Do action of `avy' ACTION.'"
  (interactive (list (read-char "char: " t) current-prefix-arg))
  (avy-with avy-goto-word-1
    (let* ((str (string char))
           (regex
            (cond
              ((string= str ".")
               "\\.")
              ((and avy-word-punc-regexp
                    (string-match avy-word-punc-regexp str))
               (regexp-quote str))
              ((<= char 26)
               str)
              (t (concat (if symbol "\\_<" "\\b") str)))))
      (avy-jump regex
                :window-flip arg
                :beg beg
                :end end
                :action action))))

(defun avy-zap (char &optional arg)
  "Zapping to next CHAR navigated by `avy'."
  (interactive "cchar:\nP")
  (avy-jump
   (s-concat (char-to-string char))
   :window-flip arg
   :beg (point-min)
   :end (point-max)
   :action 'avy-action-zap-to-char))

(defun avy-teleport-word (char &optional arg)
  "Teleport word searched by `arg' with CHAR.
Pass ARG to `avy-jump'."
  (interactive "cchar:\nP")
  (avy-goto-word-1-with-action char 'avy-action-teleport))

(defun avy-mark-word (char)
  "Mark word begining with CHAR searched by `avy'."
  (interactive "cchar: ")
  (avy-goto-word-1-with-action char 'avy-action-mark))

(defun avy-copy-word (char &optional arg)
  "Copy word searched by `arg' with CHAR.
Pass ARG to `avy-jump'."
  (interactive "cchar:\nP")
  (avy-goto-word-1-with-action char 'avy-action-copy))

(defun avy-yank-word (char &optional arg)
  "Paste word searched by `arg' with CHAR.
Pass ARG to `avy-jump'."
  (interactive "cchar:\nP")
  (avy-goto-word-1-with-action char 'avy-action-yank))

(defun avy-kill-word-stay (char &optional arg)
  "Paste word searched by `arg' with CHAR.
Pass ARG to `avy-jump'."
  (interactive "cchar:\nP")
  (avy-goto-word-1-with-action char 'avy-action-kill-stay))

(defun avy-kill-word-move (char &optional arg)
  "Paste word searched by `arg' with CHAR.
Pass ARG to `avy-jump'."
  (interactive "cchar:\nP")
  (avy-goto-word-1-with-action char 'avy-action-kill-move))

(defun avy-goto-line-1-with-action (action)
  "Goto line via `avy' with CHAR and do ACTION."
  (interactive)
  (avy-jump "^." :action action))

(defun avy-comment-line ()
  "With `avy' move to line and comment its."
  (interactive)
  (avy-goto-line-1-with-action 'avy-action-comment))

(defun avy-action-comment (pt)
  "Saving excursion comment line at point PT."
  (save-excursion (goto-char pt) (comment-line 1)))

(defun avy-sp-change-enclosing-in-word (ch)
  "With `avy' move to word starting with CH and `sp-change-enclosing'."
  (interactive "cchar:")
  (avy-goto-word-1-with-action ch 'avy-action-sp-change-enclosing))

(defun avy-action-sp-change-enclosing (pt)
  "Saving excursion `sp-change-enclosing' in word at point PT."
  (save-excursion (goto-char pt) (sp-change-enclosing)))

(defun avy-sp-splice-sexp-in-word (ch)
  "With `avy' move to word starting with CH and `sp-splice-sexp'."
  (interactive "cchar:")
  (avy-goto-word-1-with-action ch 'avy-action-sp-splice-sexp))

(defun avy-action-sp-splice-sexp (pt)
  "Saving excursion `sp-splice-sexp' in word at point PT."
  (save-excursion (goto-char pt) (sp-splice-sexp)))

(defun avy-change-word (ch)
  "With `avy' move to word starting with CH and change its any other."
  (interactive "cchar:")
  (avy-goto-word-1-with-action ch 'avy-action-change-word))

(defun avy-action-change-word (pt)
  "Saving excursion navigate to word at point PT and change its."
  (save-excursion
    (avy-action-kill-move pt)
    (insert (read-string "new word, please: " (current-kill 0)))))

(defun avy-transpose-words (char)
  "Goto CHAR via `avy' and transpose at point word to word at prev point."
  (interactive "cchar: ")
  (avy-goto-word-1-with-action char 'avy-action-transpose-words))

(defun avy-action-transpose-words (second-pt)
  "Goto SECOND-PT via `avy' and transpose at point to word at point ago."
  (avy-action-yank second-pt)
  (kill-sexp)
  (goto-char second-pt)
  (yank)
  (kill-sexp))

(defun avy-goto-begin-of-line-text (&optional arg)
  "Call `avy-goto-line' and move to the begin of the text of line.
ARG is will be passed to `avy-goto-line'"
  (interactive "p")
  (avy-goto-line arg)
  (beginning-of-line-text))

(defun avy-clear-line (&optional arg)
  "Move to any line via `avy' and clear this line from begin to end.
ARG is will be passed to `avy-goto-line'"
  (interactive "p")
  (avy-goto-line-1-with-action #'avy-action-clear-line))

(defun avy-action-clear-line (pt)
  "Move to PT, and clear current line, move back.
Action of `avy', see `avy-action-yank' for example"
  (save-excursion (goto-char pt) (clear-current-line)))

(defun avy-insert-new-line-at-eol ()
  "Move to any line via `avy' and insert new line at end of line."
  (interactive)
  (avy-goto-line-1-with-action #'avy-action-insert-new-line-at-eol))

(defun avy-action-insert-new-line-at-eol (pt)
  "Move to PT, and insert new line at end of line, move back.
Action of `avy', see `avy-action-yank' for example"
  (save-excursion
    (goto-char pt)
    (end-of-line)
    (newline)))

(defun avy-insert-new-line-at-bol ()
  "Move to any line via `avy' and insert new at beginning of line."
  (interactive)
  (avy-goto-line-1-with-action #'avy-action-insert-new-line-at-bol))

(defun avy-action-insert-new-line-at-bol (pt)
  "Move to PT, and insert new at beginning of line, move back.
Action of `avy', see `avy-action-yank' for example"
  (save-excursion
    (goto-char pt)
    (beginning-of-line)
    (newline)))

(use-package smartparens
    :ensure t
    :init
    (smartparens-global-mode 1)
    :bind (("RET"       . sp-newline)
           :map
           xah-fly-command-map
           (("]"         . 'sp-forward-slurp-sexp)
            ("["         . 'sp-forward-barf-sexp)
            ("M-("       . 'sp-wrap-round)
            ("M-["       . 'sp-wrap-square)
            ("M-{"       . 'sp-wrap-curly)
            ("-"         . 'sp-splice-sexp)
            ("SPC -"     . 'sp-rewrap-sexp)
            ("m"         . 'sp-backward-sexp)
            ("."         . 'sp-forward-sexp)
            ("SPC 1"     . 'sp-join-sexp)
            ("SPC SPC 1" . 'sp-split-sexp)
            ("SPC 9"     . 'sp-change-enclosing)
            ("SPC SPC g" . 'sp-kill-hybrid-sexp)
            ("SPC ="     . 'sp-raise-sexp)
            ("M-("       . 'sp-wrap-round)
            ("M-{"       . 'sp-wrap-curly))))

(require 'smartparens-config)

(defun delete-only-1-char ()
  "Delete only 1 character before point."
  (interactive)
  (backward-char)
  (delete-char 1)
  )

(define-key xah-fly-command-map (kbd "DEL") 'delete-only-1-char)

(defun mark-inner-or-expand-region ()
  "If text is selected, expand region, otherwise then mark inner of brackets."
  (interactive)
  (if (use-region-p)
      (call-interactively 'er/expand-region)
    (progn
      (-when-let (ok (sp-get-sexp))
        (sp-get ok
          (set-mark :beg-in)
          (goto-char :end-in))))))

(use-package expand-region
    :ensure t
    :bind
    (:map xah-fly-command-map
          ("1" . er/expand-region)
          ("9" . mark-inner-or-expand-region)
          ("m" . sp-backward-up-sexp)))

(defun kmacro-start-or-end-macro ()
  "If macro record have just started, then stop this record, otherwise start."
  (interactive)
  (if defining-kbd-macro
      (kmacro-end-macro 1)
    (kmacro-start-macro 1)))

(define-key xah-fly-command-map (kbd "\\") 'kmacro-start-or-end-macro)

(defun kmacro-call-macro-or-apply-to-lines (arg &optional top bottom)
  "If selected region, then apply macro to selected lines, otherwise call macro."
  (interactive
   (list
    1
    (if (use-region-p) (region-beginning) nil)
    (if (use-region-p) (region-end) nil)))

  (if (use-region-p)
      (apply-macro-to-region-lines top bottom)
    (kmacro-call-macro arg)))

(define-key xah-fly-command-map (kbd "SPC RET") 'kmacro-call-macro-or-apply-to-lines)

(use-package string-edit
    :ensure t
    :bind (:map xah-fly-command-map
                ("SPC `" . string-edit-at-point)))

(defun my-drag-stuff-left-char ()
  "Drag char to left."
  (interactive)
  (transpose-chars -1))

(defun my-drag-stuff-right-char ()
  "Drag char to right."
  (interactive)
  (transpose-chars 1))

(defcustom my-left-draggers nil
  "Functions, which drag stuff to left, or return nil.
Is used in `my-drag-stuff-left'.")

(defun my-drag-stuff-left ()
  "My more general and functional version of `drag-stuff-left'."
  (interactive)
  (--find (call-interactively it) my-left-draggers)
  (message "Start dragging, use keys u, i, o, k. Type RET for exit..."))

(defcustom my-right-draggers nil
  "Functions, which drag stuff to right, or return nil.
Is used in `my-drag-stuff-right'.")

(defun my-drag-stuff-right ()
  "My more general and functional version of `drag-stuff-right'."
  (interactive)
  (--find (call-interactively it) my-right-draggers)
  (message "Start dragging, use keys u, i, o, k. Type RET for exit..."))

(defcustom my-up-draggers nil
  "Functions, which drag stuff to up, or return nil.
Is used in `my-drag-stuff-up'.")

(defun my-drag-stuff-up ()
  "My more general and functional version of `drag-stuff-up'."
  (interactive)
  (--find (call-interactively it) my-up-draggers)
  (message "Start dragging, use keys u, i, o, k. Type RET for exit..."))

(defcustom my-down-draggers nil
  "Functions, which drag stuff to up, or return nil.
Is used in `my-drag-stuff-down'.")

(defun my-drag-stuff-down ()
  "My more general and functional version of `drag-stuff-down'."
  (interactive)
  (--find (call-interactively it) my-down-draggers)
  (message "Start dragging, use keys u, i, o, k. Type RET for exit..."))

(defun add-left-dragger (f)
  "Add F to list draggers for `my-drag-stuff-left'."
  (when (-contains-p my-left-draggers f)
    (setq my-left-draggers (remove f my-left-draggers)))
  (setq my-left-draggers (cons f my-left-draggers)))

(defun add-right-dragger (f)
  "Add F to list draggers for `my-drag-stuff-right'."
  (when (-contains-p my-right-draggers f)
    (setq my-right-draggers (remove f my-right-draggers)))
  (setq my-right-draggers (cons f my-right-draggers)))

(defun add-up-dragger (f)
  "Add F to list draggers for `my-drag-stuff-up'."
  (when (-contains-p my-up-draggers f)
    (setq my-up-draggers (remove f my-up-draggers)))
  (setq my-up-draggers (cons f my-up-draggers)))

(defun add-down-dragger (f)
  "Add F to list draggers for `my-drag-stuff-down'."
  (when (-contains-p my-down-draggers f)
    (setq my-down-draggers (remove f my-down-draggers)))
  (setq my-down-draggers (cons f my-down-draggers)))

(defun add-right-dragger (f)
  "Add F to list draggers for `my-drag-stuff-right'."
  (when (-contains-p my-right-draggers f)
    (setq my-right-draggers (remove f my-right-draggers)))
  (setq my-right-draggers (cons f my-right-draggers)))

(defcustom my-drag-stuff-functions '(my-drag-stuff-up
                                     my-drag-stuff-down
                                     my-drag-stuff-right
                                     my-drag-stuff-left
                                     my-drag-stuff-right-char
                                     my-drag-stuff-left-char)
  "List of my functions, which always drag stuffs.")

(defun my-last-command-is-drag-stuff-p ()
  "Get t, when last command is one of `my-drag-stuff-functions'."
  (interactive)
  (-contains-p my-drag-stuff-functions last-command))

(defvar my-last-command-is-drag-stuff nil
  "If last command is one of my functions which draged word then this in true.")

(defun my-last-command-is-dragged-stuff-p ()
  "Return t, when last command dragged someone stuff."
  (or
   (my-last-command-is-drag-stuff-p)
   (and
    (s-contains-p "drag-stuff" (symbol-name last-command))
    my-last-command-is-drag-stuff)))

(defmacro my-define-stuff-key (keymap key normal-command drag-command)
  "Define in KEYMAP to KEY command when run NORMAL-COMMAND or DRAG-COMMAND."
  (let ((command-name (intern
                       (s-concat
                        "my-"
                        (symbol-name (eval normal-command))
                        "-or-"
                        (symbol-name (eval drag-command))))))
    `(progn
       (defun ,command-name ()
         ,(s-lex-format "Run `${normal-command}' or `${drag-command}'.")
         (interactive)
         (let* ((is-drag (my-last-command-is-dragged-stuff-p)))
           (setq my-last-command-is-drag-stuff is-drag)
           (if is-drag
               (call-interactively ,drag-command)
             (call-interactively ,normal-command))))
       (define-key ,keymap ,key #',command-name))))

(defun stop-drag ()
  "Stop drag, just something print, and nothing do, set to nil something."
  (interactive)
  (setq my-last-command-is-drag-stuff nil)
  (message "Turn `drag' to normal!"))

(define-key-when
    my-insert-new-line-or-nothing
    xah-fly-command-map
  ""
  'stop-drag
  'my-last-command-is-dragged-stuff-p)

(my-define-stuff-key
 xah-fly-command-map
 "j"
 #'backward-char
 #'my-drag-stuff-left-char)

(my-define-stuff-key
 xah-fly-command-map
 "l"
 #'forward-char
 #'my-drag-stuff-right-char)

(my-define-stuff-key
 xah-fly-command-map
 "o"
 #'syntax-subword-forward
 #'my-drag-stuff-right)

(my-define-stuff-key
 xah-fly-command-map
 "u"
 #'syntax-subword-backward
 #'my-drag-stuff-left)

(my-define-stuff-key
 xah-fly-command-map
 "i"
 #'previous-line
 #'my-drag-stuff-up)

(my-define-stuff-key
 xah-fly-command-map
 "k"
 #'next-line
 #'my-drag-stuff-down)

(use-package drag-stuff
    :ensure t
    :config
    (drag-stuff-global-mode t)
    :bind
    ((:map xah-fly-command-map)
     ("SPC TAB j" . 'my-drag-stuff-left-char)
     ("SPC TAB l" . 'my-drag-stuff-right-char)
     ("SPC TAB i" . 'my-drag-stuff-up)
     ("SPC TAB k" . 'my-drag-stuff-down)
     ("SPC TAB o" . 'my-drag-stuff-right)
     ("SPC TAB u" . 'my-drag-stuff-left)
     ("SPC TAB ." . 'transpose-sexps)
     ("SPC TAB m" . 'transpose-sexps)
     ("SPC TAB n" . 'avy-transpose-lines-in-region)
     ("SPC TAB t" . 'transpose-regions)))

(add-left-dragger  #'drag-stuff-left)
(add-right-dragger #'drag-stuff-right)
(add-up-dragger    #'drag-stuff-up)
(add-down-dragger  #'drag-stuff-down)

(defun my-org-mode-in-heading-start-p ()
  "Return t, when current position now in start of org's heading."
  (interactive "d")
  (and
   (not (org-in-src-block-p))
   (my-current-line-prefix-p "*")))

(defun my-drag-org-heading-right ()
  "Drag Org's heading to right."
  (interactive)
  (when (and
         (eq major-mode 'org-mode)
         (or
          (my-org-mode-in-heading-start-p)
          (org-at-table-p)))
    (org-metaright)
    t))

(defun my-drag-org-heading-left ()
  "Drag Org's heading to left."
  (interactive)
  (when (and
         (eq major-mode 'org-mode)
         (or
          (my-org-mode-in-heading-start-p)
          (org-at-table-p)))
    (org-metaleft)
    t))

(defun my-drag-org-heading-up ()
  "Drag Org's heading to up."
  (interactive)
  (when (and
         (eq major-mode 'org-mode)
         (or
          (my-org-mode-in-heading-start-p)
          (org-at-table-p)))
    (org-metaup)
    t))

(defun my-drag-org-heading-down ()
  "Drag Org's heading to down."
  (interactive)
  (when (or
         (org-at-table-p)
         (my-org-mode-in-heading-start-p))
    (org-metadown)
    t))

(add-right-dragger #'my-drag-org-heading-right)
(add-left-dragger #'my-drag-org-heading-left)
(add-down-dragger #'my-drag-org-heading-down)
(add-up-dragger #'my-drag-org-heading-up)

(defun delete-and-edit-current-line ()
  "Delete current line and instroduce to insert mode."
  (interactive)
  (beginning-of-line-text)
  (kill-line)
  (xah-fly-insert-mode-init)
  )

(define-key xah-fly-command-map (kbd "w") 'delete-and-edit-current-line)

(defun clear-current-line ()
  "Clear content of current line (including whitespaces)."
  (interactive)
  (kill-region (line-beginning-position) (line-end-position))
  )

(define-key xah-fly-command-map (kbd "SPC w") 'clear-current-line)

(defun select-current-or-next-word ()
  "If word was selected, then move to next word, otherwise select word."
  (interactive)
  (if (use-region-p)
      (forward-word)
    (xah-extend-selection))
  )

(define-key xah-fly-command-map (kbd "8") 'select-current-or-next-word)

(defun delete-current-text-block-or-cancel-selection ()
  "If text is selected, then cancel selection, otherwise delete current block."
  (interactive)
  (if (use-region-p)
      (deactivate-mark)
    (xah-delete-current-text-block)))

(define-key xah-fly-command-map (kbd "g") nil)
(define-key xah-fly-command-map (kbd "g") 'delete-current-text-block-or-cancel-selection)

(define-key-when
    my-exchange-point-and-mark-or-splice-sexp
    xah-fly-command-map
  "-"
  'exchange-point-and-mark
  'use-region-p)

(defun open-line-saving-indent ()
  "Inserting new line, saving position and inserting new line."
  (interactive)
  (newline)
  (unless (s-blank-p (s-trim (thing-at-point 'line t)))
    (indent-according-to-mode))
  (forward-line -1)
  (end-of-line)
  (delete-horizontal-space t))

(define-key xah-fly-command-map (kbd "s") 'open-line-saving-indent)

(defun open-line-below ()
  (interactive)
  (end-of-line)
  (newline)
  (indent-for-tab-command))

(defun open-line-above ()
  (interactive)
  (beginning-of-line)
  (newline)
  (forward-line -1)
  (indent-for-tab-command))

(defun new-line-in-between ()
  (interactive)
  (newline)
  (save-excursion
    (newline)
    (indent-for-tab-command))
  (indent-for-tab-command))

(defun duplicate-current-line-or-region (arg)
  "Duplicates the current line or region ARG times.
If there's no region, the current line will be duplicated."
  (interactive "p")
  (if (region-active-p)
      (let ((beg (region-beginning))
            (end (region-end)))
        (duplicate-region arg beg end)
        (one-shot-keybinding "d" (λ (duplicate-region 1 beg end))))
    (duplicate-current-line arg)
    (one-shot-keybinding "d" 'duplicate-current-line)))

(defun one-shot-keybinding (key command)
  (set-temporary-overlay-map
   (let ((map (make-sparse-keymap)))
     (define-key map (kbd key) command)
     map) t))

(defun replace-region-by (fn)
  (let* ((beg (region-beginning))
         (end (region-end))
         (contents (buffer-substring beg end)))
    (delete-region beg end)
    (insert (funcall fn contents))))

(defun duplicate-region (&optional num start end)
  "Duplicates the region bounded by START and END NUM times.
If no START and END is provided, the current region-beginning and
region-end is used."
  (interactive "p")
  (save-excursion
    (let* ((start (or start (region-beginning)))
           (end (or end (region-end)))
           (region (buffer-substring start end)))
      (goto-char end)
      (dotimes (i num)
        (insert region)))))

(defun paredit-duplicate-current-line ()
  (back-to-indentation)
  (let (kill-ring kill-ring-yank-pointer)
    (paredit-kill)
    (yank)
    (newline-and-indent)
    (yank)))

(defun duplicate-current-line (&optional num)
  "Duplicate the current line NUM times."
  (interactive "p")
  (if (bound-and-true-p paredit-mode)
      (paredit-duplicate-current-line)
    (save-excursion
      (when (eq (point-at-eol) (point-max))
        (goto-char (point-max))
        (newline)
        (forward-char -1))
      (duplicate-region num (point-at-bol) (1+ (point-at-eol))))))

(defvar yank-indent-modes '(prog-mode
                            sgml-mode
                            js2-mode)
  "Modes in which to indent regions that are yanked (or yank-popped)")

(defvar yank-advised-indent-threshold 1000
  "Threshold (# chars) over which indentation does not automatically occur.")

(defun yank-advised-indent-function (beg end)
  "Do indentation, as long as the region isn't too large."
  (if (<= (- end beg) yank-advised-indent-threshold)
      (indent-region beg end nil)))

(defadvice yank (after yank-indent activate)
  "If current mode is one of 'yank-indent-modes, indent yanked text.
With prefix arg don't indent."
  (if (and (not (ad-get-arg 0))
           (--any? (derived-mode-p it) yank-indent-modes))
      (let ((transient-mark-mode nil))
        (yank-advised-indent-function (region-beginning) (region-end)))))

(defadvice yank-pop (after yank-pop-indent activate)
  "If current mode is one of 'yank-indent-modes, indent yanked text.
With prefix arg don't indent."
  (if (and (not (ad-get-arg 0))
           (member major-mode yank-indent-modes))
      (let ((transient-mark-mode nil))
        (yank-advised-indent-function (region-beginning) (region-end)))))

(defun yank-unindented ()
  (interactive)
  (yank 1))

(defun kill-to-beginning-of-line ()
  (interactive)
  (kill-region (save-excursion (beginning-of-line) (point))
               (point)))

(bind-keys :map
           xah-fly-command-map
           ("SPC y"     . duplicate-current-line-or-region)
           ("SPC s"     . open-line-below)
           ("SPC e"     . kill-to-beginning-of-line)
           ("SPC k RET" . new-line-in-between)
           ("SPC SPC s" . open-line-above))

(defun insert-space-before-line ()
  "Saving position, insert space to beginning of current line."
  (interactive)
  (save-excursion (beginning-of-line-text)
                  (xah-insert-space-before))
  )

(defun insert-spaces-before-each-line-by-line-nums (start-line end-line)
  "Insert space before each line in region (`START-LINE`; `END-LINE`)."
  (unless (= 0 (+ 1 (- end-line start-line)))
    (goto-line start-line)
    (insert-space-before-line)
    (insert-spaces-before-each-line-by-line-nums (+ start-line 1) end-line))
  )

(defun insert-spaces-before-each-line (beg end)
  "Insert spaces before each selected line, selected line indentifier with `BEG` & `END`."
  (interactive "r")
  (save-excursion
    (let (deactivate-mark)
      (let ((begining-line-num (line-number-at-pos beg))
            (end-line-num (line-number-at-pos end)))
        (insert-spaces-before-each-line-by-line-nums begining-line-num end-line-num))))
  )

(defun insert-spaces-before-or-to-beginning-of-each-line (beg end)
  "Insert space, and if selected region, insert space to beginning of each line, text is should will indentifier with `BEG` & `END`."
  (interactive (list (if (use-region-p) (region-beginning))
                     (if (use-region-p) (region-end))))
  (if (use-region-p)
      (insert-spaces-before-each-line beg end)
    (xah-insert-space-before))
  )

(define-key xah-fly-command-map (kbd "p") nil)
(define-key xah-fly-command-map (kbd "p") 'insert-spaces-before-or-to-beginning-of-each-line)

(define-key xah-fly-command-map (kbd "m") 'backward-sexp)
(define-key xah-fly-command-map (kbd ".") 'forward-sexp)

(require 'rect)

(define-key xah-fly-command-map (kbd "SPC t") 'rectangle-mark-mode)
(define-key xah-fly-command-map (kbd "SPC v") 'yank-rectangle)

(defun rectangle-mark-mode-p ()
  "Return t, when `rectangle-mark-mode' is enabled."
  rectangle-mark-mode)

(define-key-when
    my-copy-rectangle-or-copy-line
    xah-fly-command-map
    "c"
  'copy-rectangle-as-kill
  'rectangle-mark-mode-p)

(define-key-when
    my-kill-rectangle-or-delete-char
    xah-fly-command-map
    "d"
  'kill-rectangle
  'rectangle-mark-mode-p)

(define-key-when
    my-kill-rectangle-or-kill-line
    xah-fly-command-map
  "x"
  'kill-rectangle
  'rectangle-mark-mode-p)

(define-key-when
    my-xah-activate-insert-mode-or-replace-rectangle
    xah-fly-command-map
  "f"
  'replace-rectangle
  'rectangle-mark-mode-p)

(define-key-when
    any-exchange-point-and-mark-or-splice-sexp
    xah-fly-command-map
  "-"
  'rectangle-exchange-point-and-mark
  'rectangle-mark-mode-p)

;;

(setq-default indent-tabs-mode nil)
(setq-default tab-width          4)
(setq-default c-basic-offset     4)
(setq-default standart-indent    4)
(setq-default lisp-body-indent   4)

(defun select-current-line ()
  "Select as region current line."
  (interactive)
  (forward-line 0)
  (set-mark (point))
  (end-of-line)
  )

(defun indent-line-or-region ()
  "If text selected, then indent it, otherwise indent current line."
  (interactive)
  (save-excursion
    (if (use-region-p)
        (indent-region (region-beginning) (region-end))
      (funcall indent-line-function)
      ))
  )

(global-set-key (kbd "RET") 'newline-and-indent)
(define-key xah-fly-command-map (kbd "q") 'indent-line-or-region)
(define-key xah-fly-command-map (kbd "SPC q") 'join-line)

(setq lisp-indent-function  'common-lisp-indent-function)

(define-key xah-fly-command-map (kbd "SPC .") 'xref-find-definitions)

(defmacro add-nav-to-imports-for-language (language to-imports-function)
  "Bind `TO-IMPORTS-FUNCTION` to `LANGUAGE`-map."
  `(let ((language-hook (intern (s-append "-hook" (symbol-name ',language)))))
     (add-hook
      language-hook
      (lambda ()
        (define-key
            xah-fly-command-map
            (kbd "SPC SPC i")
          ',to-imports-function)))))

(require 'face-remap)

(use-package visual-fill-column
    :ensure t)

(defun visual-fill (&optional width)
  (interactive)
  (or width (setq width 70))
  (setq-default visual-fill-column-width width
                visual-fill-column-center-text t)
  (text-scale-mode 0)
  (visual-fill-column-mode 1))

(defmacro add-import-keymap-for-language (language add-import-function)
  "Bind `ADD-IMPORT-FUNCTION` to `LANGUAGE`-map."
  `(let ((language-hook (intern (s-append "-hook" (symbol-name ',language)))))
     (add-hook
      language-hook
      (lambda ()
        (define-key
            xah-fly-command-map
            (kbd "SPC i")
          ',add-import-function)))))

(defvar my-autoformat-functions nil
  "Current used autoformat functions.")

(defcustom my-autoformat-all-functions '(sentence-capitalization
                                         ordinals
                                         transposed-caps)
  "All my autoformat functions.")

(defun my-use-autoformat-function-p (f)
  "Return t, when must use F as autoformat function."
  (-contains-p my-autoformat-functions f))

(defmacro my-use-autoformat-in-mode (mode &rest autoformat-functions)
  "Add hook to MODE, which enable AUTOFORMAT-FUNCTIONS."
  (let* ((hook (intern (s-append "-hook" (symbol-name (eval mode)))))
         (funcs (->>
                 autoformat-functions
                 (--map (symbol-name it))
                 (--map (intern (s-prepend "autoformat-" it))))))
    `(add-hook ',hook
               (lambda ()
                 (setq-local my-autoformat-functions ',funcs)))))

(defmacro my-use-all-autoformat-in-mode (mode)
  "Use my all autoformat functions in MODE."
  `(my-use-autoformat-in-mode ,mode ,@my-autoformat-all-functions))

(defun autoformat-sentence-capitalization ()
  "Auto-capitalize first words of a sentence.
Either at the beginning of a line, or after a sentence end."
  (interactive)
  (when (and (my-use-autoformat-function-p 'autoformat-sentence-capitalization)
             (not (org-in-src-block-p))
             (or (save-excursion (backward-char) (bolp))
                 (looking-back "\*+\\W*.")
                 (looking-back (concat (sentence-end) "[a-z]"))))
    (undo-boundary)
    (capitalize-word -1)))

(defun autoformat-transposed-caps ()
  "If you write hTe, fixes it to The."
  (interactive)
  (when (and (my-use-autoformat-function-p 'autoformat-transposed-caps)
             (not (org-in-src-block-p))
             (let ((case-fold-search nil))
               (looking-back "\\<\\(?1:[a-z]\\)\\(?2:[A-Z]\\)[a-z]+"
                             (line-beginning-position))))
    (undo-boundary)
    (save-excursion
      (replace-match (upcase (match-string 1)) nil nil nil 1)
      (replace-match (downcase (match-string 2)) nil nil nil 2))))

(defun autoformat-ordinals ()
  "Expand ordinal words to superscripted versions in `org-mode'.
1st to 1^{st}.
2nd to 2^{nd}
3rd to 3^{rd}
4th to 4^{th}"
  (interactive)
  (when (and (my-use-autoformat-function-p 'autoformat-ordinals)
             (not (org-in-src-block-p))
             (looking-back
              "\\(?3:\\<\\(?1:[0-9]+\\)\\(?2:st\\|nd\\|rd\\|th\\)\\>\\)\\(?:[[:punct:]]\\|[[:space:]]\\)"
              (line-beginning-position)))
    (undo-boundary)
    (save-excursion
      (replace-match "\\1^{\\2}" nil nil nil 3))))

(defun my-autoformat ()
  "Call all autoformat functions."
  (interactive)
  (--each my-autoformat-functions
    (funcall it)))

(define-minor-mode my-autoformat-mode
    "Toggle `my-autoformat-mode'.  Converts 1st to 1^{st} as you type."
  :init-value nil
  (if my-autoformat-mode
      (add-hook 'post-self-insert-hook #'my-autoformat)
    (remove-hook 'post-self-insert-hook #'my-autoformat)))

(my-autoformat-mode t)

(use-package latex
    :ensure auctex
    :major-mode-map (LaTeX-mode)
    :hook ((LaTeX-mode . prettify-symbols-mode))
    :bind ((:map my-latex-local-map)
           ("="     . my-calc-simplify-region-copy)
           ("f"     . my-calc-simplify-region-change))
    :config (require 'calc-lang)
    (defun my-calc-simplify (expr)
      "Simplify EXPR via `calc' and return this."
      (calc-latex-language t)
      (calc-alg-entry expr)
      (with-temp-buffer
        (calc-copy-to-buffer 1)
        (delete-char -1)
        (buffer-string)))

    (defun my-calc-simplify-region-copy (beg end)
      "Take from BEG to END, simplify this via `calc' and copy as kill."
      (interactive "r")
      (let ((expr (my-calc-simplify (buffer-substring beg end))))
        (with-temp-buffer
          (insert expr)
          (copy-region-as-kill (point-min) (point-max)))
        (message "coppied: %s" (current-kill 0))))

    (defun my-calc-simplify-region-change (beg end)
      "Get from BEG to END change this via `calc' and yank instead of region."
      (interactive "r")
      (let ((expr (buffer-substring beg end)))
        (goto-char beg)
        (delete-region beg end)
        (insert (my-calc-simplify expr)))))

(use-package preview
    :after latex
    :bind ((:map my-latex-local-map)
           ("p" . org-latex-preview))
    :config
    (plist-put org-format-latex-options :scale 1.9))

(use-package cdlatex
    :ensure t
    :hook (LaTeX-mode . turn-on-cdlatex)
    :bind (:map cdlatex-mode-map ("<tab>" . cdlatex-tab)))

;; fields
(use-package cdlatex
    :hook ((cdlatex-tab . yas-expand)
           (cdlatex-tab . cdlatex-in-yas-field))
    :config (use-package yasnippet
                :bind (:map yas-keymap
                            ("<tab>" . yas-next-field-or-cdlatex)
                            ("TAB" . yas-next-field-or-cdlatex))
                :config (defun cdlatex-in-yas-field
                            ()
                          ;; Check if we're at the end of the Yas field
                          (when-let* ((_ (overlayp yas--active-field-overlay))
                                      (end (overlay-end yas--active-field-overlay)))
                            (if (>= (point) end)
                                (let ((s (thing-at-point 'sexp)))
                                  (unless (and s
                                               (assoc
                                                (substring-no-properties s)
                                                cdlatex-command-alist-comb))
                                    (yas-next-field-or-maybe-expand)
                                    t))
                              (let (cdlatex-tab-hook minp)
                                (setq minp
                                      (min
                                       (save-excursion
                                         (cdlatex-tab)
                                         (point))
                                       (overlay-end
                                        yas--active-field-overlay)))
                                (goto-char minp)
                                t))))

                (defun yas-next-field-or-cdlatex nil
                  (interactive)
                  "Jump to the next Yas field correctly with cdlatex active."
                  (if (or
                       (bound-and-true-p cdlatex-mode)
                       (bound-and-true-p org-cdlatex-mode))
                      (cdlatex-tab)
                    (yas-next-field-or-maybe-expand)))))

;; Array/tabular input with org-tables and cdlatex
(use-package org-table
    :after cdlatex
    :bind (:map orgtbl-mode-map
                ("<tab>" . lazytab-org-table-next-field-maybe)
                ("TAB" . lazytab-org-table-next-field-maybe))
    :init (add-hook 'cdlatex-tab-hook 'lazytab-cdlatex-or-orgtbl-next-field 90)
    ;; Tabular environments using cdlatex
    (add-to-list 'cdlatex-command-alist
                 '("smat" "Insert smallmatrix env"
                   "\\left( \\begin{smallmatrix} ? \\end{smallmatrix} \\right)"
                   lazytab-position-cursor-and-edit
                   nil nil t))
    (add-to-list 'cdlatex-command-alist
                 '("bmat" "Insert bmatrix env"
                   "\\begin{bmatrix} ? \\end{bmatrix}"
                   lazytab-position-cursor-and-edit
                   nil nil t))
    (add-to-list 'cdlatex-command-alist
                 '("pmat" "Insert pmatrix env"
                   "\\begin{pmatrix} ? \\end{pmatrix}"
                   lazytab-position-cursor-and-edit
                   nil nil t))
    (add-to-list 'cdlatex-command-alist
                 '("tbl" "Insert table"
                   "\\begin{table}\n\\centering ? \\caption{}\n\\end{table}\n"
                   lazytab-position-cursor-and-edit
                   nil t nil))
    :config ;; Tab handling in org tables
    (defun lazytab-position-cursor-and-edit ()
      ;; (if (search-backward "\?" (- (point) 100) t)
      ;;     (delete-char 1))
      (cdlatex-position-cursor)
      (lazytab-orgtbl-edit))

    (defun lazytab-orgtbl-edit ()
      (advice-add 'orgtbl-ctrl-c-ctrl-c :after #'lazytab-orgtbl-replace)
      (orgtbl-mode 1)
      (open-line 1)
      (insert "\n|"))

    (defun lazytab-orgtbl-replace (_)
      (interactive "P")
      (unless (org-at-table-p) (user-error "Not at a table"))
      (let* ((table (org-table-to-lisp))
             params
             (replacement-table
              (if (texmathp)
                  (lazytab-orgtbl-to-amsmath table params)
                (orgtbl-to-latex table params))))
        (kill-region (org-table-begin) (org-table-end))
        (open-line 1)
        (push-mark)
        (insert replacement-table)
        (align-regexp
         (region-beginning)
         (region-end)
         "\\([:space:]*\\)& ")
        (orgtbl-mode -1)
        (advice-remove 'orgtbl-ctrl-c-ctrl-c #'lazytab-orgtbl-replace)))

    (defun lazytab-orgtbl-to-amsmath (table params)
      (orgtbl-to-generic
       table
       (org-combine-plists
        '(:splice t
          :lstart ""
          :lend " \\\\"
          :sep " & "
          :hline nil
          :llend "")
        params)))

    (defun lazytab-cdlatex-or-orgtbl-next-field ()
      (when (and
             (bound-and-true-p orgtbl-mode)
             (org-table-p)
             (looking-at "[[:space:]]*\\(?:|\\|$\\)")
             (let ((s (thing-at-point 'sexp)))
               (not (and s (assoc s cdlatex-command-alist-comb)))))
        (call-interactively #'org-table-next-field)
        t))

    (defun lazytab-org-table-next-field-maybe ()
      (interactive)
      (if (bound-and-true-p cdlatex-mode)
          (cdlatex-tab)
        (org-table-next-field))))

(setq latex-documentclasses
      '("article" "reoport" "book" "proc" "minimal" "slides" "memoir" "letter" "beamer"))

(dolist (mode (list 'TeX-mode-hook
                    'tex-mode-hook
                    'latex-mode-hook
                    'LaTeX-mode-hook))
  (add-hook mode (lambda () (call-interactively 'visual-fill))))

(use-package latex
    :major-mode-map (TeX-mode LaTeX-mode tex-mode latex-mode)
    :bind ((:map LaTeX-mode-map)
           (";" . cdlatex-dollar)))

(use-package company-math
    :ensure t
    :init
    (defun my-company-math-setup ()
      "Setup for `company-math'."
      (add-to-list 'company-backends 'company-math-symbols-latex)
      (add-to-list 'company-backends 'company-latex-commands))
    (add-hook 'LaTeX-mode 'my-company-math-setup))

(use-package company-auctex
    :ensure t
    :config
    (company-auctex-init))

(use-package laas
    :ensure t
    :hook (LaTeX-mode . laas-mode)
    :config (aas-set-snippets 'laas-mode
              :cond #'texmathp
              ;; Some Units
              "As" "\\mathrm{А}"
              "Vs"  "\\mathrm{В}"
              "Oms"  "\\mathrm{Ом}"
              "cls" "^\\circ C"

              ;; Some Physics Sheet
              "eqv" "\\mathrm{экв}"))

(use-package org
    :major-mode-map (org-mode)
    :bind
    ((:map xah-fly-command-map)
     ("1"   . 'my-er-expand-region-or-org-todo)
     (:map my-org-local-map)
     ("SPC" . 'org-toggle-checkbox)
     ("'"   . 'org-edit-special)
     ("l"   . 'org-insert-link)
     ("t"   . 'org-babel-tangle)
     ("p"   . 'org-latex-preview)
     ("1"   . 'org-todo)
     ("s"   . 'org-schedule)
     ("RET" . 'org-open-at-point)))

(defvar my-last-command-is-org-todo nil
  "To t, when last `my-er-expand-region-or-org-todo' done `org-todo'.")

(defun my-er-expand-region-or-org-todo ()
  "If need do `org-todo', otherwise do `er/expand-region'."
  (interactive)
  (let ((is-org-todo (or
                       (eq last-command #'org-todo)
                       (and
                        (eq last-command this-command)
                        my-last-command-is-org-todo))))
    (setq my-last-command-is-org-todo is-org-todo)
    (if is-org-todo
        (call-interactively #'org-todo)
      (call-interactively #'er/expand-region))))

(add-hook 'org-mode-hook (lambda () (call-interactively 'visual-fill)))

(my-use-all-autoformat-in-mode 'org-mode)

(use-package wikinforg
    :ensure t)

(use-package org-download
    :ensure t
    :hook
    (dired-mode-hook . org-download-enable))

(use-package helm-org
    :ensure t
    :bind (:map org-mode-map
                ([remap helm-imenu]
                 . helm-org-in-buffer-headings)))

(use-package ox-clip
    :ensure t
    :bind
    ((:map my-org-local-map)
     ("c" . ox-clip-formatted-copy)))

(defun turn-on-org-cdlatex-mode ()
  "Turn on `org-cdlatex-mode'."
  (interactive)
  (org-cdlatex-mode t))

(add-hook 'org-mode-hook #'turn-on-org-cdlatex-mode)

(use-package package-lint
    :ensure t)

(use-package flycheck-package
    :ensure t
    :init
    (flycheck-package-setup))

(use-package emr
    :ensure t
    :bind (:map xah-fly-command-map
                ("SPC /" . emr-show-refactor-menu)))

(use-package cask-mode
    :ensure t
    )

(use-package elfmt
    :config
  (elfmt-global-mode 1))

(use-package suggest
    :ensure t
    )

(defun my-edit-elisp-docstring ()
  "Edit `elisp' docstring via `string-edit' and `elisp-docstring-mode'."
  (interactive)
  (string-edit-at-point)
  (elisp-docstring-mode))

(use-package elisp-docstring-mode
    :ensure t
    :bind (:map emacs-lisp-mode-map
                ([remap string-edit-at-point] . my-edit-elisp-docstring)))

(setq lisp-body-indent 2)

;;

(defun my-goto-defclass-beg ()
  "Goto backward defclass."
  (search-backward-regexp "(\\W*defclass" nil t)
  (skip-chars-forward "("))

(defun my-goto-fields-defclass-defnition ()
  "Go to fields of `defclass' defnition."
  (interactive)
  (my-goto-defclass-beg)
  (forward-sexp 4)
  (forward-char -1)
  (-when-let (sexp (sp-get-enclosing-sexp))
    (sp-get sexp (goto-char :end-in))))

(defun my-elisp-in-defclass-p (&optional pt)
  "Move to PT and return name of function/macros in which stay this sexp."
  (setq pt (or pt (point)))
  (save-excursion
    (goto-char pt)
    (when (my-goto-defclass-beg)
      (-when-let (sexp (sp-get-enclosing-sexp))
        (sp-get sexp (< :beg pt :end))))))

(defun my-elisp-defclass-name ()
  "Return name of `defclass' defnition."
  (interactive)
  (save-excursion
    (my-goto-defclass-beg)
    (forward-sexp 1)
    (forward-char 1)
    (sexp-at-point)))

(defun my-elisp-new-field-of-class ()
  "Insert new field of Lisp class.
Only when in class defnition."
  (interactive)
  (when (my-elisp-in-defclass-p)
    (my-goto-fields-defclass-defnition)
    (unless (my-current-line-is-empty-p)
      (newline-and-indent))
    (yas-expand-snippet
     (format "(${1:name} :initarg :$1 :accessor %s-$1)"
             (my-elisp-defclass-name)))))

(define-key emacs-lisp-mode-map (kbd "M-RET") 'my-elisp-new-field-of-class)

(use-package markdown-mode
    :ensure t
    :major-mode-map t
    :bind (:map
           my-markdown-mode-local-map
           ("<SPC>"     . markdown-toggle-gfm-checkbox)
           ("b"     . markdown-insert-bold)
           ("i"     . markdown-insert-italic)
           ("l"     . markdown-insert-link)
           ("p"     . markdown-live-preview-mode)
           ("'"     . markdown-edit-code-block)
           ("<RET>" . markdown-follow-thing-at-point))
    :hook (markdown-mode . visual-fill)
    :init
    (setq markdown-imenu-generic-expression
          '(("title""^\\(.*\\)[\n]=+$" 1)
            ("h2-"  "^\\(.*\\)[\n]-+$" 1)
            ("h1"   "^# \\(.*\\)$" 1)
            ("h2"   "^## \\(.*\\)$" 1)
            ("h3"   "^### \\(.*\\)$" 1)
            ("h4"   "^#### \\(.*\\)$" 1)
            ("h5"   "^##### \\(.*\\)$" 1)
            ("h6"   "^###### \\(.*\\)$" 1)
            ("fn"   "^\\[\\^\\(.*\\)\\]" 1)))

    (add-hook 'markdown-mode-hook
              (lambda ()
                (setq-local imenu-generic-expression
                            markdown-imenu-generic-expression))))

(use-package markdown-toc
    :ensure t
    :bind (:map
           my-markdown-mode-local-map
           ("t" . 'markdown-toc-generate-or-refresh-toc)))

(setq py/imports-regexp "import\\|from")

(setq python-shell-interpreter "python")

(defun py-nav-to-imports ()
  "Navigate to imports in Python mode."
  (interactive)
  (push-mark)
  (let ((old-pos (point)))
    (goto-char (point-min))
    (search-forward-regexp py/imports-regexp old-pos old-pos))
  )

(add-nav-to-imports-for-language
 python-mode
 py-nav-to-imports)

(setq flycheck-python-flake8-command "python -m flake8")
(setq flycheck-python-mypy-executable "python -m mypy")
(setq flycheck-python-pylint-executable "python -m pylint")

(use-package pydoc
    :ensure t)

(defun enable-dabbrev-company-backend ()
  "Add `company-dabbrev' backend to `company-backends' for local major mode."
  (interactive)
  (setq-local company-backends (cons 'company-dabbrev company-backends))
  )

(add-hook 'python-mode-hook 'enable-dabbrev-company-backend)

(use-package racer
    :ensure t
    :hook ((rust-mode  . racer-mode)
           (racer-mode . eldoc-mode)))

(use-package flycheck-rust
    :ensure t
    :config
    (flycheck-rust-setup))

(use-package go-mode
    :ensure t)

(use-package go-eldoc
    :ensure t
    :hook (go-mode-hook . 'go-eldoc-setup))

(add-import-keymap-for-language go-mode
                                go-import-add)

(use-package pdf-tools
    :ensure t
    )

(use-package haskell-mode
    :ensure t
    :hook (haskell-mode . haskell-indent-mode))

(add-import-keymap-for-language
 haskell-mode
 haskell-add-import)

(add-nav-to-imports-for-language
 haskell-mode
 haskell-navigate-imports)

(use-package company-ghci
    :ensure t
    :init
    (push 'company-ghci company-backends)
    (add-hook 'haskell-mode-hook 'company-mode)
    (add-hook 'haskell-interactive-mode-hook 'company-mode))

(setq js/imports-regexp "import")

(setq js/function-or-class-regexp "function \\|class ")

(use-package js-comint
    :ensure t)

(if (user-os-windows-p)
    (setq js-comint-program-command "C:/Program Files/nodejs/node.exe"))

(use-package web-mode
    :ensure t)

(defun my-enable-flycheck ()
  (flycheck-mode 1))

(use-package js2-mode
    :ensure t
    :mode "\\.js$"
    :custom
    (js2-allow-rhino-new-expr-initializer nil)
    (js2-auto-indent-p nil)
    (js2-enter-indents-newline nil)
    (js2-global-externs '("module"
                          "require"
                          "buster"
                          "sinon"
                          "assert"
                          "refute"
                          "setTimeout"
                          "clearTimeout"
                          "setInterval"
                          "clearInterval"
                          "location"
                          "__dirname"
                          "console"
                          "JSON"))
    (js2-idle-timer-delay 0.1)
    (js2-indent-on-enter-key nil)
    (js2-mirror-mode nil)
    (js2-strict-inconsistent-return-warning nil)
    (js2-auto-indent-p t)
    (js2-include-rhino-externs nil)
    (js2-include-gears-externs nil)
    (js2-concat-multiline-strings 'eol)
    (js2-rebind-eol-bol-keys nil)
    (js2-show-parse-errors nil)
    (js2-strict-missing-semi-warning nil)
    (js2-strict-trailing-comma-warning t)
    :hook (js2-mode . my-enable-flycheck))

(defun js/nav-to-imports ()
  "Navigate to imports in JS mode."
  (interactive)
  (push-mark)
  (let ((old-pos (point)))
    (goto-char (point-min))
    (search-forward-regexp js/imports-regexp old-pos old-pos))
  )

(add-nav-to-imports-for-language
 js2-mode
 js/nav-to-imports)

(use-package json-mode
    :major-mode-map t)

(use-package json-snatcher
    :ensure t
    :bind
    (:map
     my-json-mode-local-map
     ("c" . jsons-print-path)))

(defcustom html-modes '(web-mode html-mode mhtml-mode)
  "List of `html` major modes.")

(use-package web-mode
    :ensure t
    :hook (web-mode . yas-minor-mode-off)
    :custom
    (web-mode-script-padding 1)
    (web-mode-block-padding 0))

(use-package auto-rename-tag
    :ensure t
    :config
    :init
    (--each html-modes
      (add-hook (intern (s-append "-hook" (symbol-name it)))
                (lambda () (auto-rename-tag-mode 38)))))

(use-package emmet-mode
    :ensure t
    :custom (emmet-move-cursor-between-quotes t)
    :hook
    (web-mode . emmet-mode)
    (mhtml-mode . emmet-mode)
    (css-mode . emmet-mode)
    (html-mode . emmet-mode))

(use-package helm-emmet
    :ensure t
    :init
    (defun fast-exec-helm-emmet-keys ()
      "Keymaps for `helm-emmet'."
      (fast-exec/some-commands
       ("View Emmet Cheat" 'helm-emmet)))
    (fast-exec/register-keymap-func 'fast-exec-helm-emmet-keys)
    (fast-exec/reload-functions-chain))

(use-package tagedit
    :ensure t
    :init
    (--each html-modes
      (let ((map-symbol (intern (s-append "-map" (symbol-name it))))
            map)
        (when (boundp map-symbol)
          (setq map (eval map-symbol))
          (define-key
              map
              [remap sp-kill-hybrid-sexp]
            'tagedit-kill)
          (define-key
              map
              [remap sp-join-sexp]
            'tagedit-join-tags)
          (define-key
              map
              [remap sp-raise-sexp]
            'tagedit-raise-tag)
          (define-key
              map
              [remap sp-splice-sexp]
            'tagedit-splice-tag)
          (define-key
              map
              [remap sp-change-enclosing]
            'tagedit-kill-attribute)))))

(use-package company-web
    :ensure t
    :init
    (add-hook 'web-mode-hook
              (lambda ()
                (set (make-local-variable 'company-backends)
                     '(company-web-html))
                (company-mode t))))

(use-package css-mode)

(use-package css-eldoc
    :ensure t
    :init
    (dolist (hook (list 'web-mode-hook 'css-mode-hook))
      (add-hook hook 'css-eldoc-enable)))

(show-paren-mode 2)
(setq make-backup-files         nil)
(setq auto-save-list-file-name  nil)
(defalias 'yes-or-no-p 'y-or-n-p)
(toggle-truncate-lines 38)

(use-package git-gutter
    :ensure t
    :hook
    (prog-mode . git-gutter-mode))

(use-package which-key
    :ensure t
    :config
    (which-key-setup-side-window-bottom)
    (which-key-mode))

(use-package helpful
    :ensure t
    :init
    (global-set-key (kbd "C-h f") #'helpful-callable)
    (global-set-key (kbd "C-h v") #'helpful-variable)
    (global-set-key (kbd "C-h k") #'helpful-key)
    (global-set-key (kbd "C-c C-d") #'helpful-at-point)
    (global-set-key (kbd "C-h F") #'helpful-function)
    (global-set-key (kbd "C-h C") #'helpful-command))

(use-package helm
    :ensure t
    :custom
    (helm-M-x-fuzzy-match t)
    (helm-autoresize-min-height 20)
    :init (helm-autoresize-mode 1) (helm-mode 1)
    :bind (:map
           xah-fly-command-map
           ("SPC SPC f" . helm-find-files)))

(use-package command-log-mode
    :ensure t)

(add-hook 'prog-mode-hook 'whitespace-mode)

(add-hook 'change-major-mode-hook 'visual-line-mode)

(use-package aggressive-fill-paragraph
    :ensure t
    :config
    (afp-setup-recommended-hooks))

(use-package scratch
    :ensure t
    :bind (("C-t" . scratch)))

(use-package syntax-subword
    :ensure t
    :custom
    (syntax-subword-skip-spaces t)
    :config
    (global-syntax-subword-mode))

(defun my-pandoc-tex-to-documents-dir ()
  "Move all .docx files in working dir to directroy documents."
  (f-mkdir "documents")
  (-->
   (file-expand-wildcards "*.tex")
   (-map 'f-base it)
   (--each
       it
     (shell-command
      (s-lex-format
       "pandoc -t docx -f latex -o documents/${it}.docx ${it}.tex")))))

(defun fast-exec-pandoc-keys ()
  "Get some useful keymaps of  `fast-exec' for pandoc"
  (fast-exec/some-commands
   ("Convert Tex Files and Move to Documents Dir"
    'my-pandoc-tex-to-documents-dir)))

(fast-exec/register-keymap-func 'fast-exec-pandoc-keys)
(fast-exec/reload-functions-chain)

(setq default-input-method "russian-computer")

(use-package cowsay
    :ensure t
    :custom
    (cowsay-directories '("~/.emacs.d/cows"))
    :config
    (defun fast-exec-define-cowsay-keymaps ()
      "Some useful keymaps for `cowsay'/`fast-exec'."
      (fast-exec/some-commands
       ("Cow Say String..."  'cowsay-string)
       ("Cow Say Region..."  'cowsay-region)
       ("Cow Say and Insert" 'cowsay-replace-region)
       ("Cow Say Load Cows"  'cowsay-load-cows)))
    (fast-exec/register-keymap-func 'fast-exec-define-cowsay-keymaps)
    (fast-exec/reload-functions-chain))

(add-to-list 'load-path "~/projects/super-save/")

(use-package super-save
    :config
  (setq super-save-exclude '("Emacs.org"))
  (setq auto-save-default nil)
  (super-save-mode 38))

(use-package devdocs
    :ensure t
    :hook (python-mode . (lambda ()
                           (setq-local devdocs-current-docs
                                       '("python~3.9"))))
    )

(use-package pomidor
    :ensure t
    :bind (("<f12>" . pomidor))
    :custom
    (pomidor-sound-tick . nil)
    (pomidor-sound-tack . nil)
    :hook
    (pomidor-mode . (lambda ()
                      (display-line-numbers-mode -1)
                      (setq left-fringe-width 0 right-fringe-width 0)
                      (setq left-margin-width 2 right-margin-width 0)
                      (set-window-buffer nil (current-buffer))))
    :init
    (pomidor))

(use-package pacmacs
    :ensure t
    :init
    (defun fast-exec-define-pacmacs-keys ()
      "Bind `fast-exec' and `pacmacs'."
      (fast-exec/some-commands
       ("Play to Pacmacs" 'pacmacs-start))
      )
    (fast-exec/register-keymap-func 'fast-exec-define-pacmacs-keys)
    (fast-exec/reload-functions-chain))

(use-package helm-wikipedia
    :ensure t)

(use-package helm-spotify-plus
    :ensure t)

(use-package helm-github-stars
    :ensure t
    :custom
    (helm-github-stars-username "semeninrussia")
    :init
    (defun fast-exec-define-helm-github-stars ()
      "Bind `helm-github-stars' and `fast-exec'."
      (fast-exec/some-commands
       ("View Github Stars" 'helm-github-stars-fetch)))
    (fast-exec/register-keymap-func 'fast-exec-define-helm-github-stars)
    (fast-exec/reload-functions-chain))

;; (use-package helm-gitignore
;;     :ensure t
;;     :init
;;     (defun fast-exec-define-helm-gitignore-keys ()
;;         "Bind `fast-exec' and `helm-gitignore'."
;;         (fast-exec/some-commands
;;          ("Generate Gitignore" 'helm-gitignore)))
;;     (fast-exec/register-keymap-func 'fast-exec-define-helm-gitignore-keys)
;;     (fast-exec/reload-functions-chain)))

(use-package helm-google
    :ensure t
    :init
    (defun fast-exec-helm-google-define-keys ()
      "Keymaps for `helm-google' and `fast-exec'."
      (fast-exec/some-commands
       ("Search in Google" 'helm-google)))
    (fast-exec/register-keymap-func 'fast-exec-helm-google-define-keys)
    (fast-exec/reload-functions-chain))

(use-package helm-kinopoisk
    :load-path "~/projects/emacs-kinopoisk")

(defun my-new-fake-pptx-file ()
  "Make this buffer, fake presentation with format (.pptx)."
  (interactive)
  (->> "~/broken.pptx" (f-read) (insert))
  (text-mode))

(defun fast-exec-fake-pptx-keys ()
  "Get some useful keymaps of  `fast-exec' for fake-pptx."
  (fast-exec/some-commands ("New Fake PPTX File" 'my-new-fake-pptx-file)))

(fast-exec/register-keymap-func 'fast-exec-fake-pptx-keys)
(fast-exec/reload-functions-chain)

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode   -1)

(add-to-list 'default-frame-alist '(fullscreen . maximized))
(add-hook 'emacs-startup-hook 'toggle-frame-fullscreen)

(use-package gruber-darker-theme
    :ensure t)

(use-package doom-themes
    :ensure t)

(add-to-list 'custom-theme-load-path
             "~/.emacs.d/themes")

(load-theme 'doom-monokai-classic t)

(setq dont-display-lines-modes
      '(org-mode
        term-mode
        shell-mode
        treemacs-mode
        eshell-mode
        helm-mode))

(defun display-or-not-display-numbers-of-lines ()
  "Display numbers of lines OR don't display numbers of lines.
If current `major-mode` need to display numbers of lines, then display
numbers of lines, otherwise don't display."
  (interactive)
  (if (-contains? dont-display-lines-modes major-mode)
      (display-line-numbers-mode 0)
    (display-line-numbers-mode 38))
  )

(add-hook 'prog-mode-hook 'display-or-not-display-numbers-of-lines)

(use-package doom-modeline :ensure t)

(doom-modeline-def-segment drag
  (when (my-last-command-is-dragged-stuff-p)
    (propertize
     " DRG "
     'face (if (doom-modeline--active)
               'doom-modeline-panel
             'mode-line-inactive))))

(doom-modeline-def-segment my-matches
  "Display `macro-recoring', `multiple-cursors' and `buffer-size'."
  (let ((meta (concat (doom-modeline--macro-recording)
                      (doom-modeline--multiple-cursors))))
    (or (and (not (equal meta "")) meta)
        (doom-modeline--buffer-size))))

(defcustom my-modeline-time-segment-format-string " [%H-%M]"
  "By this format string will draw time in `doom-modeline'.
See `format-time-string' for see what format string"
  :type 'string)

(doom-modeline-def-segment time
    (let* ((hour (string-to-number (format-time-string "%H"))))
      (propertize
       (format-time-string my-modeline-time-segment-format-string)
       'face
       (if (< 4 hour 19)
           'hi-red-b
         'outline-1))))

(doom-modeline-def-segment pomidor
    ()
  "Return header."
  (when (featurep 'pomidor)
    (let* ((state (pomidor--current-state))
           (break (pomidor--break-duration state))
           (overwork (pomidor--overwork-duration state))
           (work (pomidor--work-duration state))
           (face (cond
                   (break 'pomidor-break-face)
                   (overwork 'pomidor-overwork-face)
                   (work 'pomidor-work-face)))
           (pomidor-time-format " Pom %-Mm")
           (time (-first 'identity (list break overwork work))))
      (propertize (pomidor--format-time time) 'face face))))

(defvar durand-buffer-name-max 20
  "The maximal length of the buffer name in modeline.")

(doom-modeline-def-segment buffer-info-durand
    ()
  (declare (pure t) (side-effect-free t))
  (let* ((buffer-info
          (format-mode-line
           (s-truncate
            durand-buffer-name-max
            (doom-modeline-segment--buffer-info)))))
    (concat
     (s-truncate
      durand-buffer-name-max
      buffer-info))))

(defvar my-modeline-ignored-modes '(company-mode))

(use-package doom-modeline
    :ensure t
    :defer 0.1
    :init
    (size-indication-mode t)
    :custom
    (doom-modeline-buffer-file-name-style 'buffer-name)
    (doom-modeline-icon nil)
    (xah-fly-insert-state-p nil)
    :config
    (display-time-mode t)
    (doom-modeline-def-modeline 'main
        '(bar
          my-matches
          drag
          buffer-info-durand
          time
          pomidor
          word-count
          selection-info)
      '(objed-state
        persp-name
        grip
        irc
        gnus
        github
        debug
        repl
        input-method
        indent-info
        buffer-encoding
        major-mode
        process
        vcs
        checker))
    (doom-modeline-set-modeline 'main t))

(set-face-attribute 'default nil :font "Consolas" :height 250)
(set-frame-font "Consolas" nil t)

(global-hl-line-mode 1)

(use-package page-break-lines
    :ensure t
    :init
    (global-page-break-lines-mode 38))

(use-package projectile
    :custom
  (projectile-project-search-path '("~/projects/"))
  (projectile-completion-system 'helm)
  :init (projectile-mode 1)
  :bind
  (("S-<f5>" . projectile-test-project)
   ("<f5>"   . projectile-run-project)))

(projectile-mode 1)

(use-package helm-projectile
    :ensure t
    :bind (:map xah-fly-command-map
                ("SPC j" . 'helm-projectile-find-file)))

(use-package regex-tool
    :ensure t
    :init
    (add-hook 'regex-tool-mode-hook (lambda () (toggle-frame-maximized))))

(use-package magit :ensure t)

(use-package blamer
    :ensure t
    :defer 20
    :custom
    (blamer-idle-time 0.3)
    (blamer-min-offset 70)
    :custom-face
    (blamer-face ((t :foreground "#7a88cf"
                     :background nil
                     :height 140
                     :italic t)))
    )

(use-package git-undo
    :init
  (defun fast-exec-define-git-undo-keymaps ()
    "Bind `git-undo' and `fast-exec'."
    (fast-exec/some-commands
     ("Undo via Git" 'git-undo)))
  (fast-exec/register-keymap-func 'fast-exec-define-git-undo-keymaps)
  (fast-exec/reload-functions-chain))

(use-package git-modes
    :ensure t)

(use-package helm-gitignore
    :init
  (defun fast-exec-helm-gitignore-keys ()
    "Bind of `helm-gitignore' and `fast-exec'."
    (fast-exec/some-commands
     ("Generate Gitignore" 'helm-gitignore)))
  (fast-exec/register-keymap-func 'fast-exec-helm-gitignore-keys)
  (fast-exec/reload-functions-chain))

(add-hook 'dired-mode-hook (lambda () (dired-hide-details-mode 1)))

(use-package run-command
    :ensure t
    :custom
    (run-command-completion-method 'helm)
    :bind (:map xah-fly-command-map
                ("SPC , c" . run-command)))

(use-package run-command-recipes
    :ensure t
    :config
    (run-command-recipes-use-all))

(use-package skeletor
    :ensure t
    :custom
    (skeletor-init-with-git nil)
    (skeletor-project-directory "~/projects")
    (skeletor-completing-read-function completing-read-function))

(use-package hl-todo
    :ensure t
    :config (global-hl-todo-mode))

(defun run-command-recipe-snitch ()
  "Recipes of `run-command' for snitch."
  (when (f-directory-p (f-join (projectile-acquire-root)
                               ".git"))
    (list
     (list :command-name "sntich-list"
           :display "See List of TODOs from via Snitch"
           :command-line "snitch list")
     (list :command-name "sntich-report"
           :display "Report to VC TODOs of Project via Snitch"
           :command-line "snitch list"))))

(add-to-list 'run-command-recipes 'run-command-recipe-snitch)

(setq org-agenda-files '("~/agenda.org"))

(defun my-open-main-agenda-file ()
  "Open \"~/agenda.org\"."
  (interactive)
  (find-file "~/agenda.org"))

(global-set-key (kbd "<f9>") #'org-agenda)
(global-set-key (kbd "S-<f9>") #'org-todo-list)
(define-key xah-fly-command-map (kbd "SPC <f9>") #'org-todo-list)

(global-set-key (kbd "<f5>") #'my-open-main-agenda-file)

(defgroup my-notes nil
  "My own simple system of notes."
  :group 'tools)

(defcustom my-notes-extension "org"
  "Extension of notes, defaults to org."
  :type 'string
  :group 'my-notes)

(defcustom my-notes-categories-list-file-path
  "~/notes/categories.txt"
  "Path to file which contains list of categories."
  :type 'string
  :group 'my-notes)

(defun my-notes-parse-categories ()
  "Find notes' categories in special path.
See `my-notes-categories-list-file-path'"
  (->>
   my-notes-categories-list-file-path
   (f-read)
   (s-trim)
   (s-lines)
   (-remove #'s-blank-p)))

(defcustom my-notes-categories
  (my-notes-parse-categories)
  "List of tags of notes."
  :group 'my-notes
  :type '(repeat symbol))

(defcustom my-notes-templates-dir
  "~/notes/templates"
  "Directory in which will save templates of my notes."
  :type 'string
  :group 'my-notes)

(defcustom my-notes-default-template
  "~/notes/default.org"
  "Default template for notes."
  :type 'string
  :group 'my-notes)

(defcustom my-notes-directory
  "~/notes"
  "Main directory of my notes."
  :type 'string
  :group 'my-notes)

(defun my-notes-read-note ()
  "Read from user note."
  (my-notes-note
   :title (read-string "Please, enter title of note: ")
   :category (my-notes-read-category)))

(defun my-notes-read-category (&optional prompt)
  "Read category from user with PROMPT."
  (setq prompt (or prompt "Choose category, please: "))
  (completing-read prompt my-notes-categories))

(defun my-notes-find ()
  "Find one of notes."
  (interactive)
  (let* ((category (my-notes-read-category))
         (title
          (->>
           category
           (my-notes-titles-of-category)
           (completing-read "Choose one of notes, please: ")
           (my-normalize-string))))
    (my-notes-note :title title :category category)))

(defun my-notes-visit (note)
  "Visit NOTE's file."
  (interactive (list (my-notes-find)))
  (->> note (my-notes-note-path) (find-file)))

(defun my-notes-change-category-of-note (note new-category)
  "Change category of NOTE to NEW-CATEGORY."
  (interactive
   (list
    (my-notes-find)
    (my-notes-read-category "New category, please: ")))
  (let ((new-note
         (my-notes-note
          :title (my-notes-note-title note)
          :category new-category)))
    (my-notes-category-mkdir new-category)
    (f-move
     (my-notes-note-path note)
     (my-notes-note-path new-note))))

(defun my-notes-delete (note)
  "Delete NOTE."
  (interactive (list (my-notes-find)))
  (->> note (my-notes-note-path) (f-delete)))

(defun my-notes-titles-of-category (category)
  "Get list of notes' titles of CATEGORY."
  (->>
   category
   (my-notes-category-path)
   (my-files-with-extension my-notes-extension)
   (-map #'f-base)
   (-map #'my-humanize-string)))

(defun my-notes-category-path (category)
  "Get path to CATEGORY's directory."
  (->> category (my-normalize-string) (f-join my-notes-directory)))

(defun my-notes-category-mkdir (category)
  "If directory of CATEGORY not created, then create its."
  (->>
   category
   (my-notes-category-path)
   (f-mkdir)))

(defun my-notes-visit-category-template (category)
  "Create or visit template for CATEGORY."
  (interactive (list (my-notes-read-category)))
  (->> category (my-notes-category-template-path) (find-file)))

(defun my-notes-new (note &optional is-template-as-snippet)
  "Create new note from NOTE object.
If IS-TEMPLATE-AS-SNIPPET is t, then expand template of note as YAS snippet"
  (interactive (list (my-notes-read-note) t))
  (find-file (my-notes-note-path note))
  (my-notes-note-insert-template note is-template-as-snippet))

(defun my-notes-buffer-to-note (buffer note)
  "Save BUFFER as NOTE."
  (interactive
   (list
    (current-buffer)
    (my-notes-note
     :title (read-string "Name of note, please: " (buffer-name))
     :category (my-notes-read-category))))
  (with-current-buffer buffer
    (let ((text (buffer-string)))
      (kill-buffer buffer)
      (my-notes-new note)
      (insert text))))

(defclass my-notes-note ()
  ((title :initarg :title :accessor my-notes-note-title)
   (category :initarg :category :accessor my-notes-note-category))
  "My note object.")

(defun my-notes-add-note-extension (s)
  "Add `my-notes-extension' to S."
  (f-swap-ext s my-notes-extension))

(defun my-notes-note-path (note)
  "Get path to file of note NOTE."
  (->>
   note
   (my-notes-note-title)
   (my-normalize-string)
   (my-notes-add-note-extension)
   (f-join (my-notes-category-path (my-notes-note-category note)))))

(defun my-notes-note-template-path (note)
  "Get path of template for NOTE."
  (let ((template-for-category
         (->>
          note
          (my-notes-note-category)
          (my-notes-category-template-path))))
    (if (f-exists-p template-for-category)
        template-for-category
      my-notes-default-template)))

(defun my-notes-category-template-path (category)
  "Get path of CATEGORY's template."
  (->>
   category
   (my-normalize-string)
   (f-join my-notes-templates-dir)
   (my-notes-add-note-extension)))

(defun my-notes-note-insert-template (note &optional is-as-snippet)
  "Insert template for NOTE.
If IS-AS-SNIPPET is t, then expand template as YAS snippet"
  (let ((template (my-notes-note-get-template-string note)))
    (if is-as-snippet
        (yas-expand-snippet template)
      (insert template))))

(defun my-notes-note-get-template-string (note)
  "Get string of template for NOTE."
  (->>
   note
   (my-notes-note-template-path)
   (f-read)
   (s-replace "{title}" (my-notes-note-title note))
   (s-replace "{category}" (my-notes-note-category note))))

(defun my-notes-new-category (category)
  "Add CATEGORY list of categories."
  (interactive "sName of new category, please: ")
  (->>
   my-notes-categories
   (cons category)
   (my-notes-change-categories)))

(defun my-notes-rename-category (category new-name)
  "Change name of CATEGORY to NEW-NAME."
  (interactive
   (let* ((category (my-notes-read-category))
          (new-name
           (read-string "New name of this category, please: " category)))
     (list category new-name)))
  (my-notes-rename-category-directory category new-name)
  (my-notes-rename-category-template category new-name)
  (->>
   my-notes-categories
   (-replace category new-name)
   (my-notes-change-categories)))

(defun my-notes-rename-category-directory (category new-name)
  "Rename directory of CATEGORY to NEW-NAME."
  (my-try-move
   (my-notes-category-path category)
   (my-notes-category-path new-name)))

(defun my-notes-rename-category-template (category new-name)
  "Rename template of CATEGORY to template of NEW-NAME."
  (my-try-move
   (my-notes-category-template-path category)
   (my-notes-category-template-path new-name)))

(cl-defun my-notes-delete-category (category &optional (is-delete-notes t))
  "Delete CATEGORY and files of this category if IS-DELETE-NOTES is true."
  (interactive
   (list
    (my-notes-read-category)
    (y-or-n-p "Is delete notes of this category? ")))
  (when is-delete-notes ;nofmt
    (my-notes-delete-notes-of-category category))
  (my-notes-delete-category-template category)
  (->>
   my-notes-categories
   (remove category)
   (my-notes-change-categories)))

(defun my-notes-delete-notes-of-category (category)
  "Delete each note of CATEGORY."
  (-> category (my-notes-category-path) (my-try-delete t)))

(defun my-notes-delete-category-template (category)
  "Delete template for CATEGORY."
  (->> category (my-notes-category-template-path) (my-try-delete)))

(defun my-notes-change-categories (new-categories)
  "Change list of categories to NEW-CATEGORIES with change category's file."
  (setq my-notes-categories new-categories)
  (my-notes-change-categories-list-file new-categories))

(defun my-notes-change-categories-list-file (new-categories)
  "Change categories with file in which its writed to NEW-CATEGORIES."
  (f-write-text
   (s-join "\n" new-categories)
   'utf-8
   my-notes-categories-list-file-path))

(defun fast-exec-my-notes-keys ()
  "Get some useful keymaps of  `fast-exec' for my-notes."
  (fast-exec/some-commands
   ("New Note" 'my-notes-new)
   ("Buffer to Note" 'my-notes-buffer-to-note)
   ("Delete Note" 'my-notes-delete)
   ("Change Category of Note" 'my-notes-change-category-of-note)
   ("New Category of Notes" 'my-notes-new-category)
   ("Delete Category of Notes" 'my-notes-delete-category)
   ("Rename Category of Notes" 'my-notes-rename-category)
   ("Find Template for Note" 'my-notes-visit-category-template)
   ("Find Note" 'my-notes-visit)))

(fast-exec/register-keymap-func 'fast-exec-my-notes-keys)
(fast-exec/reload-functions-chain)

(defcustom my-mipt-dir "c:/Users/hrams/Documents/mfti-solutions"
  "Path to directory in which will saved solutions of MIPT tasks.")

(defcustom my-mipt-lessons '("f" "m") "Lessons of MIPT.")

(defclass my-mipt-task ()
  ((class :initform nil :initarg :class :accessor my-mipt-task-class)
   (lesson :initform nil
           :initarg :lesson
           :accessor my-mipt-task-lesson)
   (section :initform nil
            :initarg :section
            :accessor my-mipt-task-section)
   (kind :initform nil :initarg :kind :accessor my-mipt-task-kind)   ; 'control or 'normal
   (number  :initform nil
            :initarg :number
            :accessor my-mipt-task-number))
  "Object for task of MIPT.")

(defvar my-mipt-found-task
  (my-mipt-task)
  "Object of `my-mipt-task', will set automatically when find task.")

(defun my-mipt-task-control-p (task)
  "Return t, when TASK is control."
  (eq (my-mipt-task-kind task) 'control))

(defun my-mipt-task-normal-p (task)
  "Return t, when TASK is normal, no control."
  (not (my-mipt-task-control-p task)))

(defun my-mipt-task-parse (filename)
  "Parse from FILENAME MIPT task."
  (when (s-matches-p ".+-.-.+-.+\\(-control\\)?\\.tex" filename)
    (-let*
        ((base (f-base (f-no-ext filename)))
         ((class lesson section num is-control)
          (s-split "-" base)))
      (my-mipt-task
       :class (string-to-number class)
       :lesson lesson
       :section (string-to-number section)
       :number (string-to-number num)
       :kind (if (stringp is-control) 'control 'normal)))))

(defun my-mipt-task-path (task)
  "Get path to TASK's solution."
  (->>
   (format
    "%s-%s-%s-%s%s.tex"
    (my-mipt-task-class task)
    (my-mipt-task-lesson task)
    (my-mipt-task-section task)
    (my-mipt-task-number task)
    (if (my-mipt-task-control-p task) "-control" ""))
   (f-join my-mipt-dir)))

(defun my-mipt-next-task ()
  "Return next task, after last found task."
  (interactive)
  (let ((next-task my-mipt-found-task))
    (incf (my-mipt-task-number next-task))
    (if (interactive-p)
        (my-mipt-task-visit next-task)
      next-task)))

(defun my-mipt-task-visit (task)
  "Visit file of TASK's solution."
  (interactive (list (my-mipt-find-task)))
  (->> task (my-mipt-task-path) (find-file)))

(defun my-mipt-all-tasks ()
  "Return all mipt tasks in special dir `my-mipt-dir'."
  (->> my-mipt-dir (f-files) (-keep #'my-mipt-task-parse)))

(defun my-mipt-find-task ()
  "Find task of MIPT from created."
  (interactive)
  (setq my-mipt-found-task (my-mipt-task))
  (->>
   (my-mipt-all-tasks)
   (my-mipt--find-lesson-from-tasks)
   (my-mipt--find-class-from-tasks)
   (my-mipt--find-section-from-tasks)
   (my-mipt--find-kind-from-tasks)
   (my-mipt--find-number-from-tasks))
  (my-mipt-complete-task my-mipt-found-task))

(defun my-mipt--find-lesson-from-tasks (tasks)
  "From TASKS find lesson, save in special variable, and return filtered TASKS.
Special variable is `my-mipt-found-task'"
  (let ((lesson (my-mipt-read-lesson)))
    (setf (my-mipt-task-lesson my-mipt-found-task) lesson)
    (->> tasks (--filter (string-equal (my-mipt-task-lesson it) lesson)))))

(defun my-mipt-read-lesson ()
  "Read from user MIPT's lesson."
  (completing-read "Choose one of MIPT lessons, please: " my-mipt-lessons))

(defun my-mipt--find-class-from-tasks (tasks)
  "From TASKS find class, save in special variable, and return filtered TASKS.
Special variable is `my-mipt-found-task'"
  (let* ((class (my-mipt-choose-one-of-task-classes tasks)))
    (setf (my-mipt-task-class my-mipt-found-task) class)
    (->> tasks (--filter (= (my-mipt-task-class it) class)))))

(defun my-mipt-choose-one-of-task-classes (tasks)
  "Take TASKS and choose one of classes."
  (->> tasks (-map #'my-mipt-task-class) (-max) (my-mipt-read-class)))

(defun my-mipt-read-class (&optional default)
  "Read from user class of MIPT task, defaults to DEFAULT."
  (read-number "Choose one of MIPT classes, please: " default))

(defun my-mipt--find-section-from-tasks (tasks)
  "From TASKS find section, save in special variable, and return filtered TASKS.
Special variable is `my-mipt-found-task'"
  (let* ((section (my-mipt-choose-one-of-task-sections tasks)))
    (setf (my-mipt-task-section my-mipt-found-task) section)
    (->> tasks (--filter (= (my-mipt-task-section it) section)))))

(defun my-mipt-choose-one-of-task-sections (tasks)
  "Take TASKS and choose one of sections."
  (->>
   tasks
   (-map #'my-mipt-task-section)
   (-max)
   (my-mipt-read-section)))

(defun my-mipt-read-section (&optional default)
  "Read from user section of MIPT task, defaults to DEFAULT."
  (read-number "Enter section of MIPT task, please: " default))

(defun my-mipt--find-kind-from-tasks (tasks)
  "From TASKS find kind, save in special variable, and return filtered TASKS.
Special variable is `my-mipt-found-task'"
  (let ((kind (my-mipt-choose-one-of-task-kinds tasks)))
    (setf (my-mipt-task-kind my-mipt-found-task) kind)
    (->> tasks (--filter (eq (my-mipt-task-kind it) kind)))))

(defun my-mipt-choose-one-of-task-kinds (tasks)
  "Take TASKS and choose one of kinds."
  (let* ((is-was-normal-tasks (-any #'my-mipt-task-normal-p tasks))
         (is-normal
          (if is-was-normal-tasks
              (y-or-n-p "Your task normal? ")
            (not (y-or-n-p "Your task control? ")))))
    (if is-normal 'normal 'control)))

(defun my-mipt-read-kind (&optional default)
  "Read from user kind of MIPT task, defaults to DEFAULT."
  (if (y-or-n-p "Your task normal? ") 'normal 'control))

(defun my-mipt--find-number-from-tasks (tasks)
  "From TASKS find number, save in special variable, and return filtered TASKS.
Special variable is `my-mipt-found-task'"
  (let* ((number (my-mipt-choose-one-of-task-numbers tasks)))
    (setf (my-mipt-task-number my-mipt-found-task) number)
    (->> tasks (--filter (= (my-mipt-task-number it) number)))))

(defun my-mipt-choose-one-of-task-numbers (tasks)
  "Take TASKS and choose one of classes."
  (->> tasks
       (-map #'my-mipt-task-number)
       (-max)
       (my-mipt-read-number)))

(defun my-mipt-read-number (&optional default)
  "Read from user number of MIPT's task, defaults to DEFAULT."
  (read-number "Please, type number of MIPT task: " default))

(defun my-mipt-complete-task (task)
  "Complete all fields of TASK, and return modified TASK."
  (my-mipt-task
   :class (or (my-mipt-task-class task) (my-mipt-read-class))
   :lesson (or (my-mipt-task-lesson task) (my-mipt-read-lesson))
   :section (or (my-mipt-task-section task) (my-mipt-read-section))
   :kind (or (my-mipt-task-kind task) (my-mipt-read-kind))
   :number (or (my-mipt-task-number task) (my-mipt-read-number))))

(defun fast-exec-mipt-keys ()
  "Get some useful keymaps of  `fast-exec' for MIPT."
  (fast-exec/some-commands
   ("Next MIPT Task" 'my-mipt-next-task)
   ("Find MIPT Task" 'my-mipt-task-visit)))

(fast-exec/register-keymap-func 'fast-exec-mipt-keys)
(fast-exec/reload-functions-chain)

(defun my-copy-buffer-content-as-mipt-solution ()
  "Take content of current buffer, compress it and copy its."
  (interactive)
  (->> (buffer-string) (my-compress-latex-source) (kill-new)))

(defun my-compress-latex-source (source)
  "Take SOURCE and return compressed variant."
  (->>
   source
   (s-replace "\n" " ")
I   (s-replace "\\begin{equation}" "\\[")
   (s-replace "\\end{equation}" "\\]")
   (s-append "Так как ваш сайт не любит большие решения, ")
   (s-append "то оно было уменьшено с помощю програмного кода")))

(bind-keys
 :map my-latex-local-map
 ("c" . my-copy-buffer-content-as-mipt-solution))
