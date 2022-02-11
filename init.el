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
(add-to-list 'load-path "~/projects/simple-indention.el")

(use-package s :ensure t)

(use-package f :ensure t)

(use-package dash :ensure t)

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
    (yas-wrap-around-region t)
    :bind (:map yas-keymap
                ("<return>" . yas-exit-all-snippets)))

(use-package yasnippet-classic-snippets :ensure t)

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
            (message "map-name is %s" map-name)
            (message "modes is %s" modes)
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

(defun keymap-to-list (keymap)
    "Convert `KEYMAP` to list."
    (--filter (ignore-errors '((cat it) (cdr it))) (-drop 1 keymap)))


(defun function-of-key (keymap key)
    "Get function bound on `KEY` in `KEYMAP`."
    (let* ((list-keymap (keymap-to-list keymap))
           (kbd-key (kbd key))
           (key-chars (string-to-list kbd-key))
           (head-key-char (-first-item key-chars))
           (tail-key-chars (-drop 1 key-chars))
           (object-on-key (--filter (ignore-errors
                                        (eq head-key-char (-first-item it)))
                                    list-keymap))
           )
        (cond
          (tail-key-chars
           (function-of-key object-on-key
                            (chars-to-string tail-key-chars)))
          (t (cdr (-first-item object-on-key)))))
    )


(defun chars-to-string (chars)
    "Convert list of `CHARS` to string."
    (--reduce-from (s-concat acc (char-to-string it)) "" chars))


(defmacro define-key-when (keymap key def condition)
    "Macro for define keymaps for `rectangle-mode` in `xah-fly-command-mode`"
    `(define-key ,keymap (kbd ,key)
         (lambda ()
             (interactive)
             (if (funcall ,condition)
                 (call-interactively ,def)
                 (call-interactively ',(function-of-key (eval keymap) key))))))

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

(use-package semantic
    :ensure t)

(use-package imenu
    :custom (imenu-auto-rescan t))

(bind-keys :map xah-fly-command-map
           ("SPC SPC SPC" . helm-semantic-or-imenu))

(use-package imenu-anywhere
    :ensure t
    :bind (:map xah-fly-command-map
                ("SPC SPC n" . imenu-anywhere)))

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
    :bind
    (:map xah-fly-command-map
          ("7" . my-mc-mark-like-this-or-edit-lines)
          ("SPC 7" . mc/mark-previous-like-this-word)
          ("SPC TAB 7" . mc/reverse-regions)
          ("SPC h" . my-bob-or-mc-align)
          ("SPC n" . my-eob-or-mc-align-with-space)
          ("SPC a" . my-mark-all)))

(use-package avy
    :ensure t
    :custom (avy-background t)
    (avy-translate-char-function #'translate-char-from-russian)
    :bind ((:map xah-fly-command-map)
           ("n"           . avy-goto-char)
           ("SPC SPC v"   . avy-yank-word)
           ("SPC SPC x"   . avy-teleport-word)
           ("SPC SPC t"   . avy-mark-word)
           ("SPC SPC 5"   . avy-zap)
           ("SPC SPC c"   . avy-copy-word)
           ("SPC SPC d"   . avy-kill-word-stay)
           ("SPC SPC -"   . avy-sp-splice-sexp-in-word)
           ("SPC SPC 8"   . avy-kill-word-move)
           ("SPC SPC o"   . avy-change-word)
           ("SPC SPC 9"   . avy-sp-change-enclosing-in-word)
           ("SPC SPC z"   . avy-comment-line)
           ("SPC SPC 5"   . avy-zap)
           ("SPC SPC a v" . avy-copy-region)
           ("SPC SPC a d" . avy-kill-region)
           ("SPC SPC a x" . avy-move-region)
           ("SPC SPC a c" . avy-kill-ring-save-region)
           ("SPC SPC l v" . avy-copy-line)
           ("SPC SPC l ;" . avy-goto-end-of-line)
           ("SPC SPC l x" . avy-move-line)
           ("SPC SPC l c" . avy-kill-ring-save-whole-line)
           ("SPC SPC l d" . avy-kill-whole-line)))


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
    (interactive (list (read-char "char: " t)
                       current-prefix-arg))
    (avy-with avy-goto-word-1
        (let* ((str (string char))
               (regex (cond ((string= str ".")
                             "\\.")
                            ((and avy-word-punc-regexp
                                  (string-match avy-word-punc-regexp str))
                             (regexp-quote str))
                            ((<= char 26)
                             str)
                            (t
                             (concat
                              (if symbol "\\_<" "\\b")
                              str)))))
            (avy-jump regex
                      :window-flip arg
                      :beg beg
                      :end end
                      :action action))))

(defun avy-zap (char &optional arg)
    "Zapping to next CHAR navigated by `avy'."
    (interactive "cchar:\nP")
    (avy-jump (s-concat (char-to-string char))
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
    (save-excursion
        (goto-char pt)
        (comment-line 1)))

(defun avy-sp-change-enclosing-in-word (ch)
    "With `avy' move to word starting with CH and `sp-change-enclosing'."
    (interactive "cchar:")
    (avy-goto-word-1-with-action ch 'avy-action-sp-change-enclosing))

(defun avy-action-sp-change-enclosing (pt)
    "Saving excursion `sp-change-enclosing' in word at point PT."
    (save-excursion
        (goto-char pt)
        (sp-change-enclosing)))

(defun avy-sp-splice-sexp-in-word (ch)
    "With `avy' move to word starting with CH and `sp-splice-sexp'."
    (interactive "cchar:")
    (avy-goto-word-1-with-action ch 'avy-action-sp-splice-sexp))

(defun avy-action-sp-splice-sexp (pt)
    "Saving excursion `sp-splice-sexp' in word at point PT."
    (save-excursion
        (goto-char pt)
        (sp-splice-sexp)))

(defun avy-change-word (ch)
    "With `avy' move to word starting with CH and change its any other."
    (interactive "cchar:")
    (avy-goto-word-1-with-action ch 'avy-action-change-word))

(defun avy-action-change-word (pt)
    "Saving excursion navigate to word at point PT and change its."
    (save-excursion
        (avy-action-kill-move pt)
        (insert (read-string "new word, please: "))))

(use-package smartparens
    :ensure t
    :init
    (smartparens-global-mode 1)
    :bind (:map xah-fly-command-map
                (("]" . sp-forward-slurp-sexp)
                 ("[" . sp-backward-slurp-sexp)
                 ("-" . sp-splice-sexp)
                 ("SPC -" . sp-rewrap-sexp)
                 ("m" . sp-backward-sexp)
                 ("." . sp-forward-sexp)
                 ("SPC 1" . sp-join-sexp)
                 ("SPC SPC 1" . sp-split-sexp)
                 ("SPC 9" . sp-change-enclosing)
                 ("SPC SPC g" . sp-kill-hybrid-sexp)
                 ("SPC =" . sp-raise-sexp)
                 ("M-(" . sp-wrap-round)
                 ("M-{" . sp-wrap-curly))))

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

(bind-keys :map
           xah-fly-command-map
           ("SPC TAB o" . transpose-words)
           ("SPC TAB u" . backward-transpose-words)

           ("SPC TAB l" . transpose-chars)
           ("SPC TAB j" . backward-transpose-chars)

           ("SPC TAB i" . backward-transpose-lines)
           ("SPC TAB k" . transpose-lines)

           ("SPC TAB ." . transpose-sexps)
           ("SPC TAB m" . backward-transpose-sexps)

           ("SPC TAB n" . avy-transpose-lines-bacin-region)
           ("SPC TAB t" . transpose-regions))


(defun backward-transpose-words ()
    "As `transpose-words' but set position to backward of transpose."
    (interactive)
    (transpose-words -1))


(defun backward-transpose-chars ()
    "As `transpose-chars' but set position to backward of transpose."
    (interactive)
    (transpose-chars -1))


(defun backward-transpose-lines ()
    "As `transpose-lines' but set position to backward of transpose."
    (interactive)
    (transpose-lines -1))


(defun backward-transpose-sexps ()
    "As `transpose-sexps' but set position to backward of transpose."
    (interactive)
    (transpose-sexps -1))

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

(define-key-when xah-fly-command-map "-" 'exchange-point-and-mark 'use-region-p)

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

(define-key-when xah-fly-command-map "c" 'copy-rectangle-as-kill
         (lambda () rectangle-mark-mode))

(define-key-when xah-fly-command-map "d" 'kill-rectangle
         (lambda () rectangle-mark-mode))

(define-key-when xah-fly-command-map "x" 'kill-rectangle
         (lambda () rectangle-mark-mode))

(define-key-when xah-fly-command-map "f" 'replace-rectangle
         (lambda () rectangle-mark-mode))

(define-key-when xah-fly-command-map "s" 'open-rectangle
        (lambda () rectangle-mark-mode))

(define-key-when xah-fly-command-map "-" 'rectangle-exchange-point-and-mark
        (lambda () rectangle-mark-mode))

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

(define-key xah-fly-command-map (kbd "SPC e") 'xah-fly-c-keymap)

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

(use-package auctex
    :ensure t)

(setq latex-documentclasses
    '("article" "reoport" "book" "proc" "minimal" "slides" "memoir" "letter" "beamer"))

(dolist (mode (list 'TeX-mode-hook
                    'tex-mode-hook
                    'latex-mode-hook
                    'LaTeX-mode-hook))
    (add-hook mode (lambda () (call-interactively 'visual-fill))))

(defun latex-wrap-text (command)
    "If regions select, wrap region with COMMAND, otherwise wrap word."
    (unless (use-region-p)
        (set-mark (point))
        (forward-word)
        (exchange-point-and-mark)
        (backward-word))
    (goto-char (region-beginning))
    (insert (s-lex-format "\\${command}{"))
    (goto-char (region-end))
    (insert "}")
    (indent-region (region-beginning) (region-end)))


(defun latex-make-text-italic ()
    "If regions select, wrap region with `emph`, otherwise make word."
    (interactive)
    (latex-wrap-text "emph"))

(defun latex-make-text-bold ()
    "If regions select, wrap region with `textbf`, otherwise make word."
    (interactive)
    (latex-wrap-text "emph"))

(defun latex-make-text-formula ()
    "If regions select, make region formula, otherwise make line formula."
    (interactive)
    (unless (use-region-p)
        (end-of-line)
        (set-mark (point-at-bol)))
    (let ((text-beg (region-beginning))
          (text-end (region-end)))
        (deactivate-mark)
        (goto-char text-beg)
        (insert "\\[")
        (goto-char (+ text-end 2))
        (insert "\\]")))


(use-package latex
    :major-mode-map (TeX-mode LaTeX-mode tex-mode latex-mode)
    :bind (:map
           my-latex-local-map
           ("m" . helm-insert-latex-math)
           ("i" . latex-make-text-italic)
           ("b" . latex-make-text-bold)
           ("f" . latex-make-text-formula)))

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

(use-package org
    :major-mode-map (org-mode)
    :bind (:map
           my-org-local-map
           ("'"   . org-edit-special)
           ("l"   . org-insert-link)
           ("RET" . org-open-at-point)))

(add-hook 'org-mode-hook (lambda () (call-interactively 'visual-fill)))

(use-package wikinforg
  :ensure t)

(use-package org-download
    :ensure t
    :hook
    (dired-mode-hook . org-download-enable)
    )

(use-package helm-org
    :ensure t
    :bind (:map org-mode-map
                ([remap helm-semantic-or-imenu]
                 . helm-org-in-buffer-headings)))

(use-package package-lint
    :ensure t
    )

(use-package flycheck-package
    :ensure t
    :init
    (flycheck-package-setup)
    )

(use-package emr
    :ensure t
    :bind (:map xah-fly-command-map
                ("SPC /" . emr-show-refactor-menu)))

(use-package cask-mode
    :ensure t
    )

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
