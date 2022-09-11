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

(use-package pcache
    :ensure t)

(use-package org-ml
    :ensure t)

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

(defun my-parts-of-string (n s)
  "Divide string S to N same parts.'"
  (->>
   (my-parts-of-seq n s)
   (--map (apply #'s-concat it))))

(defcustom my-url-prefixes
  '("http://"
    "https://"
    "file://")
  "List of the prefixes which indicates that string is a URL."
  :type '(repeat string))

(defun my-url-p (s)
  "Return t, when S is a url."
  (my-one-of-prefixes-p my-url-prefixes s))

(defun my-regexp-prefix-p (prefix s)
  "Return t when the regexp PREFIX matches with S as prefix."
  (-some->>
      s
    (s-matched-positions-all prefix)
    (-first-item)
    (car)                                ; the beginning of the matched position
    (= 0)))

(defun my-max (list)
  "Return the max value of LIST, if LIST is empty, then return nil."
  (when list
    (-max list)))

(defun my-into-list (obj)
  "Transform OBJ to list.
Supoorted types of OBJ:
- `string'
- `list'"
  (cl-typecase obj
    (list obj)
    (string (my-string-to-list obj))))

(defun my-string-to-list (s)
  "Transform S to list of 1 size string."
  (->>
   s
   (string-to-list)
   (-map #'char-to-string)))

(defun my-parts-of-seq (n seq)
  "Divide SEQ to N same parts.
SEQ may be one of types which supported in function `my-into-list'"
  (setq seq (my-into-list seq))
  (let ((step (/ (length seq) n)))
    (-partition-in-steps step step seq)))

(defun my-goto-lisp-sexp-begin (start-name)
  "Go to backward beginning of Lisp sexp which start with START-NAME."
  (when (search-backward-regexp
         (rx "(" (zero-or-more whitespace) (regexp start-name))
         nil t)
    (skip-chars-forward "(")))

(defun my-goto-lisp-sexp-end (start-name)
  "Go to end of the backward Lisp sexp which start with START-NAME.
End of Lisp sexp is point before the last closed paren"
  (my-goto-lisp-sexp-begin start-name)
  (forward-char -1)
  (sp-get (sp-get-sexp)
    (goto-char :end-in)))

(defun my-mark-lisp-sexp-inner (start-name)
  "Mark the inner of the Lisp sexp which start with function START-NAME."
  (my-goto-lisp-sexp-begin start-name)
  (forward-char -1)
  (sp-get (sp-get-sexp)
    (just-mark-region :beg-in
                      :end-in)))

(defun my-in-lisp-sexp-p (start-name &optional pt)
  "Get t, When cursor at PT placed in Lisp sexp which start with START-NAME."
  (setq pt (or pt (point)))
  (save-excursion
    (goto-char pt)
    (when (my-goto-lisp-sexp-begin start-name)
      (-when-let
          (sexp (sp-get-enclosing-sexp))
        (sp-get sexp (< :beg pt :end))))))

(defun my-read-image-url ()
  "Read the URL of a image from the user.

If copied text is a URL, then return.  If region is active, then return a text
in the region.  Otherwise, read a URL from the minibuffer."
  (or
   (my-url-from-kill-ring)
   (just-text-in-region)
   (read-string "Enter URL for image, please: ")))

(defun my-url-from-kill-ring ()
  "If the last element of the kill ring is a URL, get it, otherwise get nil."
  (let ((copied (-first-item kill-ring)))
    (when (my-url-p copied)
      copied)))

(defun prime-p (n)
  "Return non-nil, when N is prime."
  ;; `n' has divisors greater than 1 and `n'
  (> (length (divisors-of n)) 2))

(defun divisors-of (n)
  "Return divisors of N."
  (->>
   n
   (-iota)
   (cdr)                                ; ignore zero
   (--filter (= (% n it) 0))
   (cons n)
   (-rotate 2)))

(use-package just
    :load-path "~/projects/just/")

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
    (if (eq (-first-item args) t)
        (list (symbol-name name) (list name))
      (cl-typecase (-first-item args)
        (null nil)
        (list (setq modes (-first-item args)))
        (symbol (setq map-name (symbol-name (-first-item args))))
        (string (setq map-name (-first-item args))))
      (cl-typecase (-second-item args)
        (null nil)
        (list (setq modes (-second-item args)))
        (symbol (setq map-name (symbol-name (-second-item args))))
        (string (setq map-name (-second-item args))))
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
                                        package
                                        skeletor
                                        yasnippet
                                        format-all
                                        wikinforg
                                        suggest
                                        devdocs
                                        helm-wikipedia)

(fast-exec/initialize)

(define-key xah-fly-command-map (kbd "=") 'fast-exec/exec)

(use-package aas
    :ensure t
    :config
    (aas-global-mode))

(use-package yasnippet
    :ensure t
    :init
    (yas-global-mode 1)
    :custom
    (yas-snippet-dirs '("~/.emacs.d/snippets"))
    (yas-wrap-around-region t))

(defun yas--fetch (table key)
  "Fetch templates in TABLE by KEY.

Return a list of cons (NAME . TEMPLATE) where NAME is a
string and TEMPLATE is a `yas--template' structure."
  (let* ((key (s-downcase key))
         (keyhash (yas--table-hash table))
         (namehash (and keyhash (gethash key keyhash))))
    (when namehash
      (yas--filter-templates-by-condition
       (yas--namehash-templates-alist namehash)))))

(defun fast-exec-my-yas-keys ()
  "Get some useful keymaps of  `fast-exec' for my-yas."
  (fast-exec/some-commands ("Yasnippet Edit Snippet" 'yas-visit-snippet-file)))

(fast-exec/register-keymap-func 'fast-exec-my-yas-keys)
(fast-exec/reload-functions-chain)

(use-package flycheck
    :ensure t
    :config
    '(custom-set-variables
      '(flycheck-display-errors-function
        #'flycheck-pos-tip-error-messages))
    (global-flycheck-mode 1))

(defun turn-off-flycheck ()
  "Disable `flycheck-mode' locally for current buffer."
  (interactive)
  (flycheck-mode 0))

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

(use-package helm-swoop
    :ensure t
    :bind ((:map xah-fly-command-map)
           ("'" . 'helm-swoop)
           ("SPC k '" . 'helm-multi-swoop-current-mode)
           (:map helm-swoop-map)
           ("M-j" . 'helm-swoop-edit)
           (:map helm-swoop-edit-map)
           ([remap save-buffer] . 'helm-swoop--edit-complete)))

(use-package deadgrep
    :ensure t
    :bind (:map
           xah-fly-command-map
           ("SPC '" . deadgrep)))

(define-key xah-fly-command-map (kbd "SPC SPC r") 'projectile-replace)

(use-package visual-regexp
    :ensure t
    :bind ((:map xah-fly-command-map)
           ("SPC r" . 'vr/query-replace)))

(use-package string-inflection
    :ensure t
    :bind ((:map xah-fly-command-map)
           ("b" . 'string-inflection-cycle)))

(defcustom my-aggresive-indent-in-modes '(racket-mode
                                          css-mode
                                          emacs-lisp-mode
                                          eshell-mode)
  "List of major modes in which should work `aggressive-indent-mode'."
  :type '(repeat symbol))

(use-package aggressive-indent
    :ensure t
    :init
    (--each my-aggresive-indent-in-modes
      (add-hook (->> it
                     (symbol-name)
                     (s-append "-hook")
                     (intern))
                #'aggressive-indent-mode)))

(use-package imenu
    :custom (imenu-auto-rescan t))

(bind-keys :map xah-fly-command-map
           ("SPC SPC SPC" . helm-imenu))

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

(defun my-buffer-list-or-edit-lines ()
  "Do `helm-buffer-list' or `mc/edit-lines'."
  (interactive)
  (if (use-region-p)
      (call-interactively #'mc/edit-lines)
    (call-interactively #'helm-multi-files)))

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
    :custom
    (avy-background t)
    :bind ((:map xah-fly-command-map)
           ("n"     . nil)              ;by default this is `isearch', so i turn
                                        ;this to keymap
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
    :bind (("RET"       . 'sp-newline)
           ("M-("       . 'sp-wrap-round)
           ("M-{"       . 'sp-wrap-curly)
           (:map xah-fly-command-map)
           ("]"         . 'sp-forward-slurp-sexp)
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
           ("SPC SPC e" . 'sp-splice-sexp-killing-backward)
           ("SPC ="     . 'sp-raise-sexp)))

(require 'smartparens-config)

(defun delete-only-1-char ()
  "Delete only 1 character before point."
  (interactive)
  (backward-char)
  (delete-char 1)
  )

(define-key xah-fly-command-map (kbd "DEL") 'delete-only-1-char)

(use-package embrace
    :ensure t
    :bind ((:map xah-fly-command-map)
           ("/"         . 'embrace-commander)
           ("SPC SPC /" . 'xah-goto-matching-bracket))
    :config ;nofmt
    (add-hook 'emacs-lisp-mode-hook 'embrace-emacs-lisp-mode-hook)
    (add-hook 'org-mode-hook 'embrace-org-mode-hook))

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

(use-package eldoc
    :custom ((eldoc-idle-delay 0.01)))

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
  ""
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

(defun my-drag-org-right ()
  "Try drag anything `org-mode' thing to right."
  (interactive)
  (when (my-drag-org-p)
    (org-metaright)
    t))

(defun my-drag-org-left ()
  "Try drag anything `org-mode' thing to left."
  (interactive)
  (when (my-drag-org-p)
    (org-metaleft)
    t))

(defun my-drag-org-up ()
  "Try drag anything `org-mode' thing to up."
  (interactive)
  (when (my-drag-org-p)
    (org-metaup)
    t))

(defun my-drag-org-down ()
  "Try drag anything `org-mode' thing to down."
  (interactive)
  (when (my-drag-org-p)
    (org-metadown)
    t))

(defun my-drag-org-p ()
  "Return t, when draggers for `org-mode' should work."
  (and
   (eq major-mode 'org-mode)
   (or
    (my-org-mode-in-heading-start-p)
    (my-org-mode-at-list-item)
    (org-at-table-p))))

(defun my-org-mode-in-heading-start-p ()
  "Return t, when the current position being at a `org-mode' heading."
  (interactive "d")
  (and
   (not (org-in-src-block-p))
   (just-line-prefix-p "*")))

(defun my-org-mode-at-list-item ()
  "Return t, when the current position being at an item of a `org-mode' list."
  (interactive "d")
  (and
   (not (org-in-src-block-p))
   (or
    (just-line-prefix-p "-" nil t)
    (just-line-regexp-prefix-p "[0-9]+."))))

(add-right-dragger 'my-drag-org-right)
(add-left-dragger 'my-drag-org-left)
(add-down-dragger 'my-drag-org-down)
(add-up-dragger 'my-drag-org-up)

(defun my-latex-drag-right-list-item ()
  "Drag the list item at the point to the right LaTeX list item."
  (interactive)
  (my-latex-mark-list-item)
  (let ((1-list-item (just-text-in-region)))
    (delete-region (region-beginning) (region-end))
    (my-latex-end-of-list-item)
    (insert 1-list-item)
    (my-latex-goto-backward-list-item)))

(defun my-latex-drag-left-list-item ()
  "Drag the list item at the point to the left LaTeX list item."
  (interactive)
  (my-latex-mark-list-item)
  (let ((1-list-item (just-text-in-region)))
    (delete-region (region-beginning) (region-end))
    (my-latex-goto-backward-list-item)
    (insert 1-list-item)
    (my-latex-goto-backward-list-item)))

(defun my-latex-mark-list-item ()
  "Mark as region from the start of the current list item to end of that."
  (interactive)
  (my-latex-beginning-of-list-item)
  (push-mark nil nil t)
  (my-latex-end-of-list-item))

(defun my-latex-beginning-of-list-item ()
  "Go to the beginning of an LaTeX list item."
  (interactive)
  (end-of-line)
  (just-search-backward-one-of-regexp
   '("\\\\item"                         ;nofmt
     "\\\\begin *{\\(enumerate\\|itemize\\)}")))

(defun my-latex-end-of-list-item ()
  "Go to the end of an LaTeX list item."
  (interactive)
  (end-of-line)
  (just-search-forward-one-of-regexp
   '("\\\\item"                         ;nofmt
     "\\\\end *{\\(itemize\\|enumerate\\)}"))
  (beginning-of-line))

(defun my-latex-goto-backward-list-item ()
  "Go to the beginning of the backward list item."
  (beginning-of-line)
  (search-backward my-latex-list-item-string))

(defun my-latex-list-item-drag-p ()
  "Return t, when dragger for LaTeX list items should work."
  (interactive)
  (and (eq major-mode 'latex-mode) (my-latex-list-item-line-p)))

(defun my-latex-list-item-line-p ()
  "Return t, when current line is a LaTeX list item."
  (just-line-prefix-p "\\item" nil t))

(defun my-latex-try-drag-right-list-item ()
  "If the dragger for LaTeX list item should be work, drag that to right."
  (interactive)
  (when (my-latex-list-item-drag-p)
    (my-latex-drag-right-list-item)
    t))

(defun my-latex-try-drag-left-list-item ()
  "If the dragger for LaTeX list item should be work, drag that to left."
  (interactive)
  (when (my-latex-list-item-drag-p) (my-latex-drag-left-list-item) t))

(add-up-dragger 'my-latex-try-drag-left-list-item)
(add-down-dragger 'my-latex-try-drag-right-list-item)

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
        (one-shot-keybinding "d"
                             (lambda (duplicate-region 1 beg end))))
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

(defun my-change-case-of-current-line ()
  "Change case of current line to next (see `xah-toggle-letter-case')."
  (interactive)
  (save-mark-and-excursion
    (select-current-line)
    (xah-toggle-letter-case)))

(bind-keys
 :map xah-fly-command-map
 ("SPC SPC b" . my-change-case-of-current-line)
 ("b"         . my-toggle-change-case-of-line-or-word-or-selection))

(defvar my-last-command-is-changed-case-of-current-line
  nil "In t, when last command change case.")

(defun my-toggle-change-case-of-line-or-word-or-selection ()
  "Using one of functions, which change case.
List of functions: `xah-toggle-letter-case', `my-change-case-of-current-line'."
  (interactive)
  (let* ((change-case-of-line
          (or
           (eq last-command 'my-change-case-of-current-line)
           (and
            (eq
             last-command
             'my-toggle-change-case-of-line-or-word-or-selection)
            my-last-command-is-changed-case-of-current-line))))
    (setq my-last-command-is-changed-case-of-current-line change-case-of-line)
    (if change-case-of-line
        (my-change-case-of-current-line)
      (xah-toggle-letter-case))))

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

(defun my-duplicate-last-block ()
  "Take last text block and insert."
  (interactive)
  (while (looking-back "[\n\t ]") (delete-backward-char 1))
  (->>
   (buffer-substring (my-point-at-last-block-beg) (point))
   (s-trim)
   (s-append "\n")
   (s-prepend "\n\n")
   (insert))
  (goto-char (my-point-at-last-block-beg)))

(defun my-point-at-last-block-beg ()
  "Return the position of beginning of last block."
  (interactive)
  (save-excursion
    (if (re-search-backward "\n[\t\n ]*\n+" nil 1)
        (match-end 0)
      (point-min))))

(bind-keys*
 :map xah-fly-command-map
 ("SPC k 6" . my-duplicate-last-block))

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

(defun my-visit-last-opened-buffer ()
  "Visit buffer which was opened recently."
  (interactive)
  (->>
   (buffer-list)
   (cdr)
   (--find (not (my--visit-last-opened-buffer-ignore-p it)))
   (switch-to-buffer)))

(defun my--visit-last-opened-buffer-ignore-p (buffer)
  "Take object of BUFFER and return nil when don't need visit its."
  (->>
   buffer
   (buffer-name)
   (s-trim)
   (s-prefix-p "*Minibuf")))

(bind-keys
 :map xah-fly-command-map
 ("SPC 0" . my-visit-last-opened-buffer))

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

(defcustom my-autoformat-all-functions
  '(sentence-capitalization)
  "All my autoformat functions.")

(defun my-use-autoformat-function-p (f)
  "Return t, when must use F as autoformat function."
  (-contains-p my-autoformat-functions f))

(defmacro my-use-autoformat-in-mode (mode &rest autoformat-functions)
  "Add hook to MODE, which enable AUTOFORMAT-FUNCTIONS."
  (let* ((hook
          (intern (s-append "-hook" (symbol-name (eval mode)))))
         (fun-name
          (->>
           mode
           (eval)
           (symbol-name)
           (s-prepend "my-autoformat-set-functions-for-")
           (intern)))
         (funcs
          (--map
           (intern (s-prepend "autoformat-" (symbol-name it)))
           autoformat-functions)))
    `(progn
       (defun ,fun-name ()
         "Add autoformat special functions for mode."
         (interactive)
         (setq-local my-autoformat-functions ',funcs))
       (add-hook ',hook ',fun-name))))

(defmacro my-also-use-autoformat-in-mode (mode &rest autoformat-functions)
  "Add hook to MODE, which enable AUTOFORMAT-FUNCTIONS plus default functions."
  `(my-use-autoformat-in-mode ,mode
                              ,@(-concat autoformat-functions
                                         my-autoformat-all-functions)))

(defmacro my-use-all-autoformat-in-mode (mode)
  "Use my all autoformat functions in MODE."
  `(my-use-autoformat-in-mode ,mode ,@my-autoformat-all-functions))

(defun autoformat-sentence-capitalization ()
  "Auto-capitalize first words of a sentence.
Either at the beginning of a line, or after a sentence end."
  (interactive)
  (when (and
         (my-in-text-p)
         (looking-back "[а-яa-z]")
         (save-excursion
           (forward-char -1)
           (or
            (bobp)
            (looking-back (sentence-end))
            (and
             (skip-chars-backward " ")
             (bolp)
             (my-previous-line-is-empty))
            (and
             (skip-chars-backward " ")
             (< (skip-chars-backward "*") 0)
             (bolp)))))
    (undo-boundary)
    (capitalize-word -1)))

(defun my-previous-line-is-empty ()
  "Move to previous line and return t, when this line is empty.
See `just-line-is-whitespaces-p'"
  (just-call-on-prev-line 'just-line-is-whitespaces-p))

(defun my-in-text-p ()
  "Return t, when cursor has position on common text."
  (and
   (not (org-in-src-block-p))
   (not (texmathp))))

(defun my-autoformat ()
  "Call all autoformat functions."
  (interactive)
  (--each my-autoformat-functions
    (funcall it)))

(define-minor-mode my-autoformat-mode
    "Toggle `my-autoformat-mode'."
  :init-value nil
  (if my-autoformat-mode
      (add-hook 'post-self-insert-hook #'my-autoformat)
    (remove-hook 'post-self-insert-hook #'my-autoformat)))

(my-autoformat-mode t)

(use-package tex
    :ensure auctex
    :major-mode-map latex
    (LaTeX-mode)
    :hook ((latex-mode . my-latex-find-master-file)
           (latex-mode . LaTeX-mode)))

(defun my-latex-find-master-file ()
  "Find auctex master file for the current buffer."
  (interactive)
  (setq-local TeX-master
              (or
               (my-latex-lookup-master-file-of (buffer-file-name))
               t)))

(defcustom my-latex-master-files-alist
  '(("~/zms/*/solutions/*.tex" . "../Solution.tex"))
  "Associated list, keys are wildcards, values are him master files."
  :type '(alist :key-type string :value-type string))

(defun my-latex-lookup-master-file-of (filename)
  "Lookup a auctex master file for the file with FILENAME."
  (->>
   my-latex-master-files-alist
   (--find (my-latex-master-file-key-matches-with-p it filename))
   (cdr)))

(defun my-latex-master-file-key-matches-with-p (master-file-key filename)
  "Return t, when master file key alist MASTER-FILE-KEY match with FILENAME."
  (->
   master-file-key
   (car)
   (f-full)
   (wildcard-to-regexp)
   (string-match-p filename)))

(use-package latex
    :ensure auctex
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
            (kill-new expr)
            (message "coppied: %s" (current-kill 0))))

    (defun my-calc-simplify-region-change (beg end)
        "Get from BEG to END change this via `calc' and yank instead of region."
        (interactive "r")
        (let* ((expr (buffer-substring beg end))
               (simplified (my-calc-simplify expr)))
            (goto-char beg)
            (delete-region beg end)
            (insert simplified))))

(use-package xenops
    :ensure t
    ;; :hook
    ;; (LaTeX-mode . xenops-mode)
    :custom
    (xenops-math-image-scale-factor 2))

(use-package math-preview
    :ensure t
    :custom
    (math-preview-preprocess-functions
     (list (lambda (s) (s-concat "{\\color{white}" s "}"))))
    :config
    (defun my-latex-preview-in-other-window ()
      "Preview fragment of LaTeX source at point in seperated window."
      (interactive)
      (let ((source
             (save-mark-and-excursion
               (xah-select-block)
               (buffer-substring (region-beginning) (region-end)))))
        (switch-to-buffer-other-window "*my-latex-preview*")
        (delete-region (point-min) (point-max))
        (LaTeX-mode)
        (insert source)
        (math-preview-region (point-min) (point-max))))
    (defun fast-exec-math-preview-keys ()
      "Get some useful keymaps of  `fast-exec' for math-preview."
      (fast-exec/some-commands
       ("Preview All Latex Fragments" 'math-preview-all)))

    (fast-exec/register-keymap-func 'fast-exec-math-preview-keys)
    (fast-exec/reload-functions-chain)
    :bind
    ((:map my-latex-local-map)
     ("v" . 'my-latex-preview-in-other-window)))

(use-package magic-latex-buffer
    :ensure t
    :hook (LaTeX-mode . magic-latex-buffer))

(defvar my-latex-insert-at-start-arg-type 'optional
  "Type of argument (optional or required) which will be inserted at start.")

(defun my-latex-insert-command (name &rest args)
  "Insert a LaTeX command named NAME with required arguments ARGS.

Each arg of ARGS will be inserted as string, which will be taked with function
`format' and the argument string is \"%s\"

If one of ARGS is nil, then don't insert it.  If one of ARGS is `alist', then
insert it by the following form (see `my-latex-format-for-arg' for understand of
formating):

key1=val1,key2=val2

If you need to use optional arguments, add to ARGS the keyword `:optional' and
each element after it will be inserted as optional argument."
  (insert "\\" name)
  (my-latex--insert-args args)
  (indent-region (point-at-bol) (point-at-eol)))

(defun my-latex--insert-args (args)
  "Insert ARGS as LaTeX arguments.

See `my-latex-insert-command' for understand of ARGS"
  (-let (((required optional)
          (my-latex-optional-and-required-args args)))
    (case my-latex-insert-at-start-arg-type
      ((optional)
       (my-latex--insert-optional-args optional)
       (my-latex--insert-required-args required))
      ((required)
       (my-latex--insert-required-args required)
       (my-latex--insert-optional-args optional)))))

(defun my-latex--insert-optional-args (optional)
  "Insert list OPTIONAL as optional LaTeX arguments."
  (-each optional 'my-latex-insert-optional-arg))

(defun my-latex--insert-required-args (required)
  "Insert list REQUIRED as required LaTeX arguments."
  (-each required 'my-latex-insert-required-arg))

(defun my-latex-optional-and-required-args (args)
  "Split ARGS to 2 lists: required and optional arguments.

Optional arguments are elements of ARGS after the `:optional' keyword, required
are rest"
  (let* ((break-index (-elem-index :optional args)))
    (case break-index
      ((nil)
       (list args nil))
      ((0)
       (list nil (cdr args)))           ; here cdr ignore
                                        ; the first element of args: :optional
      (t
       (list
        (-slice args 0 break-index)
        (-slice args (1+ break-index)))))))

(defun my-latex-insert-required-arg (val)
  "Insert VAL as an LaTeX required argument."
  (--when-let (my-latex-format-for-arg val)
    (insert "{" it "}")))

(defun my-latex-insert-optional-arg (val)
  "Insert VAL as an LaTeX optional argument."
  (--when-let (my-latex-format-for-arg val)
    (insert "[" it "]")))

(defun my-latex-format-for-arg (val)
  "Format VAL as string for inserting of an LaTeX argument."
  (cond
    ((my-alist-p val)
     (-some->>
         val
       (-filter 'cdr)
       (--map
        (format "%s=%s" (car it) (cdr it)))
       (s-join ",")))
    (val
     (format "%s" val))))

(defun my-alist-p (obj)
  "Return t, when OBJ is `alist'."
  (and
   (listp obj)
   (not (null obj))
   (consp (car obj))))

(defun my-latex-insert-single-line-command (command &rest args)
  "Insert the LaTeX command named COMMAND with ARGS.

This version of `my-latex-insert-command', so see `my-latex-insert-command'"
  (just-ensure-empty-line)
  (apply 'my-latex-insert-command command args))

(defun my-latex-insert-env (name &rest args)
  "Insert the LaTeX environment named NAME with ARGS.

See `my-latex-insert-command' for understand of use ARGS"
  (let ((beg (point))
        end)
    (my-latex-insert-single-line-command "begin" name)
    (my-latex--insert-args args)
    (newline)
    (insert "  ")
    (save-excursion
      (newline)
      (my-latex-insert-command "end" name)
      (setq end (point)))
    (run-hook-with-args LaTeX-after-insert-env-hook name beg end)))

(defun my-latex-expand-define-function (key fun)
  "Bind call of FUN at KEY in the LaTeX.

FUN will be called when the user press KEY and dot"
  (aas-set-snippets 'latex-mode
    (s-concat key ".") fun))

(defmacro my-latex-expand-define (key name args &rest body)
  "Bind evaluation of BODY at KEY in LaTeX, define function with NAME and ARGS.

BODY will be evaluated when the user press KEY and dot"
  (declare (indent 3))
  `(progn
     (defun ,name ,args
       ,@body)
     (my-latex-expand-define-function ,key ',name)))

(define-minor-mode my-latex-expansion-mode
    "Which expands certain text fragments to LaTeX objects."
  :init-value nil
  (if my-latex-expansion-mode
      (progn
        (aas-mode +1)
        (aas-activate-keymap 'latex-mode))
    (aas-deactivate-keymap 'latex-mode)))

(use-package tex
    :ensure auctex
    :hook (LaTeX-mode . my-latex-expansion-mode))

(defun my-latex-ensure-has-graphicspath ()
  "Ensure that current LaTeX buffer has \\graphicspath{}.

If now current LaTeX buffer hasn't \\graphicspath{}, read new graphicspath from
the minibuffer."
  (unless (my-latex-current-graphicspathes)
    (my-latex-add-graphicspath
     (read-directory-name "Dirictory of the saved images, please: "))))

(defun my-latex-graphics-init ()
  "Add some stuffs for graphics in LaTeX."
  (interactive)
  (my-latex-use-package "graphicx")
  (my-latex-ensure-has-graphicspath)
  (--each (my-latex-current-graphicspathes) (f-mkdir (f-full it))))

(defun my-latex-add-graphicspath (path)
  "Add to list of current LaTeX file' s graphicpath PATH."
  (let ((path (s-chop-prefix (f-full default-directory) (f-full path))))
    (->>
     (my-latex-current-graphicspathes)
     (cons path)
     (my-latex-set-graphicspath))))

(defun my-latex-set-graphicspath (paths)
  "Set graphicpath (command \\graphicspath)of current LaTeX buffer to PATHS."
  (save-excursion
    (my-latex-goto-graphicspath)
    (forward-char -1)
    (sp-get
        (sp-get-enclosing-sexp)
      (delete-region :beg-in :end-in)
      (--each paths (insert "{" it "}")))))

(defun my-latex-current-graphicspathes ()
  "Parse from current LaTeX buffer value of \\graphicspath."
  (save-excursion
    (my-latex-goto-graphicspath)
    (forward-char -1)
    (sp-get (sp-get-enclosing-sexp)
      (->>
       (buffer-substring-no-properties :beg-in :end-in)
       (s-match-strings-all
        "{\\(.*\\)}")
       (-map '-last-item)))))

(defun my-latex-goto-graphicspath ()
  "Goto end of LaTeX command for set graphic paths, if isn't exit insert."
  (goto-char (point-min))
  (unless (search-forward-regexp "^\\\\graphicspath" nil t)
    (search-forward-regexp "\\usepackage.*{graphicspath}" nil t)
    (forward-line 1)
    (beginning-of-line)
    (my-latex-insert-command "graphicspath")
    (insert "{}")
    (newline)
    (forward-char -1))
  (end-of-line))

(defun my-latex-insert-includegraphics (filename &optional width)
  "Insert the \\includegraphics LaTeX command which include FILENAME.

If WIDTH is non-nil, then insert WIDTH as the optional argument width to the
\\includegraphics command."
  (let* ((filename filename)
         (image-graphicspath (--find
                              (s-prefix-p it filename)
                              (my-latex-current-graphicspathes)))
         (filename (if image-graphicspath
                       (s-chop-prefix (f-full image-graphicspath) filename)
                     filename)))
    (my-latex-insert-single-line-command "includegraphics" filename
                                         :optional `((width . ,width)))))

(defun my-latex-download-image-to-graphicspath (url &optional filename)
  "Download image at URL into graphicspath of current LaTeX buffer.
This file has name FILENAME.  Return nil when fail, otherwise return path of
downloaded file"
  (or filename (setq filename (my-uri-of-url url)))
  (let* ((graphicspath (-last-item (my-latex-current-graphicspathes)))
         (dest (f-join graphicspath filename)))
    (url-copy-file url dest t)
    dest))

(defun my-uri-of-url (url)
  "Get the URI of URL."
  (->>
   url
   (s-split "/")
   (-last-item)
   (s-split "?")
   (-first-item)))

(defun my-latex-use-package (package &optional options)
  "Add \\usepackage for PACKAGE with OPTIONS to the current LaTeX buffer."
  (interactive
   (list
    (my-read-string-or-nil "Please, choose package which you need use: ")))
  (unless (my-latex-used-package-p package)
    (save-excursion
      (my-latex-goto-use-package-source)
      (beginning-of-line)
      (my-latex-insert-command "usepackage" package :optional options)
      (newline)
      (run-hooks LaTeX-after-usepackage-hook))))

(defun my-latex-goto-use-package-source ()
  "Go to the place of the LaTeX source code where shoud be inserted usepackage."
  (interactive)
  (goto-char (point-min))
  (unless (search-forward-regexp "^\\\\usepackage" nil t)
    (search-forward-regexp "^\\\\begin{document}" nil t))
  (beginning-of-line))

(defun my-latex-used-package-p (package)
  "Return t, when PACKAGE was used in current LaTeX buffer."
  (-contains-p (my-latex-used-packages) package))

(defun my-latex-used-packages ()
  "Parse from current LaTeX buffer, list of used packages."
  (->>
   (buffer-string)
   (s-lines)
   (--filter (s-prefix-p "\\usepackage" it))
   (--map
    (-last-item
     (s-match "\\\\usepackage\\(\\[.*\\]\\)?{\\(.*\\)}" it)))))

(my-latex-expand-define "t" my-latex-insert-table ;nofmt
    (preamble &optional centering-p)
  "Insert the LaTeX environment table to the current buffer with PREAMBLE."
  (interactive (list
                (my-latex-read-preamble)
                (my-latex-read-centering)))
  (my-latex-insert-env "table")
  (when centering-p
    (my-latex-insert-centering))
  (my-latex-insert-tabular preamble))

(defun my-latex-read-preamble ()
  "Read a LaTeX preamble for table from the minibuffer."
  (my-read-string-or-nil "Preamble, please: "))

(my-latex-expand-define "f" my-latex-insert-figure (&optional placement)
  "Insert the LaTeX environment figure into current buffer with PLACEMENT."
  (interactive (list (my-latex-read-placement)))
  (my-latex-insert-env "figure" :optional placement))

(my-latex-expand-define "cp" my-latex-insert-caption ;nofmt
    (&optional caption-string)
  "If CAPTION-STRING is non-nil, then insert a caption for with LaTeX syntax."
  (interactive (list (my-latex-read-caption)))
  (when caption-string
    (my-latex-insert-single-line-command "caption" caption-string)))

(my-latex-expand-define "cr" my-latex-insert-center
    ()
  "Insert the LaTeX environment center."
  (interactive)
  (my-latex-insert-env "center"))

(my-latex-expand-define "tr" my-latex-insert-tabular
    (preamble)
  "Insert the latex environment tabular."
  (interactive (list (my-latex-read-preamble)))
  (my-latex-insert-env "tabular" preamble))

(my-latex-expand-define "ar" my-latex-insert-arabic
    (counter)
  "Insert the latex command arabic."
  (interactive (list (my-latex-read-counter-name)))
  (my-latex-insert-command "arabic" counter))

(my-latex-expand-define "rom" my-latex-insert-roman
    (counter)
  "Insert the latex command roman."
  (interactive (list (my-latex-read-counter-name)))
  (my-latex-insert-command "roman" counter))

(my-latex-expand-define "al" my-latex-insert-alph
    (counter)
  "Insert the latex command alph."
  (interactive (list (my-latex-read-counter-name)))
  (my-latex-insert-command "alph" counter))

(defcustom my-latex-default-counters
  '("part"
    "chapter"
    "section"
    "subsection"
    "subsubsection"
    "paragraph"
    "subparagraph"
    "page"
    "equation"
    "figure"
    "table"
    "footnote"
    "mpfootnote"
    "enumi"
    "enumii"
    "enumiii"
    "enumiv"
    "task")
  "List of the built-in LaTeX counters names."
  :type '(repeat string))

(defun my-latex-read-counter-name ()
  "Read name of a LaTeX counter from the minibuffer."
  (completing-read "Name of counter, please: "
                   (my-latex-get-counters-names)))

(defun my-latex-get-counters-names ()
  "Get list of all LaTeX counters names."
  (-concat
   my-latex-default-counters
   (my-latex-find-counters-names-in-buffer)))

(defun my-latex-find-counters-names-in-buffer ()
  "Find in the current buffer all LaTeX counters defnitions, return its names."
  (->>
   (buffer-string)
   (s-lines)
   (--keep
    (-when-let ((all name)
                (s-match "\\newcounter\\(?:\[.*?\]\\)?{\\(.*\\)}" it))
      name))))

(my-latex-expand-define "ncr" my-latex-insert-newcounter
    (counter &optional sub-counter)
  "Insert the LaTeX command newcounter with name COUNTER.

SUB-COUNTER is optional argument of that LaTeX command."
  (interactive (list
                (read-string "Name of the counter, please: ")
                (my-read-string-or-nil "Depends on counter: ")))
  (let ((my-latex-insert-at-start-arg-type 'required))
    (my-latex-insert-single-line-command "newcounter" counter
                                         :optional sub-counter)))

(my-latex-expand-define "atc" my-latex-insert-addtocounter
    (counter amount)
  "Insert the LaTeX command addtocounter with name COUNTER and AMOUNT."
  (interactive (list
                (my-latex-read-counter-name)
                (read-number "Amount for add to counter: ")))
  (my-latex-insert-command "addtocounter" counter amount))

(my-latex-expand-define "sc" my-latex-insert-setcounter
    (counter value)
  "Insert the LaTeX command setcounter with name COUNTER and VALUE."
  (interactive (list
                (my-latex-read-counter-name)
                (read-number "Value to set counter: ")))
  (my-latex-insert-command "setcounter" counter value))

(my-latex-expand-define "stc" my-latex-insert-stepcounter
    (counter)
  "Insert the LaTeX command stepcounter with name COUNTER and VALUE."
  (interactive (list (my-latex-read-counter-name)))
  (my-latex-insert-command "stepcounter" counter))

(defun my-read-string-or-nil ;nofmt
    (prompt &optional initial-input history default-value inherit-input-method)
  "Read string from the minibuffer, if the user type nothing, return nil.

Pass PROMPT, INITIAL-INPUT, HISTORY, DEFAULT-VALUE, INHERIT-INPUT-METHOD to
`read-string'"
  (let ((input
         (read-string prompt initial-input history default-value
                      inherit-input-method)))
    (unless (s-blank-p input)
      input)))

(my-latex-expand-define "au" my-latex-insert-author
    (name)
  "Insert the latex command author with NAME."
  (interactive (list (read-string "Name of the author, please: "
                                  user-full-name)))
  (my-latex-insert-single-line-command "author" name))

(my-latex-expand-define "ttl" my-latex-insert-title
    (title)
  "Insert the LaTeX command title with TITLE."
  (interactive "sTitle of the document: ")
  (my-latex-insert-single-line-command "title" title))

(my-latex-expand-define "dt" my-latex-insert-date
    (date)
  "Insert the LaTeX command date with DATE."
  (interactive "sDate of the document, please: ")
  (my-latex-insert-single-line-command "date" date))

(my-latex-expand-define "s" my-latex-insert-section
    (name &optional toc-name)
  "Insert the LaTeX command section with NAME and name for the TOC NAME-TOC."
  (interactive (list
                (read-string "Name of the new section: ")
                (read-string "Name for the TOC of the section: ")))
  (run-hooks 'LaTeX-section-hook))

(use-package cdlatex
    :ensure t
    :hook (LaTeX-mode . turn-on-cdlatex)
    :defer t
    :bind ((:map cdlatex-mode-map)
           ("<tab>" . cdlatex-tab)))

;; fields
(use-package cdlatex
    :hook ((cdlatex-tab . yas-expand)
           (cdlatex-tab . cdlatex-in-yas-field))
    :custom (cdlatex-math-modify-alist
             '((?q "\\sqrt" nil t nil nil))))

(use-package yasnippet
    :bind ((:map yas-keymap)
           ("<tab>" . yas-next-field-or-cdlatex)
           ("TAB" . yas-next-field-or-cdlatex))
    :config ;nofmt
    (defun cdlatex-in-yas-field ()
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
        (yas-next-field-or-maybe-expand))))

(use-package latex
    :major-mode-map (TeX-mode LaTeX-mode tex-mode latex-mode)
    :bind ((:map my-latex-local-map)
           ("1" . 'latex-split-block)
           ("6" . 'my-latex-mark-inside-environment-or-math)))

(defun my-latex-mark-inside-environment-or-math ()
  "If the cursor place inside of the math environment mark that."
  (interactive)
  (if (texmathp)
      (er/mark-LaTeX-math)
    (er/mark-LaTeX-inside-environment)))

(use-package laas
    :ensure t
    :hook (LaTeX-mode . laas-mode)
    :config
    (aas-set-snippets
        'laas-mode
      :cond #'texmathp
      ;; Some Physics Units
      "As" "\\mathrm{А}"
      "Vs"  "\\mathrm{В}"
      "Oms"  "\\mathrm{Ом}"
      "cls" "^\\circ C"

      ;; Some Physics Sheet
      "eqv" "\\mathrm{Экв.}"

      ;; Some Cool Symbols
      "trg" "\\triangle"
      "agl" "\\angle"
      "grd" "^\\circ"))

(use-package embrace
    :ensure t
    :hook
    ((LaTeX-mode . embrace-LaTeX-mode-hook)
     (LaTeX-mode . my-embrace-LaTeX-mode-hook)))

(defun my-embrace-LaTeX-mode-hook ()
  "My additional `embrace-LaTeX-mode-hook'."
  (interactive)
  (setq-local embrace-show-help-p nil)
  (--each cdlatex-math-modify-alist-default
    (my-embrace-add-paren-of-cdlatex-math it))
  (my-embrace-add-paren-latex-command ?a "answer")
  (embrace-add-pair-regexp
   ?\\
   (rx "\\" (1+ wordchar) (* space) (? "[" (*? any) "]" (1+ space)) "{")
   "}"
   'my-embrace-with-latex-command
   (embrace-build-help "\\name{" "}"))
  (embrace-add-pair-regexp
   ?d
   "\\\\left."
   "\\\\right."
   'my-embrace-with-latex-left-right
   (embrace-build-help "\\name{" "}"))
  (embrace-add-pair-regexp
   ?e
   "\\\\begin{\\(.*?\\)}\\(\\[.*?\\]\\)*"
   "\\\\end{\\(.*?\\)}"
   'my-embrace-with-latex-env
   (embrace-build-help "\\begin{name}" "\\end{name}")
   t))

(defun my-embrace-add-paren-of-cdlatex-math (element)
  "Add an ELEMENT of the `cdlatex-math-modify-alist' to the `embrace' parens."
  (let* ((key (-first-item element))
         (cmd (and (-third-item element)
                   (s-chop-prefix "\\" (-third-item element))))
         (type (-fourth-item element)))
    (when cmd
      (if type
          (my-embrace-add-paren-latex-command key cmd)
        (my-embrace-add-paren-latex-style-command key cmd)))))

(defun my-embrace-add-paren-latex-command (key name)
  "Add paren at KEY for the LaTeX command with NAME in `embrace'."
  (embrace-add-pair-regexp
   key
   (my-latex-command-left-paren-regexp name)
   "}"
   (-const (cons (my-latex-command-left-paren name) "}"))
   (embrace-build-help (my-latex-command-left-paren name) "}")))

(defun my-latex-command-left-paren (name)
  "Return paren right of the LaTeX command named NAME."
  (s-concat "\\" name "{"))

(defun my-latex-command-left-paren-regexp (name)
  (rx "\\"
      (literal name)
      (* space)
      (? "[" (*? any) "]" (* space))
      "{"))

(defun my-embrace-add-paren-latex-style-command (key name)
  "Add paren at KEY for the style LaTeX command with NAME in `embrace'."
  (embrace-add-pair-regexp
   key
   (my-latex-style-command-left-paren-regexp name)
   "}"
   (-const (cons (my-latex-style-command-left-paren name) "}"))
   (embrace-build-help (my-latex-style-command-left-paren name) "}")))

(defun my-latex-style-command-left-paren (name)
  "Return paren right of the LaTeX command named NAME."
  (s-concat "{\\" name " "))

(defun my-latex-style-command-left-paren-regexp (name)
  (rx "{" (* space) "\\" (literal name) (* space)))

(defun my-embrace-with-latex-command ()
  "Return pair from the left and right pair for a LaTeX command."
  (let ((name (read-string "Name of a LaTeX command, please: ")))
    (cons (s-concat "\\" name "{") "}")))

(defun my-embrace-with-latex-left-right ()
  "Return pair from the left and right pair for the LaTeX command \\left."
  (cons (s-concat "\\left" (read-char "Left paren, please: "))
        (s-concat "\\right" (read-char "Right paren, please: "))))

(defun my-embrace-with-latex-env ()
  "Return pair from the left and right pair for the LaTeX command \\left."
  (let ((env (read-string "Name of the environment, please: "
                          (latex-complete-envnames))))
    (cons (s-concat "\\begin{" env "}")
          (s-concat "\\end{" env "}"))))

(use-package tex-mode
    :after cdlatex
    :bind
    ((:map cdlatex-mode-map)
     ("(" .  self-insert-command)
     (")" .  self-insert-command)
     ("{" .  self-insert-command)
     ("}" .  self-insert-command)
     ("[" .  self-insert-command)
     ("]" .  self-insert-command)
     ("\"" . self-insert-command)
     ("\\" . self-insert-command)))

(defun my-latex-dollar ()
  "Insert dollars and turn input method into English."
  (interactive)
                                        ; when current-input-method isn standard
  (if (not current-input-method)
                                        ; then
      (insert ";")
                                        ; else
    (toggle-input-method)
    (if (use-region-p)
        (sp-wrap-with-pair "$")
      (sp-insert-pair "$")
      (forward-char -1))))

(use-package cdlatex
    :ensure t
    :bind
    (:map cdlatex-mode-map)
    (";" . my-latex-dollar))

(sp-with-modes
    '(tex-mode plain-tex-mode latex-mode LaTeX-mode)
  (sp-local-pair " ``"
                 "''"
                 :trigger "\""
                 :unless '(sp-latex-point-after-backslash sp-in-math-p)
                 :post-handlers '(sp-latex-skip-double-quote))

  (sp-local-pair " \"<"
                 "\">"
                 :trigger "<"
                 :unless '(sp-latex-point-after-backslash sp-in-math-p)))

(use-package org-table)

(my-latex-expand-define "ot" my-latex-insert-orgtbl-table
    (name)
  "Insert table with `orgtbl-mode'.

It's insert two comments line and the LaTeX environment comment in which
you will edit your table in the `org-mode' syntax.  After you should press
C-c C-c and the source code will be replaced.

Each table should has certain NAME, so inserted table will has name NAME"
  (interactive "sName of the table, please: ")
  (let* ((e (cl-assoc-if #'derived-mode-p orgtbl-radio-table-templates))
         (txt (nth 1 e))
         name pos)
    (unless e (user-error "No radio table setup defined for %s" major-mode))
    (while (string-match "%n" txt)
      (setq txt (replace-match name t t txt)))
    (or (bolp) (insert "\n"))
    (setq pos (point))
    (insert txt)
    (goto-char pos)))

(eval
 `(my-use-autoformat-in-mode
   'LaTeX-mode
   ,@(append '(latex-capitalize-special-commands
               latex-expand-to-list-item)
             my-autoformat-all-functions)))

(defvar autoformat-latex-capitalize-latex-commands
  '("author" "title" "date" "part" "subsection" "section" "part" "chapter")
  "List of regexps which Emacs will automatically capitalize.")

(defvar autoformat-latex-capitalize-regexps
  (--map
   (s-concat "\\\\" it "\\W*{.")
   autoformat-latex-capitalize-latex-commands)
  "List of regexps which Emacs will automatically capitalize.")

(add-to-list 'autoformat-latex-capitalize-regexps "\\\\item\\W+.")

(defun autoformat-latex-capitalize-special-commands ()
  "Capitalize last symbol, when its match on special regexp."
  (interactive)
  (when (-any #'looking-back autoformat-latex-capitalize-regexps)
    (undo-boundary)
    (capitalize-word -1)))

(defun autoformat-latex-expand-to-list-item ()
  "Try expand fragments sush as 1. or - to LaTeX list items."
  (cond
    ((autoformat-latex-expand-to-enumerate-list-item-p)
     (autoformat-latex-expand-to-enumerate-list-item))
    ((autoformat-latex-expand-to-itemized-list-item-p)
     (autoformat-latex-expand-to-itemized-list-item))))

(defcustom autoformat-latex-enumerate-list-items-triggers
  '("[0-9]*\\. ")
  "List of regepxs which should be expanded to LaTeX enumerate list item.

Will be expanded only on matching in empty line and not in math"
  :type '(repeat string))

(defcustom autoformat-latex-itemized-list-items-triggers
  '("- "
    "\\* ")
  "List of regepxs which should be expanded to LaTeX itemized list item.

Will be expanded only on matching in empty line and not in math"
  :type '(repeat string))

(defun autoformat-latex-expand-to-enumerate-list-item-p ()
  "Get t, when autoformat should expand text to the enumerate LaTeX list."
  (my-one-of-regexps-looking-back-on-bol
   autoformat-latex-enumerate-list-items-triggers))

(defun autoformat-latex-expand-to-itemized-list-item-p ()
  "Get t, when autoformat should expand text to the itemized LaTeX list."
  (my-one-of-regexps-looking-back-on-bol
   autoformat-latex-itemized-list-items-triggers))

(defun my-one-of-regexps-looking-back-on-bol (regexps)
  "Get t, when one of REGEXPS matchs with text from current point to bol."
  (->>
   regexps
   (--map (concat "^ *" it))
   (-some 'looking-back)))

(defun autoformat-latex-expand-to-enumerate-list-item ()
  "Expand, for example, 1. to the LaTeX enumerate list item."
  (clear-current-line)
  (if (string-equal (LaTeX-current-environment) "enumerate")
      (LaTeX-insert-item)
    (LaTeX-env-item "enumerate")))

(defun autoformat-latex-expand-to-itemized-list-item ()
  "Expand, for example, 1. to the LaTeX itemized list item."
  (clear-current-line)
  (if (string-equal (LaTeX-current-environment) "itemize")
      (LaTeX-insert-item)
    (LaTeX-env-item "itemize")))

(defun my-latex-equation-to-split ()
  "Transform LaTeX equation environment to split environment."
  (interactive)
  (save-excursion
    (my-latex-wrap-environment
     (my-latex-env-beg)
     (my-latex-env-end)
     "split")
    (replace-string "=" "&=" nil
                    (my-latex-env-beg)
                    (my-latex-env-end))
    (just-for-each-line*
        (my-latex-env-beg)
        (my-latex-env-end)
      (when (just-call-on-next-line* (just-line-prefix-p "&=" nil t))
        (end-of-line)
        (insert "\\\\")))))

(defun my-latex-env-beg-and-end ()
  "Return as cons beginning and end of current LaTeX environment."
  (save-excursion
    (LaTeX-find-matching-begin)
    (end-of-line)
    (forward-char)
    (push-mark nil nil t)
    (LaTeX-find-matching-end)
    (beginning-of-line)
    (forward-char -1)
    (cons (region-beginning) (region-end))))

(defun my-latex-env-beg ()
  "Return point at beginning of current LaTeX environment."
  (car (my-latex-env-beg-and-end)))

(defun my-latex-env-end ()
  "Return point at end of current LaTeX environment."
  (cdr (my-latex-env-beg-and-end)))

(defun my-latex-env-beg ()
  "Return point at beginning of current LaTeX environment."
  (car (my-latex-env-beg-and-end)))

(defun my-latex-wrap-environment (beg end environment)
  "Wrap the region from BEG to END into ENVIRONMENT.

If the environment is not given, ask for it using completion."
  (just-mark-region beg end)
  (cdlatex-wrap-environment environment)
  (indent-region (region-beginning) (region-end)))

(bind-keys*
 :map my-latex-local-map
 ("\\" . my-latex-equation-to-split))

(defun my-latex-insert-image (filename  ;nofmt
                              &optional caption placement width centering)
  "Insert image with FILENAME with LaTeX syntax into current buffer.

By default, caption won't be inserted, but if CAPTION is a string image will
has caption CAPTION.

Image will be inserted with LaTeX environment figure and command
\\includegraphics, so you can change PLACEMENT and WIDTH of the inserted
image, PLACEMENT defaults to \"h\", WIDTH defaults to the original image
width.

If CENTERING is non-nil, then image will be centered via \\centering"
  (interactive (my--get-arguments-for-latex-insert-image))
  (my-latex-graphics-init)
  (my-latex-insert-figure placement)
  (when centering ;nofmt
    (my-latex-insert-centering))
  (my-latex-insert-includegraphics filename width)
  (my-latex-insert-caption caption))

(defun my-latex-insert-centering ()
  "Insert the LaTeX command \"centering\"."
  (interactive)
  (my-latex-insert-single-line-command "centering"))

(defun my--get-arguments-for-latex-insert-image ()
  "Read from the minibuffer arguments for the `my-latex-insert-image'."
  (my-latex-graphics-init)
  (list
   (my-latex-read-image-path-from-graphicspath)
   (my-latex-read-caption)
   (my-latex-read-width)
   (my-latex-read-placement)
   (my-latex-read-centering)))

(defun my-latex-read-image-path-from-graphicspath ()
  "Read from the minibuffer path to an image from the graphicspath."
  (let ((graphicspath (f-full (-last-item (my-latex-current-graphicspathes)))))
    (->>
     (read-file-name "Filename of image, please: " graphicspath)
     (s-chop-prefix graphicspath))))

(defun my-latex-read-caption ()
  "Read from the minibuffer a caption for the LaTeX.

  If the user typed nothing, then return nil"
  (let ((typed (read-string "Caption, please: ")))
    (unless (s-blank? typed) typed)))

(defun my-latex-read-width ()
  "Read a LaTeX width from the minibuffer.

  If the user typed nothing, then return nil"
  (let ((typed (read-string "Width, please: ")))
    (unless (s-blank? typed) typed)))

(defun my-latex-read-placement ()
  "Read a LaTeX placement from the minibuffer.

  If the user typed nothing, then return nil"
  (let ((typed
         (read-string "Placement, please (some chars of h t b p ! H): ")))
    (unless (s-blank? typed) typed)))

(defun my-latex-read-centering ()
  "Return t, when the user need to centering of the LaTeX block."
  (y-or-n-p "Centering or no? "))

(defun my-latex-insert-image-at-url (url
                                     &optional
                                       filename
                                       caption
                                       placement
                                       width
                                       centering)
  "Download as FILENAME and insert image at URL to the current LaTeX buffer.

Image will be inserted with CAPTION (if CAPTION isn't nil), PLACEMENT (defaults
to \"h\"), width (defaults to origin width of the image at URL) and centering
if centering is non-nil.

If call that function interactively, then URL will be readed from either
the `kill-ring', the selected region or the minibuffer"
  (interactive (my--get-arguments-for-latex-insert-image-at-url))
  (my-latex-graphics-init)
  (my-latex-download-image-to-graphicspath url filename)
  (my-latex-insert-image filename caption placement width centering))

(defun my--get-arguments-for-latex-insert-image-at-url ()
  "Get arguments for `my-latex-insert-image-at-url'."
  (let ((url (my-read-image-url)))
    (list
     url
     (my-latex-read-new-filename-of-image-at-url url)
     (my-latex-read-caption)
     (my-latex-read-placement)
     (my-latex-read-width)
     (my-latex-read-centering))))

(defun my-latex-read-new-filename-of-image-at-url (url)
  "Read from the minibuffer new filename of the downloaded image at URL."
  (read-string "New filename of the downloaded image: "
               (my-uri-of-url url)))

(bind-keys
 :map my-latex-local-map
 ("i" . my-latex-insert-image)
 ("u" . my-latex-insert-image-at-url))

(use-package tex-mode
    :ensure t
    :bind ((:map my-latex-local-map)
           ("x" . 'my-latex-kill-section)))

(defun my-latex-kill-section ()
  "Kill a LaTeX section."
  (interactive)
  (LaTeX-mark-section)
  (kill-region (region-beginning) (region-end)))

(setq latex-documentclasses
      '("article" "reoport" "book" "proc" "minimal" "slides" "memoir" "letter" "beamer"))

(dolist (mode (list 'TeX-mode-hook
                    'tex-mode-hook
                    'latex-mode-hook
                    'LaTeX-mode-hook))
  (add-hook mode (lambda () (call-interactively 'visual-fill))))

(use-package latex-extra
    :ensure t
    :hook ((LaTeX-mode . latex-extra-mode)
           (LaTeX-mode . auto-fill-mode))
    :bind ((:map my-latex-local-map)
           ("e" . 'latex/compile-commands-until-done)
           ("l" . 'latex/next-section-same-level)
           ("j" . 'latex/previous-section-same-level)))

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

(defcustom my-latex-math-spaces-binary-ops
  '("\\+" "-" "\\\\cdot" "\\\\times" "=")
  "List of the regexps which indicates an binary operator of the LaTeX math.

Binary operator is a LaTeX command which no take arguments and needs to 2
one left number/expression and one rigth number/expression.  So, \\frac isn't
binary operator, because it takes arguments"
  :type '(repeat regexp))

(defcustom my-latex-math-spaces-parens
  '("(" ")" "\\\\left." "\\\\right." "|")
  "List of the regexps which indicates an paren of the LaTeX math.

In that list openning and closing parens should be added separately."
  :type '(repeat string))

(defvar my-latex-math-spaces-do-hook nil
  "Hooks which will be run when called `my-latex-math-spaces-do'.")

(define-minor-mode my-latex-math-spaces-mode
    "Minor mode which automatically insert spaces in the LaTeX math."
  :init-value t
  (if (not my-latex-math-spaces-mode)
      (remove-hook 'post-self-insert-hook 'my-latex-math-spaces-do)
    (add-hook 'post-self-insert-hook 'my-latex-math-spaces-do)))

(defun my-latex-math-spaces-do ()
  "Do insertion of the spaces for the LaTeX math syntax, if needed."
  (interactive)
  (when (and (eq major-mode 'latex-mode) (texmathp))
    (run-hooks 'my-latex-math-spaces-do-hook)))

(defun my-latex-math-spaces-for-binary-ops ()
  "Do insertion of the spaces for the last LaTeX binary operation, if needed."
  (let ((start (point)))
    (when (my-latex-math-spaces-goto-binary-op-start)
      (just-spaces-to-1)
      (goto-char start)
      (my-latex-math-spaces-goto-binary-op-end)
      (just-spaces-to-1))))

(add-hook 'my-latex-math-spaces-do-hook 'my-latex-math-spaces-for-binary-ops)

(defun my-latex-math-spaces-goto-binary-op-start ()
  "Go to backward binary operation start when its looking back."
  (let ((init (point))
        binary-op-start)
    (skip-chars-backward " ")
    (setq binary-op-start
          (-any 'my-latex-math-spaces-skip-backward
                my-latex-math-spaces-binary-ops))
    (if binary-op-start
        binary-op-start
      (goto-char init)
      nil)))

(defun my-latex-math-spaces-goto-binary-op-end ()
  "Go to the end of the binary operation."
  (-any
   'my-latex-math-spaces-skip-forward
   my-latex-math-spaces-binary-ops))

(defun my-latex-math-spaces-skip-backward (regexp)
  "Skip REGEXP looking back, if regexps match return point, else return nil."
  (when (search-backward-regexp (s-concat regexp "\\=") nil t)
    (goto-char (match-beginning 0))
    (match-string 0)))

(defun my-latex-math-spaces-skip-forward (regexp)
  "Skip REGEXP looking back, if regexps match return point, else return nil."
  (search-forward-regexp (s-concat "\\=" regexp) nil t))

(use-package tex
    :ensure auctex
    :hook (LaTeX-mode . my-latex-math-spaces-mode))

(use-package org
    :major-mode-map (org-mode)
    :bind
    ((:map xah-fly-command-map)
     ("SPC z" . 'org-capture)
     (:map my-org-local-map)

     ;; Insert anything
     ("l"   . 'org-insert-link)
     ("s"   . 'org-schedule)
     ("d"   . 'org-deadline)
     ("i"   . 'my-org-insert-image)
     ("u"   . 'my-org-insert-img-at-url)

     ;; Manipulations with a subtree
     ("c"   . 'org-copy-subtree)
     ("x"   . 'org-cut-subtree)
     ("q"   . 'my-org-indent-subtree)
     ("6"   . 'org-mark-subtree)
     ("w"   . 'my-org-clear-subtree)
     ([tab] . 'org-refile)
     ("1"   . 'org-todo)
     (";"   . 'org-set-tags-command)
     ("a"   . 'org-archive-subtree)
     ("z"   . 'org-toggle-comment)

     ;; Org Babel
     ("b t" . 'org-babel-tangle)
     ("b f" . 'org-babel-tangle-file)
     ("b e" . 'org-babel-execute)

     ;; Manipulations with a table
     ("t n" . 'org-table-create-or-convert-from-region)
     ("="   . 'org-table-eval-formula)
     ("t f" . 'my-org-table-eval-formula-in-field)
     ("t i" . 'org-table-import)
     ("t e" . 'org-table-export)
     ("t g" . 'org-table-recalculate)
     ("t x" . 'org-table-kill-row)
     ("-"   . 'org-table-insert-hline)
     ("t o" . 'org-table-toggle-coordinate-overlays)
     ;; sum
     ("+"   . 'org-table-sum)
     ("t +" . 'org-table-sum)
     ("t s" . 'org-table-sum)

     ;; Export
     ("p"   . 'org-publish)
     ("e"   . 'org-export-dispatch)

     ;; Context Commands
     ("g"   . 'org-ctrl-c-ctrl-c)
     ("'"   . 'org-edit-special)
     ;; other being in the `org-mode-map' section

     ;; Miscellaneous
     ("j"   . 'org-latex-preview)
     ("SPC" . 'org-toggle-checkbox)
     ("RET" . 'org-open-at-point)
     ("r"   . 'my-org-schedule-to-today)
     ("/"   . 'org-sparse-tree)

     (:map org-mode-map)
     ;; Here continoue of the Context Commands...
     ;; M-
     ("M-j"   . 'org-metaleft)
     ("M-i"   . 'org-metaup)
     ("M-k"   . 'org-metadown)
     ("M-l"   . 'org-metaright)
     ;; M-S-
     ("M-S-j" . 'org-shiftmetaleft)
     ("M-S-i" . 'org-shiftmetaup)
     ("M-S-k" . 'org-shiftmetadown)
     ("M-S-l" . 'org-shiftmetaright)
     ;; C-S-
     ("C-S-j" . 'org-shiftcontrolleft)
     ("C-S-i" . 'org-shiftcontrolup)
     ("C-S-k" . 'org-shiftcontroldown)
     ("C-S-l" . 'org-shiftcontrolright)))

(defun my-org-clear-subtree ()
  "Kill subtree at the position, and activate insertion mode."
  (org-cut-subtree)
  (xah-fly-insert-mode-activate))

(defun my-org-table-eval-formula-in-field ()
  "Eval formula with `orgtbl-mode' syntax for the current field of the table."
  (interactive)
  (org-table-eval-formula '(4)))

(defun my-org-schedule-to-today ()
  "Scheduale a `org-mode' heading to today."
  (interactive)
  (org-schedule t (format-time-string "%Y-%m-%d")))

(defun my-org-indent-subtree ()
  "Indent current the `org-mode' subtree at current position."
  (interactive)
  (save-excursion
    (org-mark-subtree)
    (indent-region (region-beginning) (region-end))))

(defun my-org-insert-image (filename &optional caption)
  "Insert a image with FILENAME.

By default, caption for the image don't inserts, but if CAPTION is a
string, then define caption of the image to the CAPTION.

In the interactive, If the region is active, the FILENAME will be text
in the region."
  (interactive
   (list (my-org-read-image-filename) (my-org-read-image-caption)))
  (just-ensure-empty-line)
  (when caption ;nofmt
    (insert "#+CAPTION: " caption)
    (newline))
  (insert "[[" filename "]]"))

(defun my-org-read-image-filename ()
  "Read a image filename.

If the region is active, then return text in the region as filename, otherwise
return filename readed from the minibuffer."
  (or
   (just-text-in-region)
   (read-file-name "Please, choose image to insert: ")))

(defun my-org-read-image-caption ()
  "Read a image caption from the minibuffer.

If the user insert any caption, return its, otherwise return nil."
  (let ((caption (read-string "Caption for the image, please: ")))
    (unless (s-blank-p caption) caption)))

(defcustom my-org-default-images-dir "./images/"
  "Default directory for images of a `org-mode' document."
  :type 'string)

(defun my-org-insert-img-at-url (url &optional new-file-name images-dir caption)
  "Insert org image at URL, download it into IMAGES-DIR with name NEW-FILE-NAME.

If the region is active return it, otherwise read URL from the minibuffer.
If caption isn't empty string, then insert image with the caption CAPTION."
  (interactive
   (my--get-arguments-for-org-insert-img-at-url))
  (or images-dir (setq images-dir my-org-default-images-dir))
  (let ((new-filename (f-join images-dir new-file-name)))
    (my-download url new-filename)
    (my-org-insert-image new-filename caption)))

(defun my--get-arguments-for-org-insert-img-at-url ()
  "Get arguments from the user for `my-org-insert-img-at-url'."
  (let* ((url (my-read-image-url))
         (new-file-name (my-org-read-new-image-at-url-file-name url))
         (images-dir (my-org-read-images-dir))
         (caption (my-org-read-image-caption)))
    (list url new-file-name images-dir caption)))

(defun my-org-read-images-dir ()
  "Read directory path for downloading of the image."
  (read-directory-name "Image will download into directory:"
                       my-org-default-images-dir))

(defun my-org-read-new-image-at-url-file-name (url)
  "Read from the minibuffer new file name for the image at URL."
  (read-string "Image will be downloaded with name: "
               (my-uri-of-url url)))

(defun my-download (url new-filename)
  "Download file at URL as file with NEW-FILENAME."
  (make-directory (f-dirname new-filename) t)
  (url-copy-file url new-filename t))

(add-hook 'org-mode-hook (lambda () (call-interactively 'visual-fill)))

(add-hook 'org-mode-hook 'aas-activate-for-major-mode)

(aas-set-snippets 'org-mode
  "exthe" "explore the"
  "misc " "miscellaneous"
  "Iau" "I am use")

(my-use-autoformat-in-mode
 'org-mode
 org-sentence-capitalization)

(defvar autoformat-org-non-text-line-prefix
  (rx (or (1+ "*")                      ; heading
          (seq (0+ " ")
               "-"))                  ; itemized list item
      (1+ " ")))

(defvar autoformat-org-line-for-capitalization-regexp
  (rx line-start
      (optional
       (regexp autoformat-org-non-text-line-prefix)
       (optional (or "TODO" "DONE") (1+ " ")))
      letter)
  "Regular expression indicates the necessary of the last word capitalization.")

(defun autoformat-org-sentence-capitalization ()
  "Capitalize first letter of a sentence in the `org-mode'."
  (interactive)
  (autoformat-sentence-capitalization)
  (when (and
         (autoformat-org-at-non-text-line-p)
         (looking-back autoformat-org-line-for-capitalization-regexp))
    (undo-boundary)
    (capitalize-word -1)))

(defun autoformat-org-at-non-text-line-p ()
  "Return t, when the cursor being at non-text position for `org-mode'."
  (or
   (just-line-regexp-prefix-p autoformat-org-non-text-line-prefix)
   (just-call-on-prev-line*
    (just-line-regexp-prefix-p autoformat-org-non-text-line-prefix))))

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

(use-package org-ql
    :ensure t
    :config
    (require 'org-ql))

(use-package helm-org-ql
    :ensure t)

(defun fast-exec-org-ql-keys ()
  "Get some useful keymaps of  `fast-exec' for org-ql."
  (fast-exec/some-commands
   ("Search Org Files via Org Query Language" 'org-ql-search)))

(fast-exec/register-keymap-func 'fast-exec-org-ql-keys)
(fast-exec/reload-functions-chain)

(use-package org-cliplink
    :ensure t
    :bind
    ((:map my-org-local-map)
     ("i" . org-cliplink)))

(use-package toc-org
    :ensure t
    :hook (org-mode . toc-org-mode)
    :custom
    (toc-org-max-depth 4))

(use-package org
    :custom
  (org-startup-folded t)
  (org-startup-indented t)
  (org-startup-with-inline-images t))

(defun turn-on-org-cdlatex-mode ()
  "Turn on `org-cdlatex-mode'."
  (interactive)
  (org-cdlatex-mode t))

(add-hook 'org-mode-hook #'turn-on-org-cdlatex-mode)

(defun my-org-remove-empty-property-drawers ()
  "Remove all empty property drawers in current file."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward ":PROPERTIES:" nil t)
      (save-excursion
        (org-remove-empty-drawer-at (match-beginning 0))))))

(defun my-org-remove-redundant-tags ()
  "Remove redundant tags of headlines in current buffer.

A tag is considered redundant if it is local to a headline and
inherited by a parent headline.

Thanks to David Maus!"
  (interactive)
  (when (eq major-mode 'org-mode)
    (save-excursion
      (org-map-entries
       (lambda ()
         (let ((alltags (split-string (or (org-entry-get (point) "ALLTAGS") "") ":"))
               local inherited tag)
           (dolist (tag alltags)
             (if (get-text-property 0 'inherited tag)
                 (push tag inherited) (push tag local)))
           (dolist (tag local)
             (if (member tag inherited) (org-toggle-tag tag 'off)))))
       t nil))))

(defun my-org-check-misformatted-subtree ()
  "Check misformatted entries in the current buffer."
  (interactive)
  (show-all)
  (org-map-entries
   (lambda ()
     (when (and (move-beginning-of-line 2)
                (not (looking-at org-heading-regexp)))
       (if (or (and (org-get-scheduled-time (point))
                    (not (looking-at (concat "^.*" org-scheduled-regexp))))
               (and (org-get-deadline-time (point))
                    (not (looking-at (concat "^.*" org-deadline-regexp)))))
           (when (y-or-n-p "Fix this subtree? ")
             (message "Call the function again when you're done fixing this subtree.")
             (recursive-edit))
         (message "All subtrees checked."))))))

(defun my-org-fix-blank-lines (&optional prefix)
  "Ensure that Blank lines exist between headings and between headings and their contents.
With prefix, operate on whole buffer. Ensures that blank lines
exist after each headings's drawers."
  (interactive "P")
  (org-map-entries
   (lambda ()
     (org-with-wide-buffer
      ;; `org-map-entries' narrows the buffer, which prevents us from seeing
      ;; newlines before the current heading, so we do this part widened.
      (while (not (looking-back "\n\n" nil))
        ;; Insert blank lines before heading.
        (insert "\n")))
     (let ((end (org-entry-end-position)))
       ;; Insert blank lines before entry content
       (forward-line)
       (while (and (org-at-planning-p)
                   (< (point) (point-max)))
         ;; Skip planning lines
         (forward-line))
       (while (re-search-forward org-drawer-regexp end t)
         ;; Skip drawers. You might think that `org-at-drawer-p' would suffice, but
         ;; for some reason it doesn't work correctly when operating on hidden text.
         ;; This works, taken from `org-agenda-get-some-entry-text'.
         (re-search-forward "^[ \t]*:END:.*\n?" end t)
         (goto-char (match-end 0)))
       (unless (or (= (point) (point-max))
                   (org-at-heading-p)
                   (looking-at-p "\n"))
         (insert "\n"))))
   t
   (if prefix
       nil
     'tree)))

(defun my-org-align-all-tables ()
  (interactive)
  (org-table-map-tables 'org-table-align 'quietly))

(defun my-org-tidy ()
  "Use each of rules tidy org."
  (interactive)
  ;; Clean up metadata
  (my-org-remove-redundant-tags)
  (my-org-remove-empty-property-drawers)
  (my-org-check-misformatted-subtree)
  ;; Repair other elements of org-mode
  (my-org-fix-blank-lines t)
  (my-org-align-all-tables))

(use-package org
    :bind
  ((:map my-org-local-map)
   ("k" . 'my-org-tidy)))

(use-package elisp-mode
    :major-mode-map elisp (emacs-lisp-mode inferior-emacs-lisp-mode))

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
    (unless (just-line-is-whitespaces-p)
      (newline-and-indent))
    (yas-expand-snippet
     (format "(${1:name} :initarg :$1 :accessor %s-$1)"
             (my-elisp-defclass-name)))))

(define-key emacs-lisp-mode-map (kbd "M-RET") 'my-elisp-new-field-of-class)

(use-package paxedit
    :ensure t
    :hook (emacs-lisp-mode . paxedit-mode)
    :bind ((:map paxedit-mode-map)
           (";" . 'paxedit-insert-semicolon)
           ("(" . 'paxedit-open-round)
           ("[" . 'paxedit-open-bracket)
           ("{" . 'paxedit-open-curly)
           (:map my-elisp-local-map)
           ("o" . 'paxedit-transpose-forward)
           ("u" . 'paxedit-transpose-backward)
           ("u" . 'paxedit-transpose-backward)
           ("x" . 'paxedit-kill)
           ("w" . 'my-paxedit-change)
           ("d" . 'paxedit-symbol-kill)
           ("q" . 'paxedit-compress)
           ("k" . 'paxedit-delete-whitespace)
           ("y" . 'my-paxedit-duplicate)))

(defun my-paxedit-change ()
  "Kill the Lisp expression at the cursor and activate insert mode."
  (interactive)
  (paxedit-delete)
  (xah-fly-insert-mode-activate))

(defun my-paxedit-duplicate ()
  "Make copy of the Lisp expression at the cursor."
  (interactive)
  (let* ((reg (paxedit-sexp-region))
         (beg (car reg))
         (end (cdr reg))
         (sexp (buffer-substring beg end))
         (sep (if (my-lisp-sexp-whole-line-p) "\n" " ")))
    (goto-char end)
    (insert sep sexp)))

(defun my-lisp-sexp-whole-line-p ()
  "Return t, when the Lisp sexp at the point being at whole of line."
  (interactive "P")
  (and (= beg (save-excursion (beginning-of-line-text)
                              (point)))
       (= end (point-at-eol))))

(use-package racket-mode
    :ensure t
    :defer t
    :config

    ;; this is enable some useful functions
    (add-hook 'racket-mode-hook #'racket-xp-mode)

    ;; `flycheck' is very slow, and
    ;;`racket-xp-mode' also highlight errors, so i disable `flycheck' for
    ;; the Racket
    (add-hook 'racket-mode-hook #'turn-off-flycheck)

    ;; fix a bug
    (setq racket-xp-mode-hook nil))

(defcustom my-racket-meta-return-functions
  '()
  "List of functions for M-ret in racket.
Each function should return t, if it should be called and should stop next
calls of functions."
  :type '(repeat function))

(defun my-racket-meta-return ()
  "Try use one of M-ret functions for racket.
Depends on `my-racket-meta-return-functions'."
  (interactive)
  (unless (-find #'funcall my-racket-meta-return-functions)
    (message "Sorry, function not found!")))

(with-eval-after-load 'racket-mode
  (define-key racket-mode-map (kbd "M-RET") 'my-racket-meta-return))

(defun my-racket-meta-return-let ()
  "Add a binding to the let expression of the Racket.
One of `my-racket-meta-return-functions'"
  (when (my-in-lisp-sexp-p "let")
    (my-goto-lisp-sexp-begin "let")
    (search-forward "(." nil t)
    (sp-get (sp-get-sexp)
      (goto-char :end-in)
      (newline-and-indent)
      (insert "[]")
      (forward-char -1)
      t)))

(add-to-list 'my-racket-meta-return-functions #'my-racket-meta-return-let)

(defun my-racket-meta-return-test-case ()
  "Add a test case to current test module in racket.
One of `my-racket-meta-return-functions'"
  (when (my-in-lisp-sexp-p "module\+\\W*test")
    (my-goto-lisp-sexp-begin "module\+\\W*test")
    (forward-char -1)
    (sp-get (sp-get-sexp) (goto-char :end-in))
    (newline-and-indent)
    (insert "(check-equal? )")
    (forward-char -1)
    t))

(add-to-list 'my-racket-meta-return-functions #'my-racket-meta-return-test-case)

(defcustom my-racket-meta-return-cond-clauses-expression-names
  '("cond" "match" "define/match")
  "List of the racket expressions names in which should work `M-ret' function."
  :type '(repeat string))

(defun my-racket-meta-return-cond-clauses ()
  "Add new clause to racket expression which has syntax like on `cond'.

One of `my-racket-meta-return-functions'.

List of racket expressions in which this function should work:

- `cond'
- `match'
- `define/match'"
  (interactive)
  (--when-let (-find
               #'my-in-lisp-sexp-p
               my-racket-meta-return-cond-clauses-expression-names)
    (my-goto-lisp-sexp-begin it)
    (forward-char -1)
    (forward-sexp)
    (forward-char -1)
    (newline-and-indent)
    (insert "[]")
    (forward-char -1)
    t))

(add-to-list
 'my-racket-meta-return-functions
 'my-racket-meta-return-cond-clauses)

(defun my-racket-meta-return-contracted ()
  "Add new argument form to the expression of the Racket `contracted'."
  (interactive)
  (when (my-in-lisp-sexp-p "contracted")
    (my-goto-lisp-sexp-end "contracted")
    (newline)
    (insert "[]")
    (my-mark-lisp-sexp-inner "contracted")
    (align-regexp
     (region-beginning)
     (region-end)
     "\\[[^ ]+ *\\( \\)[^ ]")
    (beginning-of-line-text)
    (forward-char 1)
    t))

(add-to-list 'my-racket-meta-return-functions
             #'my-racket-meta-return-contracted)

(use-package scribble-mode
    :ensure t)

(my-use-all-autoformat-in-mode 'scribble-mode)

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

(my-also-use-autoformat-in-mode 'markdown-mode
                                markdown-capitalize-heading-line)
(my-also-use-autoformat-in-mode 'gfm-mode
                                markdown-capitalize-heading-line)

(defun autoformat-markdown-capitalize-heading-line ()
  "Capitalize first letter of a heading line (lines which started with #)."
  (when (and (just-line-prefix-p "#") (my-markdown-first-letter-of-heading))
    (undo-boundary)
    (capitalize-word -1)))

(defun my-markdown-first-letter-of-heading ()
  "Get t, when backward character is first letter of current markdown heading."
  (save-excursion
    (forward-char -1)
    (skip-chars-backward " ")
    (skip-chars-backward "#")
    (bolp)))

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
    :major-mode-map html (html-mode web-mode web-mode-prog-mode mhtml-mode)
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

(use-package impatient-mode
    :ensure t
    :bind ((:map my-html-local-map)
           ("e" . 'my-enable-impatient-mode)))

(defun my-enable-impatient-mode ()
  "Enable `impatient-mode' and open the page of current browser in web browser."
  (interactive)
  (impatient-mode +1)
  (->>
   (buffer-name)
   (s-prepend "http://localhost:8080/imp/live/")
   (browse-url)))

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
    (helm-M-x-fuzzy-match        t)
    (helm-buffers-fuzzy-matching t)
    (helm-recentf-fuzzy-match    t)
    (helm-imenu-fuzzy-match      t)
    (helm-autoresize-min-height 20)
    (helm-left-margin-width 2)
    (helm-buffers-left-margin-width 2)
    :bind (("C-h a"     . 'helm-apropos)
           (:map xah-fly-command-map)
           ("SPC SPC f" . 'helm-find-files)
           ("SPC k r"   . 'helm-regexp))

    :init
    (helm-autoresize-mode +1)
    (helm-mode +1)
    ;; this fix one bug
    (defvar helm-completion-style nil))

(use-package helm-ext
    :ensure t
    :config
    (helm-ext-ff-enable-skipping-dots +1)
    (helm-ext-ff-enable-auto-path-expansion t))

(use-package command-log-mode
    :ensure t)

(add-hook 'prog-mode-hook 'whitespace-mode)

(add-hook 'change-major-mode-hook 'visual-line-mode)

(defcustom my-aggresive-fill-paragraph-modes
  '(org-mode
    markdown-mode)
  "List of major modes in which `aggressive-fill-paragraph' should work."
  :type '(repeat symbol))

(use-package aggressive-fill-paragraph
    :ensure t
    :config
    (--each my-aggresive-fill-paragraph-modes
      (add-hook
       (->>
        it
        (symbol-name)
        (s-append "-hook")
        (intern))
       #'aggressive-fill-paragraph-mode)))

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

(setq buffer-file-coding-system 'utf-8)

(use-package helm-mode-manager
    :ensure t
    :config
    (defun fast-exec-helm-mode-manager-keys ()
      "Get some useful keymaps of  `fast-exec' for helm-mode-manager."
      (fast-exec/some-commands
       ("Switch Major Mode" 'helm-switch-major-mode)
       ("Enable Minor Mode" 'helm-enable-minor-mode)
       ("Disable Minor Mode" 'helm-disable-minor-mode)))

    (fast-exec/register-keymap-func 'fast-exec-helm-mode-manager-keys)
    (fast-exec/reload-functions-chain))

(use-package cowsay
    :ensure t
    :custom
    (cowsay-directories '("~/.emacs.d/cows"))
    :config
    (cowsay-load-cows)
    (defun fast-exec-define-cowsay-keymaps ()
      "Some useful keymaps for `cowsay'/`fast-exec'."
      (fast-exec/some-commands
       ("Cow Say String..."  'cowsay-string)
       ("Cow Say Region..."  'cowsay-region)
       ("Cow Say and Insert" 'cowsay-replace-region)
       ("Load Cows"  'cowsay-load-cows)))

    (defun cowsay--prompt-for-cow (&rest _ignored)
      "Read any cow name from the minibuffer."
      (let ((default (cowsay--get-default-cow)))
        (completing-read
         "Cow: "
         cowsay-cows
         nil t
         default
         'cowsay-cow-history
         default)))

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

(defun fast-exec-helm-net-define-keys ()
  "Keymaps for `helm-net' and `fast-exec'."
  (fast-exec/some-commands
   ("Search via Google" 'helm-google-suggest)))

(fast-exec/register-keymap-func 'fast-exec-helm-net-define-keys)
(fast-exec/reload-functions-chain)

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

(defun my-films-format-as-org-heading ()
  "Format an `kinopoisk-film' readed from the minibuffer as an org entry."
  (with-temp-buffer
    (org-mode)
    (my-films-add (my-films--search-new))
    (s-concat (buffer-string) "%?")))

(defun my-films-add (film)
  "Add FILM to current org file, this file is db of films."
  (interactive (list (my-films--search-new)))
  (org-meta-return)
  (insert "MUST-SEE ")
  (insert (kinopoisk-film-original-name film))
  (my-org-set-props
   (name      . (kinopoisk-film-name film))
   (year      . (kinopoisk-film-year film))
   (slogan    . (kinopoisk-film-slogan film))
   (id        . (kinopoisk-film-id film))
   (rating    . (kinopoisk-film-rating film))
   (countries . (kinopoisk-film-countries film))))

(defmacro my-org-set-props (&rest key-and-val)
  "Set properties of org heading with keys from KEY-AND-VAL and values from it."
  (->>
   key-and-val
   (--map
    `(org-set-property ,(symbol-name (car it)) (format "%s" ,(cdr it))))
   (cons 'progn)))

(defun my-films--search-new ()
  "Search film from the Internet."
  ;; I know that instead of `flet', I can use `cl-flet', but `cl-flet'
  ;; redefine funcitons only in body
  (flet
      ((helm-kinopoisk--handle-film (film) film))
    (helm-kinopoisk-search-films)))

(defun my-films--choose-from-top ()
  "Choose one film from the Kinopoisk top."
  ;; I know that instead of `flet', I can use `cl-flet', but `cl-flet'
  ;; redefine funcitons only in body
  (flet
      ((helm-kinopoisk--handle-film (film) film))
    (call-interactively #'helm-kinopoisk-see-films-top)))

(defun my-films-list ()
  "List of films saved in films management file."
  (interactive)
  (helm
   :buffer "*saved-films*"
   :sources
   '((name       . "List of Saved Films")
     (candidates . my-films--list-candidates)
     (action     . helm-kinonpoisk-search-source))))

(defun my-films--list-candidates ()
  "Helm candidates for `my-films-list'."
  (->>
   (org-ql-query
     :from (my-all-buffers-with-ext "org")
     :where '(todo "MUST-SEE")
     :select #'my-films--from-org-heading)
   (--map
    (cons (helm-kinopoisk--format-film-for-display it) it))))

(defun my-films--from-org-heading ()
  "Parse org heading at current position to `kinopoisk-film'."
  (when (just-line-prefix-p "*")
    (kinopoisk-film
     :id (car (string-to-number (org-property-values "id")))
     :year (car (org-property-values "year"))
     :name (car (org-property-values "name")))))

(use-package recentf
    :config (recentf-mode 69) ; Lol!
    :bind ((:map xah-fly-command-map)
           ("SPC k f" . 'recentf-open-files))
    :hook ((recentf-dialog-mode) . 'xah-fly-insert-mode-activate))

(defun fast-exec-helm-colors-keys ()
  "Get some useful keymaps of  `fast-exec' for helm-colors."
  (fast-exec/some-commands ("Get Color" 'helm-colors)))

(fast-exec/register-keymap-func 'fast-exec-helm-colors-keys)
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

;; See `doom-themes' (list of themes at starting comments)

;; List of my favorite themes:
;; - `gruber-darker'
;; - `doom-monokai-classic'

(load-theme 'doom-monokai-pro t)

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

(setq flycheck-mode-line nil)

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

(use-package prog-mode
    :hook
  (LaTeX-mode . prettify-symbols-mode))

(use-package page-break-lines
    :ensure t
    :init
    (global-page-break-lines-mode 38))

(defvar my-project-files-hash (make-hash-table :test 'equal))

(defcustom my-project-gitignore-default-patterns
  '(".git/" "*.exe")
  "Patterns in .gitignore sytax style which should be ignore by default."
  :type '(repeat string))

(defun my-project-root (&optional dir)
  (or dir (setq dir default-directory))
  (->>
   projectile-known-projects
   (--filter (s-starts-with? (f-full it) (f-full dir)))
   (--max-by (> (f-depth it) (f-depth other)))))

(defun my-projectile-root-local (dir)
  "A simple wrapper around `projectile-project-root'.

Return value at `projectile-project-root' when DIR is nil, otherwise return nil"
  (unless dir
    projectile-project-root))

(defun my-projectile-project-files (root)
  "Return filenames list of the project at ROOT, with caching!."
  (let ((root (f-full root)))
    (unless (gethash root my-project-files-hash)
      (puthash root
               (my-no-cache-project-files root)
               my-project-files-hash))
    (gethash root my-project-files-hash)))

(defun my-no-cache-project-files (root)
  "Return filenames list of the project at ROOT, without caching."
  (my-files-of-root-not-match-with-regexps
   root
   (my-project-gitignore-regexps root)))

(defun my-files-of-root-not-match-with-regexps (root regexps)
  "Return files list of ROOT each of it don't match with one of REGEXPS."
  (--reduce-from
   (cond
     ((my-matches-with-one-of-p it regexps)
      (message "Ignore %s" it)
      acc)
     ((f-directory-p it)
      (append acc (my-files-of-root-not-match-with-regexps it regexps)))
     (t
      (cons it acc)))
   nil
   (f-entries root)))

(defun my-matches-with-one-of-p (str regexps)
  "Return t, when one of REGEXPS has match with STR."
  (--some (s-matches-p it str) regexps))

(defun my-project-gitignore-regexps (root)
  "Return list of regexp from .gitignore file of project at ROOT."
  (--map
   (my-regexp-from-gitignore-pattern it root)
   (my-project-gitignore-patterns root)))

(defun my-project-gitignore-patterns (root)
  "Get patterns list with syntax of .gitignore files for the project at ROOT."
  (append (my-project-specific-gitignore-patterns root)
          my-project-gitignore-default-patterns))

(defun my-project-specific-gitignore-patterns (root)
  "Parse .gitignore file of project at ROOT into list of ignored patterns."
  (--when-let (my-project-gitignore root)
    (->>
     it
     (f-read)
     (s-lines)
     (--remove (or
                (string-equal "" it)
                (my-gitignore-comment-line-p it))))))

(defun my-project-gitignore (root)
  "Return path to .gitignore file of the project at ROOT.

If the project not contains .gitignore file, then return nil"
  (let ((path (f-join root ".gitignore")))
    (when (f-exists-p path)
      path)))

(defun my-regexp-from-gitignore-pattern (regexp gitignore-root)
  "From REGEXP of .gitignore file to real Elisp regular expression.

GITIGNORE-ROOT directory is directory which contains .gitginore file."
  ;; TODO Don't ignore files in project, which has same name with ignored
  ;; directory
  (let ((gitignore-root (f-full gitignore-root)))
    (-->
     regexp
     (my-gitignore-rx-to-el it)
     (s-chop-suffix "/" it)
     (s-prepend
      (if (s-prefix-p "/" it)
          (s-chop-suffix "/" gitignore-root)
        ".*/")
      it))))

(defun my-gitignore-rx-to-el (regexp)
  "Transform REGEXP with regexp syntax as in .gitignore file to Elisp regexp."
  (->>
   regexp
   (s-replace "*" "[^/]*")
   (s-replace "\\" "")
   (s-replace "." "\\.")))

(defun my-gitignore-comment-line-p (line)
  "Return non-nil, when LINE of .gitignore file source is commented."
  (s-prefix-p "#" (s-trim line)))

(defun projectile-project-files-clear-cache (root)
  "Function `projectile-project-files' is cached, clear this cache for ROOT."
  (interactive (list (projectile-acquire-root)))
  (remhash (f-full root)
           my-project-files-hash))

(defun my-projectile-files-with-string (string directory)
  "Return a list of all files containing STRING in DIRECTORY."
  (->>
   directory
   (projectile-project-files)
   (--filter
    (s-contains-p string (f-read it)))))

(use-package projectile
    :ensure t
    :custom                             ;nofmt
    (projectile-project-search-path '("~/projects/"))
    (projectile-completion-system 'helm)
    (projectile-project-root-functions
     '(projectile-root-local my-project-root))
    (projectile-enable-caching nil)
    :init                               ;nofmt
    (projectile-mode 1))

(use-package helm-projectile
    :ensure t
    :bind
    ((:map xah-fly-command-map)
     ("SPC j" . 'helm-projectile-find-file)
     (:map helm-projectile-find-file-map)
     ("M-<f5>" . 'my-helm-projectile-find-file-update))
    :init
    (defalias 'projectile-project-files 'my-projectile-project-files)
    (defalias 'projectile-root-local 'my-projectile-root-local)
    (defalias 'project-root 'my-project-root)
    (defalias 'projectile-files-with-string 'my-projectile-files-with-string))

(defun my-helm-projectile-find-file-update ()
  "Update function for `helm-projectile-find-file'."
  (interactive)
  (projectile-project-files-clear-cache (projectile-acquire-root))
  (helm-update))

(setq helm-projectile-find-file-map
      (let ((map (copy-keymap helm-find-files-map)))
        (helm-projectile-define-key
            map
            (kbd "C-c f") #'helm-projectile-dired-files-new-action
            (kbd "C-c a") #'helm-projectile-dired-files-add-action
            (kbd "M-e") #'helm-projectile-switch-to-shell
            (kbd "M-.") #'helm-projectile-ff-etags-select-action
            (kbd "M-!")
            #'helm-projectile-find-files-eshell-command-on-file-action)
        (define-key map (kbd "<left>") #'helm-previous-source)
        (define-key map (kbd "<right>") #'helm-next-source)
        (dolist (cmd '(helm-find-files-up-one-level
                       helm-find-files-down-last-level))
          (substitute-key-definition cmd nil map))
        map))

(setq
 helm-source-projectile-files-list
 (helm-build-sync-source "Projectile files"
   :before-init-hook
   (lambda ()
     (add-hook 'helm-after-update-hook #'helm-projectile--move-to-real)
     (add-hook 'helm-cleanup-hook #'helm-projectile--remove-move-to-real))
   :candidates
   (lambda ()
     (when (projectile-project-p)
       (with-helm-current-buffer
         (let ((root (f-full (projectile-project-root))))
           (--map
            (cons (s-chop-prefix root (f-full it)) it)
            (projectile-current-project-files))))))
   :filtered-candidate-transformer
   (lambda (files _source)
     (with-helm-current-buffer
       (let* ((root (projectile-project-root))
              (file-at-root
               (file-relative-name (expand-file-name helm-pattern root))))
         (if (or (string-empty-p helm-pattern)
                 (assoc helm-pattern files))
             files
           (if (equal helm-pattern file-at-root)
               (cl-acons (helm-ff-prefix-filename helm-pattern nil t)
                         (expand-file-name helm-pattern)
                         files)
             (cl-pairlis (list (helm-ff-prefix-filename helm-pattern nil t)
                               (helm-ff-prefix-filename file-at-root nil t))
                         (list (expand-file-name helm-pattern)
                               (expand-file-name helm-pattern root))
                         files))))))
   :fuzzy-match helm-projectile-fuzzy-match
   :keymap helm-projectile-find-file-map
   :help-message 'helm-ff-help-message
   :mode-line helm-read-file-name-mode-line-string
   :action helm-projectile-file-actions
   :persistent-action #'helm-projectile-file-persistent-action
   :persistent-help "Preview file"))

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

(use-package dired
    :bind ((:map dired-mode-map)
           ("SPC"     . nil)                ; make command at space empty prefix

           ;; Navigation
           ("k"       . 'next-line)
           ("i"       . 'previous-line)
           ("n"       . 'dired-avy)
           ("SPC h"   . 'beginning-of-buffer)
           ("SPC n"   . 'end-of-buffer)
           ("'"       . 'dired-isearch-filenames)

           ;; Open file
           ("o"       . 'dired-find-file-other-window)
           ("j"       . 'my-dired-goto-parent-dir)
           ;; some other open files I define in the section "Dired Hacks: Open"

           ;; Manipulation with file(s)
           ("SPC g"   . 'my-dired-delete)
           ("SPC x"   . 'my-dired-delete-all-files)
           ("SPC y"   . 'my-dired-duplicate)
           ("f"       . 'my-dired-rename)
           ("SPC TAB" . 'my-dired-move)
           ("s"       . 'my-dired-new-file)
           ;; copy/move/paste also defines in the section "Dired Hacks: Ranger"

           ;; Mark files
           ("t"       . 'dired-mark)
           ("SPC u"   . 'dired-unmark-all-marks)
           ("SPC a"   . 'my-dired-mark-all-files)

           ;; Misc.
           ("y"       . 'dired-undo)

           ;; Key bindings which not change your commands
           ("a"       . 'xah-fly-M-x)
           (","       . 'xah-next-window-or-frame))
    :custom ((lpr-command "PDFToPrinter")) ; Command for printing file
    :config (add-hook 'dired-mode-hook 'xah-fly-insert-mode-activate))

(defun my-dired-mark-all-files ()
  "Mark all file in `dired'."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (dired-mark 1)))

(defmacro my-define-dired-command-taking-file (name args docstring &rest body)
  "Define the command from function which take a 1 argument: filename."
  (declare (indent 2))
  `(defun ,name ()
     (interactive)
     ,docstring
     (funcall
      (lambda ,args ,@body)
      (dired-get-filename))
     (revert-buffer)))

(defun my-dired-name-of-file-at-line ()
  "Get name of file at dired object at current point."
  (-last-item (s-split " " (just-text-at-line))))

(my-define-dired-command-taking-file my-dired-rename
    (from)
  "Rename file at point from FROM to other name readed from the minibuffer."
  (let ((to (my-rename-file from)))
    (revert-buffer)
    (my-dired-goto-file to)))

(defun my-dired-goto-file (file)
  "Go to line of `dired' buffer describing FILE."
  (goto-char (point-min))
  (search-forward (f-base to)))

(defun my-rename-file (file)
  "Change name of FILE to new readed from the minibuffer name.

Return new name of FILE"
  (let* ((new-name-of-file
          (read-string "New name, please: " (f-filename from)))
         (to (f-join (f-dirname from) new-name-of-file)))
    (f-move from to)
    to))

(defun my-dired-move ()
  "Move file of current directory of `dired' at the point."
  (interactive)
  (dired-do-rename))

(my-define-dired-command-taking-file my-dired-delete
    (file)
  "Delete file at dired object at current position of the cursor."
  (f-delete file))

(defun my-dired-goto-parent-dir ()
  "Navigate to parent directory of current dired directory."
  (interactive)
  (let ((parent (f-parent (dired-current-directory))))
    (kill-buffer)
    (dired parent)))

(defun my-dired-new-file (filename)
  "Create file with FILENAME in the directory which opened in the dired buffer."
  (interactive "sName of new file, please: ")
  (f-touch (f-join (dired-current-directory) filename)))

(defun my-dired-delete-all-files ()
  "Delete all files from the directory of the `dired' buffer."
  (interactive)
  (--each (f-entries (dired-current-directory))
    (f-delete it t))
  (revert-buffer))

(defun dired-avy ()
  "Version of `avy' for the `dired'."
  (interactive)
  (avy-goto-line))

(my-define-dired-command-taking-file my-dired-duplicate (filename)
  "Make copy of the file with FILENAME in the same directory."
  (f-touch
   (f-join
    (dired-current-directory)
    (read-string "Name of the filename, please: " (f-filename filename)))))

(use-package dired-filter
    :ensure t
    :init
    (define-key dired-mode-map (kbd ".") dired-filter-map))

(use-package dired-open
    :ensure t
    :bind ((:map dired-mode-map)
           ("l"   . 'dired-open-file)
           ("RET" . 'dired-open-file))
    :config
    (add-to-list 'dired-open-functions 'my-dired-open-function-pdf))

(defun my-dired-open-function-pdf ()
  "Open function for `dired-open-functions'."
  (my-try-open-pdf-file (dired-get-file-for-visit)))

(defun my-try-open-pdf-file (filename)
  "If file with FILENAME is a pdf file, then open it as pdf, other return nil."
  (when (my-pdf-file filename)
    (helm-open-file-with-default-tool filename)
    t))

(defun my-pdf-file (filename)
  "Return t, when FILENAME is path to a PDF file."
  (s-suffix-p ".pdf" filename))

(use-package dired-rainbow
    :ensure t
    :config
    (dired-rainbow-define-chmod directory "#6cb2eb" "d.*")
    (dired-rainbow-define html 'yellow2
                          ("css"
                           "less"
                           "sass"
                           "scss"
                           "htm"
                           "html"
                           "jhtm"
                           "mht"
                           "eml"
                           "mustache"
                           "xhtml"))
    (dired-rainbow-define xml "#f2d024" ("xml"
                                         "xsd"
                                         "xsl"
                                         "xslt"
                                         "wsdl"
                                         "bib"
                                         "json"
                                         "msg"
                                         "pgn"
                                         "rss"
                                         "yaml"
                                         "yml"
                                         "rdata"))
    (dired-rainbow-define document "#9561e2" ("docm"
                                              "doc"
                                              "docx"
                                              "odb"
                                              "odt"
                                              "pdb"
                                              "pdf"
                                              "ps"
                                              "rtf"
                                              "djvu"
                                              "epub"
                                              "odp"
                                              "ppt"
                                              "pptx"))
    (dired-rainbow-define markdown "yellow" ("org"
                                             "etx"
                                             "info"
                                             "markdown"
                                             "md"
                                             "mkd"
                                             "nfo"
                                             "pod"
                                             "rst"
                                             "tex"
                                             "textfile"
                                             "txt"))
    (dired-rainbow-define database "#6574cd" ("xlsx"
                                              "xls"
                                              "csv"
                                              "accdb"
                                              "db"
                                              "mdb"
                                              "sqlite"
                                              "nc"))
    (dired-rainbow-define media "#de751f" ("mp3"
                                           "mp4"
                                           "MP3"
                                           "MP4"
                                           "avi"
                                           "mpeg"
                                           "mpg"
                                           "flv"
                                           "ogg"
                                           "mov"
                                           "mid"
                                           "midi"
                                           "wav"
                                           "aiff"
                                           "flac"))
    (dired-rainbow-define image "#f66d9b" ("tiff"
                                           "tif"
                                           "cdr"
                                           "gif"
                                           "ico"
                                           "jpeg"
                                           "jpg"
                                           "png"
                                           "psd"
                                           "eps"
                                           "svg"))
    (dired-rainbow-define log "#c17d11" ("log"))
    (dired-rainbow-define shell "#f6993f" ("awk"
                                           "bash"
                                           "bat"
                                           "sed"
                                           "sh"
                                           "zsh"
                                           "vim"))
    (dired-rainbow-define interpreted "#38c172" ("py"
                                                 "ipynb"
                                                 "rb"
                                                 "pl"
                                                 "t"
                                                 "msql"
                                                 "mysql"
                                                 "pgsql"
                                                 "sql"
                                                 "r"
                                                 "clj"
                                                 "cljs"
                                                 "scala"
                                                 "js"))
    (dired-rainbow-define compiled "#4dc0b5" ("asm"
                                              "cl"
                                              "lisp"
                                              "el"
                                              "c"
                                              "h"
                                              "c++"
                                              "h++"
                                              "hpp"
                                              "hxx"
                                              "m"
                                              "cc"
                                              "cs"
                                              "cp"
                                              "cpp"
                                              "go"
                                              "f"
                                              "for"
                                              "ftn"
                                              "f90"
                                              "f95"
                                              "f03"
                                              "f08"
                                              "s"
                                              "rs"
                                              "hi"
                                              "hs"
                                              "pyc"
                                              ".java"))
    (dired-rainbow-define executable "#8cc4ff" ("exe" "msi"))
    (dired-rainbow-define compressed "#51d88a" ("7z"
                                                "zip"
                                                "bz2"
                                                "tgz"
                                                "txz"
                                                "gz"
                                                "xz"
                                                "z"
                                                "Z"
                                                "jar"
                                                "war"
                                                "ear"
                                                "rar"
                                                "sar"
                                                "xpi"
                                                "apk"
                                                "xz"
                                                "tar"))
    (dired-rainbow-define packaged "#faad63" ("deb"
                                              "rpm"
                                              "apk"
                                              "jad"
                                              "jar"
                                              "cab"
                                              "pak"
                                              "pk3"
                                              "vdf"
                                              "vpk"
                                              "bsp"))
    (dired-rainbow-define encrypted "#ffed4a" ("gpg"
                                               "pgp"
                                               "asc"
                                               "bfe"
                                               "enc"
                                               "signature"
                                               "sig"
                                               "p12"
                                               "pem"))
    (dired-rainbow-define fonts "#6cb2eb" ("afm"
                                           "fon"
                                           "fnt"
                                           "pfb"
                                           "pfm"
                                           "ttf"
                                           "otf"))
    (dired-rainbow-define
     partition
     "#e3342f"
     ("dmg"
      "iso"
      "bin"
      "nrg"
      "qcow"
      "toast"
      "vcd"
      "vmdk"
      "bak"))
    (dired-rainbow-define vc "#0074d9"
                          ("git" "gitignore" "gitattributes" "gitmodules")))

(use-package dired-ranger
    :ensure t
    :bind ((:map dired-mode-map)
           ("m" . 'dired-ranger-move)
           ("v" . 'dired-ranger-paste)
           ("c" . 'dired-ranger-copy)))

(use-package dired-collapse
    :ensure t
    :hook (dired-mode . dired-collapse-mode))

(use-package quickrun
    :ensure t
    :bind (("S-<f5>" . quickrun))      ; in section of config *Run
                                        ; Command, I rebind this key
                                        ; binding, I'm not delete this
                                        ; bind, because if section wasn't
                                        ; load, then all will work normally
    )

(use-package run-command
    :ensure t
    :custom
    (run-command-completion-method 'helm)

    :bind ((:map xah-fly-command-map)
           ("SPC , c" . 'run-command)
           ("S-<f5>"  . 'my-run-last-command))

    :config
    (defun run-command--run (command-spec)
      "Run `COMMAND-SPEC'.  Back end for helm and ivy actions."
      (message "ok?")
      (setq-local run-command-last-recipe command-spec)
      (eval (run-command--from-spec command-spec)))

    (defun my-run-last-command ()
      "Run command which was runned last, if commands wasn't run, then `quickrun'."
      (interactive)
      (if run-command-last-recipe
          (run-command--run run-command-last-recipe)
        (message "NOT FOUND!")))

    (defvar run-command-last-recipe nil
      "Last runned recipe of `run-command'."))

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

(defun my-agenda-plan-new-day ()
  "Switch to the new day in my organization system."
  (interactive)
  (my-org-archive-done-and-saw-headings))

(defun my-org-archive-done-and-saw-headings ()
  "Archieve all `org-mode' headings which has the label done."
  (org-map-entries 'org-archive-subtree "/+DONE" 'file))

(defun my-open-main-agenda-file ()
  "Open agenda.org."
  (interactive)
  (find-file "~/agenda.org"))

(defun fast-exec-agenda-keys ()
  "Get some useful keymaps of  `fast-exec' for agenda."
  (fast-exec/some-commands ("Plane New Day" 'my-agenda-plan-new-day)))

(fast-exec/register-keymap-func 'fast-exec-agenda-keys)
(fast-exec/reload-functions-chain)

(setq org-agenda-files '("~/agenda.org"))

(bind-keys
 ("<f9>" . org-agenda)
 ("S-<f9>" . org-agenda-list)
 :map xah-fly-command-map
 ("SPC <f9>" . org-agenda-list)
 ("SPC i p" . my-open-main-agenda-file))

(defun my-add-org-subtree-to-targets-on-day ()
  "Add  a `org-mode' subtree at the point to the targets on day."
  (interactive)
  (let ((subtree-text (my-delete-and-get-text-of-org-subtree)))
    (my-goto-targets-on-day)
    (newline)
    (insert subtree-text)
    (delete-char -1)
    (beginning-of-line)
    (my-org-headline-set-todo-keyword "TODO")
    (org-schedule t (format-time-string "%Y-%m-%d"))))

(defun my-goto-targets-on-day ()
  "Visit `org-mode' subtree of the targets on day."
  (my-open-main-agenda-file)
  (beginning-of-buffer)
  (search-forward "* Targets on Day")
  (forward-char))

(defun my-delete-and-get-text-of-org-subtree (&optional pt)
  "Parse a `org-mode' subtree at the PT, delete it and return text of subtree."
  (or pt (setq pt (point)))
  (let* ((node (org-ml-parse-subtree-at pt))
         (node-beg (org-ml-get-property :begin node))
         (node-end (org-ml-get-property :end node))
         (node-text (buffer-substring node-beg node-end)))
    (--each (->>
             node
             (-second-item)
             (-partition-all 2)
             (-map 'car))
      (print it))
    (delete-region node-beg node-end)
    node-text))

(defun my-org-headline-set-todo-keyword (new-keyword &optional pt)
  "Set todo keyword of a headline at PT to NEW-KEYWORD.

PT defaults to the current `point'"
  (or pt (setq pt (point)))
  (org-ml-update-subtree-at* pt
    (org-ml-set-property :todo-keyword new-keyword it)))

(bind-keys
 :map xah-fly-command-map
 ("SPC i a" . my-add-org-subtree-to-targets-on-day))

(use-package org
    :custom
  (org-capture-templates
   '(("d"
      "Target on Day"
      entry
      (file+headline "~/agenda.org" "Targets on Day")
      "* TODO %?\n  SCHEDULED: %t\n  \n")
     ("w"
      "Target on Week"
      entry
      (file+headline "~/agenda.org" "Targets on Week")
      "* TODO %?\n  \n")
     ("f"
      "Film for See"
      entry
      (file+headline "~/agenda.org" "Films")
      (function my-films-format-as-org-heading)))))

(use-package deft
    :ensure t
    :custom
    ((deft-directory "~/notes/")
     (deft-recursive t))
    :config
    (defun fast-exec-deft-keys ()
      "Get some useful keymaps of  `fast-exec' for deft."
      (fast-exec/some-commands
       ("Manage Notes" 'deft)))
    (fast-exec/register-keymap-func 'fast-exec-deft-keys)
    (fast-exec/reload-functions-chain))

(defcustom my-mipt-dir "c:/Users/hrams/Documents/mipt"
  "Path to the directory in which will be saved all MIPT solutions.")

(defcustom my-mipt-lessons '("f" "m" "i") "Lessons of the MIPT.")

(defclass my-mipt-task ()
  ((class :initform nil :initarg :class :accessor my-mipt-task-class)
   (lesson :initform nil
           :initarg :lesson
           :accessor my-mipt-task-lesson)
   (section :initform nil
            :initarg :section
            :accessor my-mipt-task-section)
   (kind :initform nil ;nofmt
         :initarg :kind
         :accessor my-mipt-task-kind)   ; either 'control or 'normal
   (number  :initform nil
            :initarg :number
            :accessor my-mipt-task-number))
  "Object for task of the MIPT.")

(defvar my-mipt-found-task
  (my-mipt-task)
  "Object of the `my-mipt-task', will be setted automatically when find task.")

(defvar my-mipt-last-task
  nil
  "Last found MIPT task.

Object of the `my-mipt-task', will set automatically when find task.")

(defun my-mipt-task-control-p (task)
  "Return t, when TASK is control."
  (eq (my-mipt-task-kind task) 'control))

(defun my-mipt-task-normal-p (task)
  "Return t, when TASK is normal, no control."
  (not (my-mipt-task-control-p task)))

(defun my-mipt-task-parse (filename)
  "Parse from FILENAME a MIPT task."
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
  "Get path to the TASK's solution."
  (->>
   (format
    "%s-%s-%s-%s%s.tex"
    (my-mipt-task-class task)
    (my-mipt-task-lesson task)
    (my-mipt-task-section task)
    (my-mipt-task-number task)
    (if (my-mipt-task-control-p task) "-control" ""))
   (f-join my-mipt-dir)))

(defun my-mipt-last-task ()
  "Return last opened task via `recentf'."
  (-some->>
      recentf-list
    (-concat (-keep #'buffer-file-name (buffer-list)))
    (-first #'my-mipt-task-parse)
    (my-mipt-task-parse)))

(defun my-mipt-visit-last-task ()
  "Visit last opened task searched via `my-mipt-last-task'."
  (interactive)
  (my-mipt-task-visit (my-mipt-last-task)))

(defun my-mipt-next-task ()
  "Return the next task after the last found task.

When run interactively visit that task."
  (interactive)
  (let ((next-task (my-mipt-last-task)))
    (incf (my-mipt-task-number next-task))
    (if (interactive-p) (my-mipt-task-visit next-task) next-task)))

(defun my-mipt-prev-task ()
  "Return previous task, before the last found task.

When run interactively visit that task."
  (interactive)
  (let ((prev-task (my-mipt-last-task)))
    (decf (my-mipt-task-number prev-task))
    (if (interactive-p) (my-mipt-task-visit prev-task) prev-task)))

(defun my-mipt-task-visit (task)
  "Visit file of the TASK's solution."
  (interactive (list (my-mipt-find-task)))
  (find-file (my-mipt-task-path task)))

(defun my-mipt-all-tasks ()
  "Return all mipt tasks in the directory `my-mipt-dir'."
  (->> my-mipt-dir (f-files) (-keep #'my-mipt-task-parse)))

(defun my-mipt-find-task ()
  "Find the task from the minibuffer, take default info from the last task."
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
    (->>
     tasks
     (--filter (string-equal (my-mipt-task-lesson it) lesson)))))

(defun my-mipt-read-lesson ()
  "Read from user MIPT's lesson."
  (completing-read "Choose one of MIPT lessons, please: " my-mipt-lessons))

(defun my-mipt--find-class-from-tasks (tasks)
  "From TASKS find class, save in special variable, and return filtered TASKS.

Special variable is `my-mipt-found-task'"
  (let* ((class (my-mipt-choose-one-of-task-classes tasks)))
    (setf (my-mipt-task-class my-mipt-found-task) class)
    (--filter
     (= (my-mipt-task-class it) class)
     tasks)))

(defun my-mipt-choose-one-of-task-classes (tasks)
  "Take TASKS and choose one of classes."
  (->>
   tasks
   (-map #'my-mipt-task-class)
   (my-max)
   (my-mipt-read-class)))

(defun my-mipt-read-class (&optional default)
  "Read from user class of MIPT task, defaults to DEFAULT."
  (read-number "Choose one of MIPT classes, please: " default))

(defun my-mipt--find-section-from-tasks (tasks)
  "From TASKS find section, save in special variable, and return filtered TASKS.

Special variable is `my-mipt-found-task'"
  (let* ((section (my-mipt-choose-one-of-task-sections tasks)))
    (setf (my-mipt-task-section my-mipt-found-task) section)
    (--filter (= (my-mipt-task-section it) section) tasks)))

(defun my-mipt-choose-one-of-task-sections (tasks)
  "Take TASKS and choose one of sections."
  (->>
   tasks
   (-map #'my-mipt-task-section)
   (my-max)
   (my-mipt-read-section)))

(defun my-mipt-read-section (&optional default)
  "Read from user section of MIPT task, defaults to DEFAULT."
  (read-number "Enter section of MIPT task, please: " default))

(defun my-mipt--find-kind-from-tasks (tasks)
  "From TASKS find kind, save in special variable, and return filtered TASKS.

Special variable is `my-mipt-found-task'"
  (let ((kind (my-mipt-choose-one-of-task-kinds tasks)))
    (setf (my-mipt-task-kind my-mipt-found-task) kind)
    (--filter (eq (my-mipt-task-kind it) kind) tasks)))

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
    (--filter (= (my-mipt-task-number it) number) tasks)))

(defun my-mipt-choose-one-of-task-numbers (tasks)
  "Take TASKS and choose one of classes."
  (let* ((numbers (-map #'my-mipt-task-number tasks))
         (default (my-max numbers)))
    (just-completing-read-numbers "Please, choose number of MIPT task: "
                                  numbers
                                  nil
                                  nil
                                  default)))

(defun my-mipt-read-number (&optional default)
  "Read from user number of MIPT's task, defaults to DEFAULT."
  (read-number "Please, type number of MIPT task: " (or default 1)))

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
   ("Previous MIPT Task" 'my-mipt-prev-task)
   ("Open Last MIPT Task" 'my-mipt-visit-last-task)
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
   (s-replace "\\begin{equation}" "\\[")
   (s-replace "\\end{equation}" "\\]")))

(bind-keys
 :map my-latex-local-map
 ("c" . my-copy-buffer-content-as-mipt-solution))

(defcustom my-zms-compile-command
  (s-concat
   "pdflatex "
   "-interaction nonstopmode -file-line-error "
   "--output-directory=\"{output-directory}\" "
   "\"{solution.tex}\"")
  "Command which compile Solution.tex file of the ZMS section.

{solution.tex} will be replaced with path to Solution.tex file of the ZMS
section"
  :type 'string)

(defcustom my-zms-directory "~/zms"
  "Path to the directory in which will be saved files of the ZMS."
  :type 'string)

(defcustom my-zms-view-solution-latex "\\inputsolution{./solutions/%s.tex}"
  "LaTeX source code which should view solution of the ZMS task."
  :type 'string)

(defcustom my-zms-section-template-directory "~/zms/_template/"
  "Path to the directory which will be temlate for the section of the ZMS."
  :type 'string)

(defcustom my-zms-section-solutions-relatieve-path "solutions/"
  "Path relatieve with the section subdirectory to the task solutions files."
  :type 'string)

(defclass my-zms-section ()
  ((name :initarg :name :accessor my-zms-section-name)
   (number :initarg :number :accessor my-zms-section-number))
  "Section of the zms.")

(defun my-zms-new-section (section-name)
  "New section called SECTION-NAME of the ZMS tasks, solutions and other."
  (interactive "sName of the ZMS section, please: ")
  (my-zms-section-save (my-zms-next-section-called section-name)))

(defun my-zms-delete-section (section)
  "Delete SECTION of the ZMS tasks, solutions and other."
  (interactive (list (my-zms-read-section)))
  (->
   section
   (my-zms-section-path)
   (f-delete t)))

(defun my-zms-section-save (section)
  "Save the ZMS SECTION into the file system."
  (my-use-skeleton my-zms-section-template-directory
                   (my-zms-section-path section)
                   `(("_section-number"
                      . ,(number-to-string (my-zms-section-number section)))
                     ("_section-name_"
                      . ,(my-zms-section-name section)))))

(defun my-use-skeleton (skeleton-path destination replacements)
  "Copy SKELETON-PATH to DESTINATION and do REPLACEMENTS."
  (f-copy skeleton-path destination)
  (my-replace-all-in-dir replacements destination))

(defun my-replace-all-in-dir (replacements dir)
  "Replace each `car' of REPLACEMENTS to respective `cdr' in each file of DIR."
  (->
   dir
   (f-files)
   (--each (my-replace-all-in-file replacements it))))

(defun my-replace-all-in-file (replacements filename)
  "Replace each `car' of REPLACEMENTS to respective `cdr' in file at FILENAME."
  (f-write (s-replace-all replacements (f-read filename))
           'utf-8
           filename))

(defun my-zms-section-path (section)
  "Return path to the directory of the ZMS section SECTION."
  (->> section (my-zms-section-dirname) (f-join my-zms-directory)))

(defun my-zms-section-dirname (section)
  "Return name of the directory for the ZMS SECTION."
  (format "%s-%s"
          (my-zms-section-number section)
          (my-normalize-string (my-zms-section-name section))))

(defun my-zms-next-section-called (section-name)
  "Return a object of the `my-zms-section' called SECTION-NAME.

Number will be automatically initialized, depends on the previous sections."
  (my-zms-section
   :name section-name
   :number (my-zms-next-section-number)))

(defun my-zms-next-section-number ()
  "Return number of the next ZMS section."
  (--if-let (my-zms-last-section) (1+ (my-zms-section-number it)) 1))

(defun my-zms-last-section ()
  "Return the last section (section with greatest number) of the ZMS sections."
  (-some->>
      (my-zms-sections)
    (-max-by (-on '> 'my-zms-section-number))))

(defun my-zms-sections ()
  "Return list of the all ZMS sections."
  (->>
   (my-zms-sections-dirs-names)
   (-map 'my-zms-section-dirname-to-section)))

(defun my-zms-sections-dirs-names ()
  "Return list of the names of the dirictories of the all ZMS sections."
  (->>
   my-zms-directory
   (f-directories)
   (-map 'f-base)
   (--remove (s-prefix-p "_" it))))

(defun my-zms-section-dirname-to-section (section-dirname)
  "Convert ZMS SECTION-DIRNAME to an object of the `my-zms-section'."
  (let* ((dirname-parts
          (s-split " " (my-humanize-string section-dirname)))
         (number (string-to-number (car dirname-parts)))
         (name (s-join " " (cdr dirname-parts))))
    (my-zms-section :name name :number number)))

(defun my-zms-new-solution-in-current-section ()
  (interactive)
  (my-zms-new-solution (my-zms-current-section)))

(defun my-zms-new-solution (section)
  "Create a new LaTeX file into subdir of the ZMS SECTION dir named solutions."
  (interactive (list (my-zms-read-section)))
  (let ((task-number (my-zms-section-next-solution-number section)))
    (my-zms-insert-solution-to-solution.tex section task-number)
    (my-zms-find-solution section task-number)))

(defun my-zms-insert-solution-to-solution.tex (section number)
  "Insert a command viewing solution with NUMBER to Solution.tex of SECTION."
  (find-file (my-zms-section-solution.tex-path section))
  (with-temp-buffer
    (insert (f-read (my-zms-section-solution.tex-path section)))
    (forward-line -1)
    (end-of-line)
    (newline)
    (insert (format my-zms-view-solution-latex number))))

(defun my-zms-find-solution (section number)
  "Find/visit file of the ZMS SECTION solution with NUMBER file."
  (interactive
   (list
    (my-zms-read-section)
    (read-number "Number of the solution, please:")))
  (find-file (my-zms-section-solution-number-to-path section number)))

(defun my-zms-delete-solution (section number)
  "Find/visit file of the ZMS SECTION solution with NUMBER file."
  (interactive
   (list
    (my-zms-read-section)
    (read-number "Number of the solution, please:")))
  (delete-file (my-zms-section-solution-number-to-path section number)))

(defun my-zms-read-section ()
  "Read ZMS SECTION from the user."
  (helm
   :prompt "Choose section of the ZMS: "
   :sources '((name . "zms-sections")
              (candidates . my--zms-read-section-candidates)
              (action . (("Choose" . identity))))))

(defun my--zms-read-section-candidates ()
  "Candidates for the `my-zms-read-section' function."
  (->>
   (my-zms-sections)
   (--map (cons (my-zms-format-section it) it))))

(defun my-zms-format-section (section)
  "Format SECTION of the ZMS to a string."
  (format "%s. %s"
          (my-zms-section-number section)
          (my-zms-section-name section)))

(defun my-zms-section-next-solution-number (section)
  "Return number of the next task solution of the ZMS SECTION."
  (--if-let (my-zms-section-last-solution-number section) (1+ it) 1))

(defun my-zms-section-solution-number-to-path (section number)
  "Return path to the task solution of the ZMS SECTION with NUMBER."
  (f-join
   (my-zms-section-solutions-path section)
   (format "%s.tex" number)))

(defun my-zms-section-last-solution-number (section)
  "Return number of the last task solution of the SECTION."
  (->>
   section
   (my-zms-section-solutions-path)
   (f-files)
   (--map (string-to-number (f-base it)))
   (my-max)))

(defun my-zms-section-solutions-path (section)
  "Return path to the subdirectory with solutions of the SECTION subdir."
  (f-join
   (my-zms-section-path section)
   my-zms-section-solutions-relatieve-path))

(defun my-zms-section-solution.tex-path (section)
  "Return path to the Solution.tex file of the ZMS SECTION."
  (f-join
   (my-zms-section-path section)
   "Solution.tex"))

(defun my-zms-current-section ()
  "Get either ZMS session placed in the current directory or last created."
  (or
   (my-zms-path-to-section default-directory)
   (my-zms-last-section)))

(defun my-zms-path-to-section (path)
  "Convert PATH to a file of the ZMS SECTION to an object of `my-zms-section'."
  (and
   (my-zms-path-p path)
   (->>
    (f-full path)
    (s-chop-prefix (f-full my-zms-directory))
    (s-split "/")
    (car)
    (my-zms-section-dirname-to-section))))

(defun my-zms-path-p (&optional path)
  (or path (setq path (buffer-file-name)))
  (s-starts-with-p (f-full my-zms-directory) (f-full path)))

(defun fast-exec-zms-keys ()
  "Get some useful keymaps of  `fast-exec' for zms."
  (fast-exec/some-commands
   ("New ZMS Task Solution"     'my-zms-new-solution)
   ("Forward ZMS Task Solution" 'my-zms-new-solution-in-current-section)
   ("New ZMS Section"           'my-zms-new-section)
   ("Delete ZMS Section"        'my-zms-delete-section)
   ("Delete ZMS Task Solution"  'my-zms-delete-solution)))

(fast-exec/register-keymap-func 'fast-exec-zms-keys)
(fast-exec/reload-functions-chain)

(defun my-zms-run-command-recipe ()
  "Recipe of `run-command' for ZMS."
  (when (my-zms-path-p)
    (list
     (list
      :command-name "zms-compile-section"
      :display "Compile Section.tex file of the ZMS section via `pdflatex'"
      :command-line (my-zms--get-compile-command)
      :working-dir (my-zms-section-path (my-zms-current-section))))))

(defun my-zms--get-compile-command ()
  "Get command for compiling of the section file Solution.tex.

See `my-zms-compile-command'"
  (->>
   my-zms-compile-command
   (s-replace "{solution.tex}"
              (my-zms-section-solution.tex-path (my-zms-current-section)))
   (s-replace "{output-directory}"
              (my-zms-section-output-path (my-zms-current-section)))))

(defun my-zms-section-output-path (section)
  "Get path to the output directory of compiling Solution.tex file of SECTION."
  (->
   section
   (my-zms-section-path)
   (f-join "destination")))

(add-to-list 'run-command-recipes 'my-zms-run-command-recipe)
