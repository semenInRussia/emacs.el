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
    (s-equals? user-os "Windows")
    )

(if (s-equals? (format-time-string "%Y-%m-%d") user-birthday)
    (animate-birthday-present))

(use-package yasnippet
    :ensure t
    :init
    (yas-global-mode 1)
    :config
    (setq yas-snippet-dirs '("~/.emacs.d/snippets")))

(use-package yasnippet-classic-snippets :ensure t)

(use-package auto-yasnippet
    :ensure t)

(use-package flycheck
    :ensure t
    :config (global-flycheck-mode 1))

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

(use-package browse-kill-ring
    :ensure t)

(defcustom bbyac-major-modes-maps (list emacs-lisp-mode-map)
  "List of maps of `major-modes` which support `bbyac`.")

(use-package bbyac
    :ensure t
    :init
    (--each bbyac-major-modes-maps
        (define-key it (kbd "C-j") 'bbyac-expand-symbols)
        ))

(use-package format-all
    :ensure t
    )

(require 'xah-fly-keys)

(xah-fly-keys-set-layout "qwerty")
(xah-fly-keys 1)

(define-key xah-fly-command-map (kbd "SPC l") nil)
(define-key xah-fly-command-map (kbd "SPC j") nil)
(define-key xah-fly-command-map (kbd "SPC SPC") nil)

(defun keymap-to-list (keymap)
    "Convert `KEYMAP` to list."
    (--filter (ignore-errors '((cat it) (cdr it))) (-drop 1 keymap))
    )


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
    (--reduce-from (s-concat acc (char-to-string it)) "" chars)
    )


(defmacro define-key-when (keymap key def condition)
    "Macro for define keymaps for `rectangle-mode` in `xah-fly-command-mode`"
    `(define-key ,keymap (kbd ,key)
         (lambda ()
             (interactive)
             (if (funcall ,condition)
                 (call-interactively ,def)
                 (call-interactively ',(function-of-key (eval keymap) key)))))
    )

(use-package swiper-helm
    :ensure t
    :bind (:map xah-fly-command-map
                ("'" . swiper-helm))
    )

(define-key xah-fly-command-map (kbd "SPC SPC r") 'projectile-replace)

(setq imenu-auto-rescan t)
(define-key xah-fly-command-map (kbd "SPC SPC") nil)
(define-key xah-fly-command-map (kbd "SPC SPC SPC") 'imenu)

(use-package imenu-anywhere
    :ensure t
    :bind (:map xah-fly-command-map
                ("SPC SPC n" . imenu-anywhere)))

(use-package rg
    :ensure t
    )

(use-package dumb-jump
    :ensure t
    :custom
    (dumb-jump-force-searcher 'rg)
    (dumb-jump-prefer-searcher 'rg)
    :init
    (add-hook 'xref-backend-functions #'dumb-jump-xref-activate)
    )

(use-package multiple-cursors
    :ensure t
    :bind
    (:map xah-fly-command-map
          ("7" . mc/mark-next-like-this-word)
          ("SPC 7" . 'mc/mark-previous-like-this-word)
          ("SPC SPC 7" . mc/edit-beginnings-of-lines)))

(use-package avy
    :ensure t
    :custom (avy-background t)
    :bind ((:map xah-fly-command-map)
           ("n"           . avy-goto-char)
           ("SPC SPC v"   . avy-yank-word)
           ("SPC SPC x"   . avy-teleport-word)
           ("SPC SPC c"   . avy-copy-word)
           ("SPC SPC d"   . avy-kill-word)
           ("SPC SPC l c" . avy-copy-line)
           ("SPC SPC l x" . avy-move-line)
           ("SPC SPC l d" . avy-kill-whole-line)))


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


(defun avy-teleport-word (char &optional arg)
    "Teleport word searched by `arg' with CHAR.
Pass ARG to `avy-jump'."
    (interactive "cchar:\nP")
    (avy-goto-word-1-with-action char 'avy-action-teleport)
    )


(defun avy-copy-word (char &optional arg)
    "Copy word searched by `arg' with CHAR.
Pass ARG to `avy-jump'."
    (interactive "cchar:\nP")
    (avy-goto-word-1-with-action char 'avy-action-copy)
    )


(defun avy-yank-word (char &optional arg)
    "Paste word searched by `arg' with CHAR.
Pass ARG to `avy-jump'."
    (interactive "cchar:\nP")
    (avy-goto-word-1-with-action char 'avy-action-yank)
    )


(defun avy-kill-word (char &optional arg)
    "Paste word searched by `arg' with CHAR.
Pass ARG to `avy-jump'."
    (interactive "cchar:\nP")
    (avy-goto-word-1-with-action char 'avy-action-kill-stay)
    )

(defun forward-slurp-sexp ()
    "My version of `sp-slurp-sexp`."
    (interactive)
    (save-excursion
        (backward-char)
        (sp-forward-slurp-sexp)))


(defun splice-sexp ()
    "My version of `sp-splice-sexp`."
    (interactive)
    (save-excursion
        (backward-char)
        (sp-splice-sexp)))


(use-package smartparens
    :ensure t
    :init
    (smartparens-global-mode 1)
    :bind (:map xah-fly-command-map
                (("]" . forward-slurp-sexp)
                 ("-" . splice-sexp)
                 ("SPC -" . sp-rewrap-sexp)
                 ("m" . sp-backward-sexp)
                 ("." . sp-forward-sexp)
                 ("SPC 1" . sp-join-sexp)
                 ("SPC SPC 1" . sp-split-sexp)
                 ("SPC 9" . sp-change-enclosing)
                 ("SPC SPC g" . sp-kill-hybrid-sexp)
                 ("SPC =" . sp-raise-sexp)
                 )))

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

(use-package poporg
    :ensure t
    :bind (:map xah-fly-command-map
                ("`" . poporg-dwim))
    )

(define-key-when xah-fly-command-map "n" 'avy-transpose-lines-in-region
                 'use-region-p)

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
    (newline-and-indent)
    (forward-line -1)
    (end-of-line)
    )

(define-key xah-fly-command-map (kbd "s") 'open-line-saving-indent)

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

(define-key-when xah-fly-command-map "q" 'delimit-columns-rectangle
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

(defmacro add-nav-forward-block-keymap-for-language (language forward-block-function)
    "Bind `FORWARD-BLOCK-FUNCTION` to `LANGUAGE`-map."
    `(let ((language-hook (intern (s-append "-hook" (symbol-name ',language)))))
         (add-hook
          language-hook
          (lambda ()
              (define-key
                  xah-fly-command-map
                  (kbd "SPC l")
                  ',forward-block-function)))))


(defmacro add-nav-backward-block-keymap-for-language (language backward-block-function)
    "Bind `BACKWARD-BLOCK-FUNCTION` to `LANGUAGE`-map."
    `(let ((language-hook (intern (s-append "-hook" (symbol-name ',language)))))
         (add-hook
          language-hook
          (lambda ()
              (define-key
                  xah-fly-command-map
                  (kbd "SPC j")
                  ',backward-block-function)))))

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

(defun visual-fill (width)
    (interactive (list 70))
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

(defcustom pandoc-input-format-major-modes
  nil
  "List of pair from mode, one of input formats of Pandoc and pandoc codes
See this https://pandoc.org
Examples of codes (latex, markdown)"
  )

(add-to-list 'TeX-modes 'LaTeX-mode)

(setq latex-documentclasses 
    '("article" "reoport" "book" "proc" "minimal" "slides" "memoir" "letter" "beamer"))

(dolist (mode (list 'TeX-mode-hook
                    'tex-mode-hook
                    'latex-mode-hook
                    'LaTeX-mode-hook))
    (add-hook mode (lambda () (call-interactively 'visual-fill))))

(use-package auctex
    :ensure t)

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
    )


(defun latex-make-text-italic ()
    "If regions select, wrap region with `emph`, otherwise make word."
    (interactive)
    (latex-wrap-text "emph")
    )


(defun latex-make-text-bold ()
    "If regions select, wrap region with `textbf`, otherwise make word."
    (interactive)
    (latex-wrap-text "emph")
    )

(defun latex-make-text-formula ()
    "If regions select, make region formula, otherwise make line formula."
    (interactive)
    (unless (use-region-p)
        (end-of-line)
        (set-mark (point-at-bol))
        )
    (let ((text-beg (region-beginning))
          (text-end (region-end)))
        (deactivate-mark)
        (goto-char text-beg)
        (insert "\\[")

        (goto-char (+ text-end 2))
        (insert "\\]"))
    )


(--each '(tex-mode-hook latex-mode-hook LaTeX-mode-hook)
    (add-hook it (lambda ()
                     (define-key xah-fly-command-map (kbd "SPC *")
                         'latex-make-text-formula)
                     (define-key xah-fly-command-map (kbd "SPC e")
                         'latex-make-text-italic)
                     (define-key xah-fly-command-map (kbd "SPC b")
                         'latex-make-text-italic))
              ))

(add-hook 'org-mode-hook (lambda () (call-interactively 'visual-fill)))

(add-to-list 'pandoc-input-format-major-modes
             '(org-mode "org"))

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
                ([remap imenu] . helm-org-in-buffer-headings)))

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

(use-package markdown-mode
    :ensure t)

(add-hook 'markdown-mode-hook (lambda () (call-interactively 'visual-fill)))

(use-package markdown-toc
    :ensure t
    :init
    (defun fast-exec-define-markdown-toc-keys ()
        "Bind `markdown-toc' and `fast-exec'."
        (fast-exec/some-commands
         ("Make Table of Contents in Markdown"
          'markdown-toc-generate-or-refresh-toc))
        )
    (fast-exec/register-keymap-func 'fast-exec-define-markdown-toc-keys)
    (fast-exec/reload-functions-chain))

(add-to-list 'pandoc-input-format-major-modes
             '(markdown-mode "markdown"))

(add-to-list 'pandoc-input-format-major-modes
             '(gfm-mode "markdown"))

(setq py/imports-regexp "import\\|from")

(setq python-shell-interpreter "python")

(add-nav-forward-block-keymap-for-language
 python-mode
 python-nav-forward-block)


(add-nav-backward-block-keymap-for-language
 python-mode
 python-nav-backward-block)


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

(add-nav-forward-block-keymap-for-language
 haskell-mode
 haskell-ds-forward-decl)

(add-nav-backward-block-keymap-for-language
 haskell-mode
 haskell-ds-backward-decl)

(setq js/imports-regexp "import")

(setq js/function-or-class-regexp "function \\|class ")

(use-package js-comint
    :ensure t)

(if (user-os-windows-p)
    (setq js-comint-program-command "C:/Program Files/nodejs/node.exe"))

(use-package web-mode
    :ensure t)

(use-package js2-mode
    :ensure t)

(use-package json-mode
    :ensure t)

(add-to-list 'auto-mode-alist '("\\.jsx$" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

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


(defun js/nav-forward-function-or-class ()
    "Navigate to next function or class in JS."
    (interactive)
    (search-forward-regexp js/function-or-class-regexp)
    )

(add-nav-forward-block-keymap-for-language
 js2-mode
 js/nav-forward-function-or-class)

(use-package web-mode
    :ensure t
    :hook (web-mode . yas-minor-mode-off)
    :custom
    (web-mode-script-padding 1)
    (web-mode-block-padding 0)
    )


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

(defcustom html-modes '(web-mode html-mode mhtml-mode)
  "List of `html` major modes.")

(use-package tagedit
    :ensure t
    :init
    (--each html-modes
        (let ((map (eval (intern (s-append "-map" (symbol-name it))))))
            (define-key map
                [remap sp-kill-hybrid-sexp] 'tagedit-kill)
            (define-key map
                [remap sp-join-sexp] 'tagedit-join-tags)
            (define-key map
                [remap sp-raise-sexp] 'tagedit-raise-tag)
            (define-key map
                [remap splice-sexp] 'tagedit-splice-tag)
            (define-key map
                [remap sp-change-enclosing] 'tagedit-kill-attribute))))

(use-package css-eldoc
    :ensure t
    :init
    (dolist (hook (list 'web-mode-hook 'css-mode-hook))
        (add-hook hook 'css-eldoc-enable)))

(add-hook 'calc-mode-hook (lambda () (interactive) (xah-fly-keys 0)))
(add-hook 'calc-end-hook (lambda () (interactive) (xah-fly-keys 38)))

(show-paren-mode 2)
(setq make-backup-files         nil)
(setq auto-save-list-file-name  nil)
(defalias 'yes-or-no-p 'y-or-n-p)
(toggle-truncate-lines 38)

(use-package git-gutter
    :ensure t
    :hook
    (prog-mode . git-gutter-mode))

(use-package helm-tail
    :ensure t
    :init
    (defun fast-exec-define-helm-tail-keys ()
        "This is bind `fast-exec' with `helm-tail'."
        (fast-exec/some-commands
         ("Open Tail" 'helm-tail)))
    (fast-exec/register-keymap-func 'fast-exec-define-helm-tail-keys)
    (fast-exec/reload-functions-chain))

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
   :config (setq-default helm-M-x-fuzzy-match t)
   :init (helm-mode 1)
   :bind (:map xah-fly-command-map
               ("SPC SPC f" . helm-find-files)))

(require 'fast-exec)

(fast-exec/enable-some-builtin-supports haskell-mode
                                        flycheck
                                        magit
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

(use-package google-translate
    :ensure t
    :bind
    (:map xah-fly-command-map
          ("SPC \\" . google-translate-at-point)))

(defun google-translate--search-tkk ()
  "Search TKK. From https://github.com/atykhonov/google-translate/issues/137.
Thank you https://github.com/leuven65!"
  (list 430675 2721866130))

(use-package command-log-mode
    :ensure t)

(add-hook 'prog-mode-hook 'whitespace-mode)

(add-hook 'change-major-mode-hook 'visual-line-mode)

(add-hook 'change-major-mode-hook (lambda ()
                                      (interactive)
                                      (auto-fill-mode 1)
                                      ))

(use-package scratch
    :ensure t
    :bind (("C-t" . scratch))
    )

(global-subword-mode)

(use-package syntax-subword
    :ensure t
    :custom
    (syntax-subword-skip-spaces t)
    :config
    (global-syntax-subword-mode)
    )

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
                        (set-window-buffer nil (current-buffer)))))

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

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode   -1)

(toggle-frame-fullscreen)

(require 'gruber-darker-theme)

(use-package gruber-darker-theme
    :ensure t
    :init
    (load-theme 'gruber-darker t)
    )

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

(use-package doom-modeline
    :ensure t
    :custom
    (doom-modeline-icon nil)
    (doom-modeline-modal-icon nil)
    (doom-modeline-buffer-file-name-style 'auto)
    (doom-modeline-workspace-name nil)
    (doom-modeline-project-detection 'projectile)
    (doom-modeline-buffer-enconding 'projectile)
    (doom-modeline-enable-word-count t)
    (doom-modeline-height 24)
    :init
    (display-time-mode t)
    (column-number-mode)
    :config
    (doom-modeline-mode 0)
    (doom-modeline-mode 38))

(set-face-attribute 'default nil :font "Consolas" :height 250)
(set-frame-font "Consolas" nil t)

(use-package origami
    :ensure t
    :bind (:map xah-fly-command-map
                ("SPC ]" . origami-open-node)
                ("SPC [" . origami-close-node)
                ("SPC SPC ]" . origami-open-all-nodes)
                ("SPC SPC [" . origami-close-all-nodes))
    :hook (org-mode-hook . origami-mode)
    :config
    (global-origami-mode 38))

(global-hl-line-mode 1)

(use-package page-break-lines
    :ensure t
    :init
    (global-page-break-lines-mode 38))

(defun get-project-name (project-root)
    "Return name of project by path - `PROJECT-ROOT`."
    (s-titleize (f-dirname project-root)))

(require 'projectile)
(setq projectile-project-search-path '("~/projects/"))
(setq projectile-completion-system 'helm)
(setq projectile-project-name-function 'get-project-name)

(global-set-key (kbd "S-<f5>") 'projectile-test-project)
(global-set-key (kbd "<f5>") 'projectile-run-project)

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

(add-hook 'dired-mode-hook (lambda () (dired-hide-details-mode 1)))

(use-package run-command
    :ensure t
    :bind (:map xah-fly-command-map
                ("SPC , c" . run-command)))

(use-package run-command-recipes
    :ensure t
    :after (run-command)
    :init
    (run-command-recipes-use-all))

(use-package skeletor
    :ensure t
    :custom
    (skeletor-project-directory "~/projects")
    (skeletor-completing-read-function completing-read-function))

(defun if-Emacs-org-then-org-babel-tangle ()
    "If current open file is Emacs.org, then `org-babel-tangle`."
    (interactive)

    (when (s-equals? (f-filename buffer-file-name) "Emacs.org")
        (org-babel-tangle)))

(add-hook 'after-save-hook 'if-Emacs-org-then-org-babel-tangle)

\|print(name)
