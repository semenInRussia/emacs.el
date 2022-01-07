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

(require 'xah-fly-keys)

(xah-fly-keys-set-layout "qwerty")
(xah-fly-keys 1)

(define-key xah-fly-command-map (kbd "SPC l") nil)
(define-key xah-fly-command-map (kbd "SPC j") nil)

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

(setq search-highlight        t)
(setq query-replace-highlight t)

(define-key xah-fly-command-map (kbd "'") 'isearch-forward)

(setq imenu-auto-rescan t)
(define-key xah-fly-command-map (kbd "SPC SPC") nil)
(define-key xah-fly-command-map (kbd "SPC SPC SPC") 'imenu)

(defun if-selected-then-next-word-like-this (arg)
    (interactive "p")
    (when (use-region-p)
        (mc/mark-next-like-this arg)))

(use-package multiple-cursors
    :ensure t
    :bind
    (:map xah-fly-command-map
          ("SPC SPC t" . mc/edit-beginnings-of-lines)))

(use-package avy
    :ensure t
    :bind
    ((:map xah-fly-command-map)
     ("n" . avy-goto-char)))

(defun forward-slurp-sexp ()
    "My version of `sp-slurp-sexp`."
    (interactive)
    (save-excursion
        (backward-char)
        (sp-forward-slurp-sexp))
    )


(defun splice-sexp ()
    "My version of `sp-splice-sexp`."
    (interactive)
    (save-excursion
        (backward-char)
        (sp-splice-sexp))
    )


(use-package smartparens
    :ensure t
    :init (smartparens-global-mode 1)
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
                 )))

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

(--each TeX-modes
    (add-to-list 'pandoc-input-format-major-modes (list it "latex")))


(defun run-command-recipe-latex ()
    "Recipe for LaTeX `run-command'.
see https://github.com/bard/emacs-run-command#examples."
    (-when-let (file-path (buffer-file-name))
        (when (-contains-p TeX-modes major-mode)
            (list
             (list :display "Convert to PDF, with `pdflatex`"
                   :command-name "pdflatex"
                   :command-line (format
                                  "pdflatex \"%s\" --output-directory=\"%s\""
                                  file-path
                                  (f-dirname file-path)
                                  )))))
    )

(add-to-list 'run-command-recipes 'run-command-recipe-latex)

(add-hook 'org-mode-hook (lambda () (call-interactively 'visual-fill)))

(add-to-list 'pandoc-input-format-major-modes
             '(org-mode "org"))

(use-package elisp-format
    :ensure t)

(use-package markdown-mode
    :ensure t)

(add-hook 'markdown-mode-hook (lambda () (call-interactively 'visual-fill)))

(add-to-list 'pandoc-input-format-major-modes
             '(markdown-mode "markdown"))

(add-to-list 'pandoc-input-format-major-(message "message" format-args)odes
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
    :hook
    (web-mode . (lambda () (auto-rename-tag-mode 38))))


(use-package emmet-mode
    :ensure t
    :custom (emmet-move-cursor-between-quotes t)
    :hook
    (web-mode . emmet-mode)
    (css-mode . emmet-mode))

(defcustom html-modes '(web-mode html-mode) "List of `html` major modes.")

(--each html-modes
    (add-to-list 'pandoc-input-format-major-modes (list it "html")))

(use-package css-eldoc
    :ensure t
    :init
    (dolist (hook (list 'web-mode-hook 'css-mode-hook))
        (add-hook hook 'css-eldoc-enable)
        )
    )

(add-hook 'calc-mode-hook (lambda () (interactive) (xah-fly-keys 0)))
(add-hook 'calc-end-hook (lambda () (interactive) (xah-fly-keys 38)))

(show-paren-mode 2)
(setq make-backup-files         nil)
(setq auto-save-list-file-name  nil)
(defalias 'yes-or-no-p 'y-or-n-p)
(toggle-truncate-lines 38)

(use-package which-key
    :ensure t
    :config
    (which-key-setup-side-window-bottom)
    (which-key-mode))

(use-package helm
   :ensure t
   :config
   (setq-default helm-M-x-fuzzy-match t)
   :init
   (helm-mode 1)
   :bind
   ("C-o" . helm-find-files)
   (:map xah-fly-command-map
         ("SPC SPC f" . helm-find-files)))

(require 'fast-exec)

(fast-exec/enable-some-builtin-supports
 yasnippet
 projectile
 magit
 flycheck
 haskell-mode)

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

(defun open-scratch ()
    "Open scratch."
    (interactive)
    (switch-to-buffer "*scratch*")
    )

(global-set-key (kbd "C-t") 'open-scratch)

(global-subword-mode)

(use-package syntax-subword
    :ensure t
    :custom
    (syntax-subword-skip-spaces t)
    :config
    (global-syntax-subword-mode)
    )

(use-package super-save
    :ensure t
    :config
    (super-save-mode 38))

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

(use-package magit :ensure t)

(add-hook 'dired-mode-hook (lambda () (dired-hide-details-mode 1)))

(use-package run-command
    :ensure t
    :init
    :bind (:map xah-fly-command-map
                ("SPC , c" . run-command)))

(defcustom pandoc-output-formats '("asciidoc"
                                   "beamer"
                                   "bibtex"
                                   "biblatex"
                                   "commonmark"
                                   "commonmark"
                                   "context"
                                   "csljson"
                                   "docbook"
                                   "docbook5"
                                   "docx"
                                   "dokuwiki"
                                   "epub"
                                   "epub2"
                                   "fb2"
                                   "gfm"
                                   "haddock"
                                   "html"
                                   "html4"
                                   "icml"
                                   "ipynb"
                                   "jats"
                                   "jats"
                                   "jats"
                                   "jats"
                                   "jira"
                                   "json"
                                   "latex"
                                   "man"
                                   "markdown"
                                   "markdown"
                                   "markdown"
                                   "markdown"
                                   "mediawiki"
                                   "ms"
                                   "muse"
                                   "native"
                                   "odt"
                                   "opml"
                                   "opendocument"
                                   "org"
                                   "pdf"
                                   "plain"
                                   "pptx"
                                   "rst"
                                   "rtf"
                                   "texinfo"
                                   "textile"
                                   "slideous"
                                   "slidy"
                                   "dzslides"
                                   "revealjs"
                                   "s5"
                                   "tei"
                                   "xwiki"
                                   "zimwiki")
  "This is list of pandoc's formats valid to pandoc's output.
See https://pandoc.org")

(defcustom pandoc-codes-and-extensions
  (make-hash-table :test 'equal)
  "This is map of pandoc's format code and extension of file.
If your pandoc's code have extensions, which equal to pandoc's code
(for example: org = .(org)), then just don't put pair to this variable.")

(puthash "markdown" "md" pandoc-codes-and-extensions)
(puthash "latex" "tex" pandoc-codes-and-extensions)

(defun pandoc-change-format-of-file (filename new-format)
    "Change FILENAME with pandoc's format to filename with pandoc's NEW-FORMAT."
    (let ((new-ext (or (gethash new-format pandoc-codes-and-extensions)
                       new-format)))
        (f-swap-ext filename new-ext))
    )

(defun pandoc-code-for-major-mode (major-mode)
    "Return non-nil value when MAJOR-MODE is one of Pandoc input formats.
See pandoc input formats: https://pandoc.org"
    (-when-let (el (--find (eq major-mode (-first-item it))
                           pandoc-input-format-major-modes))
        (-second-item el))
    )

(defun run-command-recipe-pandoc ()
    "Pandoc `run-command` recipe, for transform to other formats.
See `run-command-recipes`:
https://github.com/bard/emacs-run-command#examples"
    (-when-let (source (buffer-file-name))
        (-when-let (source-code (pandoc-code-for-major-mode major-mode))
            (--map (list
                    :command-name (format
                                   "pandoc-%s-to-%s"
                                   source-code it)
                    :display (format
                              "Convert %s to %s with Pandoc"
                              (s-upcase source-code)
                              (s-upcase it))
                    :command-line
                    (format
                     "pandoc -t %s -f %s -o \"%s\" \"%s\""
                     it source-code
                     (pandoc-change-format-of-file source it)
                     source
                     )
                    )
                   pandoc-output-formats)))
    )

(add-to-list 'run-command-recipes 'run-command-recipe-pandoc)

(defun if-Emacs-org-then-org-babel-tangle ()
    "If current open file is Emacs.org, then `org-babel-tangle`."
    (interactive)

    (when (s-equals? (f-filename buffer-file-name) "Emacs.org")
        (org-babel-tangle)))

(add-hook 'after-save-hook 'if-Emacs-org-then-org-babel-tangle)

\|print(name)
