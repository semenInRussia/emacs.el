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

(use-package s :ensure t)

(use-package f)

(use-package dash)

(setq user-full-name    "Semen Khramtsov"
      user-mail-address "hrams205@gmail.com"
      user-birthday     "2007-01-29")

(if (s-equals? (format-time-string "%Y-%m-%d") user-birthday)
    (animate-birthday-present))

(use-package yasnippet
   :ensure t
   :init
   (yas-global-mode 1)
   :config
   (setq yas-snippet-dirs '("~/.emacs.d/snippets")))

(use-package yasnippet-snippets
    :ensure t
    :config
    (yasnippet-snippets-initialize))

(use-package yasnippet-classic-snippets :ensure t)

(use-package flycheck
    :config (global-flycheck-mode 1))

(setq search-highlight        t)
(setq query-replace-highlight t)

(use-package company
    :ensure t
    :init
    (setq company-async-wait 0.8)
    :config
    (global-company-mode))

(require 'xah-fly-keys)

(xah-fly-keys-set-layout "qwerty")
(xah-fly-keys 1)

(define-key xah-fly-command-map (kbd "SPC l") nil)
(define-key xah-fly-command-map (kbd "SPC j") nil)

(defun if-selected-then-next-word-like-this (arg)
    (interactive "p")
    (when (use-region-p)
        (mc/mark-next-like-this arg)))

(use-package multiple-cursors
    :ensure t
    :bind (("<tab>" . if-selected-then-next-word-like-this)))

(use-package avy
    :ensure t
    :bind
    ((:map xah-fly-command-map)
     ("SPC '" . avy-goto-char)))

(use-package smartparens
    :ensure t
    :config (smartparens-global-mode))

(defun delete-only-1-char ()
    "Delete only 1 character before point."
    (interactive)
    (backward-char)
    (delete-char 1)
    )

(define-key xah-fly-command-map (kbd "DEL") 'delete-only-1-char)

(use-package expand-region
    :ensure t
    :bind
    (:map xah-fly-command-map
    ("1" . er/expand-region)))

(defun kmacro-start-or-end-macro ()
    "If macro record have just started, then stop this record, otherwise start macro record."
    (interactive)
    (if defining-kbd-macro
        (kmacro-end-macro 1)
        (kmacro-start-macro 1)))

(define-key xah-fly-command-map (kbd "\\") 'kmacro-start-or-end-macro)

(defun kmacro-call-macro-or-apply-to-lines (arg &optional top bottom)
    "If selected region, then apply last macro to selected lines, otherwise call last macro."
    (interactive 
     (list
      1
      (if (use-region-p) region-beginning nil)
      (if (use-region-p) region-end nil)))

    (if (use-region-p)
        (apply-macro-to-region-lines top bottom)
        (kmacro-call-macro arg)))

(define-key xah-fly-command-map (kbd "=") 'kmacro-call-macro-or-apply-to-lines)

(defun delete-and-edit-current-line ()
    "Delete current line and instroduce to insert mode."
    (interactive)
    (beginning-of-line-text)
    (kill-line)
    (xah-fly-insert-mode-init)
    )

(define-key xah-fly-command-map (kbd "w") 'delete-and-edit-current-line)

(defun select-current-or-next-word ()
    "If word was selected, then select next word, otherwise select current word."
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

(setq-default indent-tabs-mode nil)
(setq-default tab-width          4)
(setq-default c-basic-offset     4)
(setq-default standart-indent    4)
(setq-default lisp-body-indent   4)

(global-set-key (kbd "RET") 'newline-and-indent)
(setq lisp-indent-function  'common-lisp-indent-function)

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
    (unless (= 0 (- end-line start-line))
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
         (message "%s" language-hook)
         (add-hook
          language-hook
          (lambda ()
              (define-key
                  xah-fly-command-map
                  (kbd "SPC j")
                  ',backward-block-function)))))

(setq latex-documentclasses 
    '("article" "reoport" "book" "proc" "minimal" "slides" "memoir" "letter" "beamer"))

(setq latex-environment-names
    '( "figure"
       "table"
       "description"
       "enumerate"
       "itemize"
       "list"
       "math"
       "displaymath"
       "split"
       "array"
       "eqnarray"
       "equation"
       "theorem"
       "matrix"
       "cases"
       "align"
       "center"
       "flushleft"
       "flushright"
       "minipage"
       "quotation"
       "quote"
       "verbatim" 
       "verse"
       "tabbing"
       "tabular"
       "thebibliography" 
       "titlepage"
       "document"))

;; If this information is not actual, then here my python script and add `document`, 
;; so all claims to this site https://latex.wikia.org/wiki/List_of_LaTeX_environments:

;; import requests
;; from bs4 import BeautifulSoup as Soup


;; url = "https://latex.wikia.org/wiki/List_of_LaTeX_environments"

;; def main():
;;     request = requests.get(url)
;;     soup = Soup(request.text, "html.parser")
;;     elements = soup.select("h3 > span.mw-headline")
;;     elements = list(filter(lambda el: "environment" in el.text, elements))
;;     codes = list(map(lambda el: el.text.split()[0].lower(), elements))
;;     print(codes)

(setq flycheck-python-pylint-executable "pylint")
(setq flycheck-python-flake8-executable "flake8")
(setq flycheck-python-mypy-executable "mypy")

(defun org-mode-visual-fill ()
  (interactive)
  (setq visual-fill-column-width 90
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :ensure t
  :hook (org-mode . org-mode-visual-fill))

(defun org-forward-heading ()
    "Forward heading in `org-mode`."
    (interactive)
    (let ((old-point (point)))
        (org-forward-heading-same-level 1)
        (if (= (point) old-point)
            (org-forward-element)))
    )

(defun org-backward-heading ()
    "Backward heading in `org-mode`."
    (interactive)
    (let ((old-point (point)))
        (org-backward-heading-same-level 1)
        (if (= (point) old-point)
            (org-backward-element)))
    )

(add-nav-forward-block-keymap-for-language org-mode org-forward-heading)
(add-nav-backward-block-keymap-for-language org-mode org-backward-heading)

(show-paren-mode 2)
(setq make-backup-files         nil)
(setq auto-save-list-file-name  nil)
(defalias 'yes-or-no-p 'y-or-n-p)

(setq dont-truncate-lines-modes '(org-mode))

(defun truncate-or-not-truncate-words ()
    "Truncate words or don't truncate words.
If current `major-mode` don't need to truncate words, then don't truncate words,
otherwise truncate words."
    (interactive)
    (if (-contains? dont-truncate-lines-modes major-mode)
        (toggle-truncate-lines 0)
        (toggle-truncate-lines 38)))

(add-hook 'prog-mode-hook 'truncate-or-not-truncate-words)

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
   ("C-o" . helm-find-files))

(use-package google-translate
    :ensure t
    :bind
    (:map xah-fly-command-map
          ("SPC \\" . google-translate-at-point)))

(defun google-translate--search-tkk ()
  "Search TKK. From https://github.com/atykhonov/google-translate/issues/137.
Thank you https://github.com/leuven65!"
  (list 430675 2721866130))

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode   -1)

(toggle-frame-fullscreen)

(use-package doom-themes 
    :ensure t
    :config
    (load-theme 'doom-1337 t))

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
  :config
  (display-time-mode t)
  (setq doom-modeline-icon nil)
  (setq doom-modeline-workspace-name nil)
  :init
  (doom-modeline-mode 1))

(set-face-attribute 'default nil :font "Consolas" :height 200)
(set-frame-font "Consolas" nil t)

(hl-line-mode 1)

(defun get-project-name (project-root)
    "Return name of project by path - `PROJECT-ROOT`."
    (s-titleize (f-dirname project-root)))

(require 'projectile)
(setq projectile-project-search-path '("~/projects/"))
(setq projectile-completion-system 'helm)
(setq projectile-project-name-function 'get-project-name)

(global-set-key (kbd "S-<f5>") 'projectile-test-project)

(projectile-mode 1)

(use-package magit :ensure t)

(add-hook 'dired-mode-hook (lambda () (dired-hide-details-mode 1)))

(defun if-Emacs-org-then-org-babel-tangle ()
    "If current open file is Emacs.org, then `org-babel-tangle`."
    (interactive)

    (when (s-equals? (f-filename buffer-file-name) "Emacs.org")
        (org-babel-tangle)))

(add-hook 'after-save-hook 'if-Emacs-org-then-org-babel-tangle)
