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

(show-paren-mode 2)
(setq make-backup-files         nil)
(setq auto-save-list-file-name  nil)
(toggle-truncate-lines t)
(setq word-wrap t)
(defalias 'yes-or-no-p 'y-or-n-p)

(use-package yasnippet-snippets
   :ensure t
   :init
   (yas-global-mode 1)
   :config
   (setq yas-snippet-dirs '("~/.emacs.d/snippets")))

(use-package helm
   :ensure t
   :config
   (setq-default helm-M-x-fuzzy-match t)
   :init
   (helm-mode 1)
   :bind
   ("C-o" . helm-find-files))

(setq search-highlight        t)
(setq query-replace-highlight t)

(use-package company
    :ensure t
    :init
    (company-mode)
    :hook (after-init-hook . company-mode))

(require 'xah-fly-keys)

(xah-fly-keys-set-layout "qwerty") 
(xah-fly-keys 1)

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode   -1)

(toggle-frame-fullscreen)

(use-package doom-themes
  :ensure t
  :config
  (load-theme 'doom-molokai))

(use-package linum
   :config
   (setq linum-format "  %d    ")
   :init
   (global-linum-mode 1)
   )

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

(setq-default indent-tabs-mode nil)
(setq-default tab-width          4)
(setq-default c-basic-offset     4)
(setq-default standart-indent    4)
(setq-default lisp-body-indent   4)

(global-set-key (kbd "RET") 'newline-and-indent)
(setq lisp-indent-function  'common-lisp-indent-function)

(hl-line-mode 1)

(defun get-project-name (project-root)
    "Return name of project by path - `PROJECT-ROOT`."
    (s-titleize (f-dirname project-root)))

(require 'projectile)
(setq projectile-project-search-path '("~/projects/"))
(setq projectile-completion-system 'helm)
(setq projectile-project-name-function 'get-project-name)
(projectile-mode 1)

(use-package magit :ensure t)

(defun if-Emacs-org-then-org-babel-tangle ()
    (interactive)
    (message buffer-file-name)
    (when (s-equals? (f-filename buffer-file-name) "Emacs.org")
        (org-babel-tangle)))

(add-hook 'after-save-hook 'if-Emacs-org-then-org-babel-tangle)
