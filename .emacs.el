(require 'cl-lib)

(setq initial-buffer-choice "~/Start.org")

(defun my-file-base (file)
  "Return base of the FILE: just name of file without extension."
  (car
   (split-string (car (last (split-string file "/"))) "\\.")))

(add-to-list 'load-path "~/.emacs.d/lisp")

(let ((dirs (directory-files-recursively "~/.emacs.d/lisp" ".*" t)))
  (while dirs
    (when (file-directory-p (car dirs))
      (add-to-list 'load-path (car dirs)))
    (setq dirs (cdr dirs))))

(defcustom my-modules-order
  (list
   'my-straight
   'my-leaf
   "package-management"
   'my-libs
   'my-lib
   'my-all-the-icons
   'my-doom-modeline
   'my-info
   'my-xah
   'my-fast-exec
   'my-smartparens
   "editing"
   'my-aas
   'my-lisp
   'my-autoformat
   "languages"
   "env"
   "ui"
   t)
  "List of the modules which will be load with the special function.

The special function is `my-for-each-config-module'.")

(defvar my-modules-files
  (directory-files-recursively "~/.emacs.d/lisp" ".el$" nil
                               (lambda (x)
                                 (not (string-suffix-p "test" x)))))

(defvar my-load-modules-all (length my-modules-files))

(defun my-reload-modules (&rest modules)
  "Unload and then load each of MODULES.

Each of MODULES is either string (indicates loading each of directory),
symbol indicates load only one module or t, indicates load other"
  (apply 'my-unload-modules modules modules)
  (apply 'my-load-modules modules))

(defun my-load-modules (&rest modules)
  "Load each of MODULES.

Each of MODULES is either string (indicates loading each of directory),
symbol indicates load only one module or t, indicates load other"
  (my-for-each-config-module 'my-require-or-debug modules))

(defun my-require-or-debug (module)
  "Require MODULE, if has any errors, then debug that."
  (unless (featurep module)
    (let ((start-time (current-time)))
      (if (ignore-errors (require module nil t))
          (message "`%s' module took %dsec"
                   module
                   (float-time (time-since start-time)))
        (lwarn 'my :error "Error in module `%s'\n" module)))))

(defun my-unload-modules (&rest modules)
  "UnLoad each of MODULES.

Each of MODULES is either string (indicates loading each of directory),
symbol indicates load only one module or t, indicates load other"
  (my-for-each-config-module
   (lambda (module) (unload-feature module t))
   modules))

(defun my-for-each-config-module (f modules &optional start)
  "Call F for each of the my Emacs configuration MODULES.

Each of MODULES is either string (indicates loading each of directory),
symbol indicates load only one module or t, indicates load other.

After each call of F, print progress starting from the START."
  (let ((count (or start 0))
        module)
    (while modules
      (setq module (car modules))
      (setq modules (cdr modules))
      (setq count (1+ count))
      (cond
       ((and
	 (symbolp module)
	 (not (eq module t))
	 (not (eq module '##)))
        (message "Call `%s' with %s (%s/%s)"
                 f
                 module
                 count
                 my-load-modules-all)
        (funcall f module))
       ((stringp module)
        (setq count
              (+ count
                 (1-
                  (my-for-each-module-of-config-dir f module
                                                    (1- count))))))
       ((eq module t)
        (print "AGAIN!")
        (my-for-each-module-of-config-dir f "."))))))

(defun my-for-each-module-of-config-dir (f dir &optional start)
  "Call F for each Emacs Lisp file of the the configuration subdir DIR.

After each call of F, print progress starts with START.

Return number of modules on which was call F."
  (let ((modules
         (mapcar
          (lambda (file) (intern (my-file-base file)))
          (directory-files-recursively
           (concat "~/.emacs.d/lisp/" dir)
           "\\.el$"))))
    (my-for-each-config-module f modules start)
    (length modules)))

(defun my-load-modules-of-dir (dir)
  "Load each Emacs Lisp file of the DIR."
  (apply
   'my-load-modules
   (mapcar
    (lambda (file) (intern (my-file-base file)))
    (directory-files-recursively dir ".el$"))))

(defun my-load-all-config-modules ()
  "Load each of the my Emacs configuration Elisp files via `require'."
  (interactive)
  (apply 'my-load-modules my-modules-order))

(defun my-reload-all-config-modules ()
  "Unload and load after each module of the my Emacs configuration."
  (interactive)
  (apply 'my-reload-modules my-modules-order))

(my-load-all-config-modules)

(defgroup my nil "Group for all my config files." :group 'tools)

(defun my-bench ()
  "Show bench analysis."
  (interactive)
  (require 'dash)
  (require 's)
  (require 'inspector)
  (switch-to-buffer "*Messages*")
  (->>
   (buffer-substring-no-properties (point-min) (point-max))
   (s-lines)
   (--keep
    (-when-let
        ((_ module duration)
         (s-match "‘\\(.*?\\)’ module took \\(.*?\\)sec" it))             ;nofmt
      (cons module (string-to-number duration))))
   (--sort (> (cdr it) (cdr other)))
   (inspector-inspect)))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(avy-background t)
 '(blamer-idle-time 0.3 t)
 '(blamer-min-offset nil t)
 '(company-dabbrev-ignore-case nil)
 '(company-idle-delay 0.3)
 '(company-minimum-prefix-length 2)
 '(company-show-quick-access t)
 '(company-tooltip-align-annotations t)
 '(company-tooltip-flip-when-above t)
 '(company-tooltip-limit 15)
 '(custom-safe-themes
   '("416e9537eccd2888c108a6ea5d1a2fff6a3e32bf3ddc33731bc303b7fecde67f" "70937c8612012bf9582da21de4e8ae89d0cd13f8eb90ceedb0a638dbc6aa92b7" "be8b0984e738655bf130e6b0d23dcdb70253bd2a85247fa37bd7e4252bf96c97" "79de6f796909e0ae1e528262adc205eacd4ea44852a2a737694df8dadd23ec89" "f03004fbcff53479b80cd335851a92f1ea9e912b46c41ae6528227349ffe78b0" default))
 '(deft-default-extension "org" t)
 '(deft-directory "~/notes/" t)
 '(deft-recursive t)
 '(deft-use-filename-as-title nil t)
 '(dumb-jump-force-searcher 'rg t)
 '(dumb-jump-prefer-searcher 'rg t)
 '(eldoc-idle-delay 0.01)
 '(elfeed-feeds '("https://habr.com/ru/rss/all/all/?fl=ru"))
 '(imenu-auto-rescan t)
 '(js-indent-level 2)
 '(js2-allow-rhino-new-expr-initializer nil t)
 '(js2-auto-indent-p t t)
 '(js2-concat-multiline-strings 'eol t)
 '(js2-enter-indents-newline nil t)
 '(js2-global-externs
   '("module" "require" "buster" "sinon" "assert" "refute" "setTimeout" "clearTimeout" "setInterval" "clearInterval" "location" "__dirname" "console" "JSON")
   t)
 '(js2-idle-timer-delay 0.1 t)
 '(js2-include-gears-externs nil t)
 '(js2-include-rhino-externs nil t)
 '(js2-indent-on-enter-key t t)
 '(js2-mirror-mode nil t)
 '(js2-rebind-eol-bol-keys nil t)
 '(js2-show-parse-errors nil t)
 '(js2-strict-inconsistent-return-warning nil t)
 '(js2-strict-missing-semi-warning nil t)
 '(js2-strict-trailing-comma-warning t t)
 '(lisp-body-indent 2)
 '(lpr-command "PDFToPrinter")
 '(magit-refresh-status-buffer nil)
 '(my-mc-cmds-to-run-once
   '(my-mark-all my-bob-or-mc-align my-eob-or-align-with-spaces my-mc-mark-like-this-or-edit-lines my-mc-mark-like-this-or-edit-lines toggle-input-method)
   t)
 '(org-agenda-files
   '("~/tasks-archive/task-archive.org" "c:/Users/hrams/AppData/Roaming/agenda.org"))
 '(org-capture-templates
   '(("d" "Target on Day" entry
      (file+headline "~/agenda.org" "Targets on Day")
      "* TODO %?
  SCHEDULED: %t
  
")
     ("w" "Target on Week" entry
      (file+headline "~/agenda.org" "Targets on Week")
      "* TODO %?
  
")
     ("f" "Film for See" entry
      (file+headline "~/agenda.org" "Films")
      #'my-films-format-as-org-heading))
   t)
 '(org-publish-list-skipped-files nil)
 '(org-publish-project-alist
   '(("Notes" :base-directory "~/notes/" :publishing-directory "~/notes/destination/" :publishing-function org-latex-publish-to-latex :recursive t :author "Семён" :language "ru")))
 '(org-publish-use-timestamps-flag nil)
 '(org-refile-targets '((org-agenda-files :maxlevel . 2)))
 '(org-refile-use-outline-path nil)
 '(org-safe-remote-resources
   '("\\`https://fniessen\\.github\\.io/org-html-themes/org/theme-bigblow\\.setup\\'"))
 '(org-startup-folded t)
 '(org-startup-indented t)
 '(org-startup-with-inline-images t)
 '(package-archives
   '(("org" . "https://orgmode.org/elpa/")
     ("melpa" . "https://melpa.org/packages/")
     ("gnu" . "https://elpa.gnu.org/packages/")))
 '(package-selected-packages
   '(xclip inspector org-appear org-modern toc-org org-cliplink helm-org-ql org-ql helm-org org-download goggles googles company-auctex company-math latex-extra laas cdlatex magic-latex-buffer math-preview xenops run-command-recipes flycheck-ghcmod ghc company-ghci hindent highlight-thing gruber-darker-theme doom-themes skeletor run-command helm-projectile projectile hl-todo git-modes blamer magit git-gutter dired-collapse dired-ranger dired-rainbow dired-open dired-filter deft js2-mode haskell-mode racket-mode suggest emr package-lint flycheck-package paxedit yasnippet smartparens visual-regexp deadgrep helm-swoop multiple-cursors imenu-anywhere format-all flycheck expand-region embrace eldoc dumb-jump rg drag-stuff company comment-dwim-2 avy aggressive-indent aas leaf-keywords org-ml pcache dash f s))
 '(projectile-completion-system 'helm)
 '(projectile-enable-caching nil)
 '(projectile-project-root-functions
   '(projectile-root-local my-project-root))
 '(projectile-project-search-path '("~/projects/"))
 '(python-shell-interpreter "python")
 '(racket-xp-mode-hook nil t)
 '(run-command-completion-method 'helm t)
 '(skeletor-completing-read-function 'completing-read-default t)
 '(skeletor-init-with-git nil t)
 '(skeletor-project-directory "~/projects" t)
 '(warning-suppress-types
   '(((unlock-file))
     (my)))
 '(web-mode-block-padding 0 t)
 '(web-mode-script-padding 1 t)
 '(yas-snippet-dirs '("~/.emacs.d/snippets"))
 '(yas-wrap-around-region t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(blamer-face
   ((t (:foreground "#7a88cf" :background nil :height 140 :italic t)))
   nil "Customized with leaf in `blamer' block")
 '(focus-unfocused
   ((t :inherit shadow))
   nil "Customized with leaf in `focus' block")
 '(region
   ((t (:background "white" :foreground "black" :inherit t)))))
