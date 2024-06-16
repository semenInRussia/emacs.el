;;; my-c.el --- My configuration of c and c++ languages -*- lexical-binding: t; -*-

;; Copyright (C) 2023, 2024 semenInRussia

;;; Commentary:

;; My configuration of c and c++ languages.  NOTE that I am often use
;; C++ for sport programming, so config for sport programming features
;; are located here.  (also my `yasnippets' is an important part of my
;; workflow, for example snippet ! - basic template for start any
;; problem)

;;; Code:

(require 'my-leaf)
(require 'f)
(require 's)


(defvar my-c-backend 'lsp
  "A symbol which tells to Emacs which one choose: LSP or ctags.")

(leaf cc-mode
  :config (leaf google-c-style
            :ensure (google-c-style :repo "google/styleguide" :host github)
            :hook ((c++-mode-hook c-mode-hook)   . google-set-c-style)))

(defun my-c-update-backend (backend &rest _ignore)
  "Change backend to a given BACKEND for C/C++ development.

Backend is either symbol tags or lsp"
  (leaf citre
    :when (equal backend 'tags)
    :remove-hook ((c++-mode-hook c-mode-hook) . my-lsp-ensure)
    :hook (c++-mode-hook c-mode-hook))

  (leaf eglot
    :when (equal backend 'lsp)
    :remove-hook ((c++-mode-hook c-mode-hook) . citre-mode)
    :hook ((c++-mode-hook c-mode-hook) . my-lsp-ensure)))

(my-c-update-backend my-c-backend)
(add-variable-watcher 'my-c-backend
                      #'my-c-update-backend)

(defun my-copy-whole-buffer-as-kill (&optional msg-p)
  "Copy the content of whole current buffer onto `kill-ring'.

If MSG-P is non-nil, say that content was copied."
  (interactive "p")
  (kill-new (buffer-string))
  (when msg-p
    (message "%s chars was COPIED!" (- (point-max) (point-min)))))

(defvar my-sport-map
  (define-keymap
    "C-i" #'my-sport-insert-samples
    "C-f" #'my-sport-find-samples-file
    "C-p" #'run-python
    "C-y" #'my-copy-whole-buffer-as-kill
    "C-w" #'my-copy-whole-buffer-as-kill))
(global-set-key (kbd "C-c ;") my-sport-map)

(defun my-sport-find-samples-file ()
  "Find input.txt file for current C++ file."
  (interactive)
  (find-file "input.txt"))

;; some settings to compile my C++ file using certain flags,
;; optimizations, warnings which are useful for Olympiad programming
(leaf run-command
  :after run-command cc-mode
  :defvar run-command-recipes
  :config
  (defun run-command-sportprog-recipe ()
    "A recipe for `run-command' useful to sport programming."
    (when (and (buffer-file-name)
               (eq major-mode 'emacs-lisp-mode)))
    (list
     (and
      (file-exists-p "input.txt")
      (list
       :display "Sport: compile, execute with input.txt [all flags]"
       :command-name "sport-execute-sample"
       :command-line
       (format
        "g++ %s -Wdisabled-optimization -Werror -g && cat input.txt | ./a.out"
        (buffer-file-name))))
     (list
      :display "Sport: execute only [all flags]"
      :command-name "sport-execute"
      :command-line
      (format
       "g++ %s -Wdisabled-optimization -Werror -g && ./a.out"
       (buffer-file-name)))
     (list
      :display "Sport: compile only [all flags]"
      :command-name "sport-compile"
      :command-line
      (format
       "g++ %s -Wdisabled-optimization -Werror -g"
       (buffer-file-name)))))
  (add-to-list 'run-command-recipes 'run-command-sportprog-recipe))

;; In sport programming I sometimes use debugger (gdb) or run
;; `eshell'.  When I run `gud-gdb' (see `my-realgud') I need to enter
;; all samples data (located in file input.txt) in one line.  The
;; following function do it.  I also can call it with (C-c ; C-i, it)
;; + it is like on (C-x i) which inserts content of the file.
(defun my-sport-insert-samples ()
  "And insert the content of the input.txt onto the buffer in one line."
  (interactive)
  (and
   (or (file-exists-p "input.txt")
       (user-error "File input.txt isn't exists, create it using C-c ; C-f (SPC ; f)"))
   (->>
    "input.txt"
    f-read-text
    (s-replace "\n" " ")
    insert)))

(provide 'my-c)
;;; my-c.el ends here
