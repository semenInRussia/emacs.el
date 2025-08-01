;;; my-c.el --- My configuration of c and c++ languages -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2025 semenInRussia

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


(setq-default c-basic-offset 2)

(defvar my-c-backend 'lsp
  "A symbol which tells to Emacs which one choose: LSP or ctags.")

(defun my-c-update-backend (backend &rest _ignore)
  "Change back end to a given BACKEND for C/C++ development.

Back end is either symbol tags or LSP"
  (leaf citre
    :when (equal backend 'tags)
    :remove-hook ((c++-mode-hook c-mode-hook) . my-lsp-ensure)
    :hook c++-mode-hook c-mode-hook)

  (leaf eglot
    :when (equal backend 'lsp)
    :remove-hook ((c++-mode-hook c-mode-hook) . citre-mode)
    :hook ((c++-mode-hook c-mode-hook) . my-lsp-ensure)))

(my-c-update-backend my-c-backend)
(add-variable-watcher 'my-c-backend #'my-c-update-backend)

(declare-function my-copy-whole-buffer-as-kill "my-sport-funcs")
(declare-function my-sport-insert-samples "my-sport-funcs")
(declare-function my-sport-find-samples-file "my-sport-funcs")
(defvar-keymap my-sport-map
  "C-f" #'my-sport-find-samples-file
  "C-i" #'my-sport-insert-samples
  "C-p" #'run-python
  "C-w" #'my-copy-whole-buffer-as-kill)
(global-set-key (kbd "C-c ;") my-sport-map)

;; some settings to compile my C++ file using certain flags,
;; optimizations, warnings which are useful for Olympiad programming
(leaf run-command
  :after run-command cc-mode
  :defvar run-command-recipes
  :config
  (defun run-command-sportprog-recipe ()
    "A recipe for `run-command' useful to sport programming."
    (when (and (buffer-file-name)
               (eq major-mode 'c++-mode))
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
        (format "g++ %s -Wdisabled-optimization -Werror -g && ./a.out"
                (buffer-file-name)))
       (list
        :display "Sport: compile only [all flags]"
        :command-name "sport-compile"
        :command-line
        (format
         "g++ %s -Wdisabled-optimization -Werror -g"
         (buffer-file-name))))))
  (add-to-list 'run-command-recipes 'run-command-sportprog-recipe))

(provide 'my-c)
;;; my-c.el ends here
