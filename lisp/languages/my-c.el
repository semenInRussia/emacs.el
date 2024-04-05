;;; my-c.el --- My configuration of c and c++ languages -*- lexical-binding: t; -*-

;; Copyright (C) 2023 semenInRussia

;;; Commentary:

;; My configuration of c and c++ languages

;;; Code:
(require 'my-leaf)


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
       :display "Gcc: compile, execute with input.txt [all flags]"
       :command-name "sport-execute-sample"
       :command-line
       (format
        "g++ %s -Wdisabled-optimization -Wfloat-equal -Werror -g && cat input.txt | ./a.out"
        (buffer-file-name))))
     (list
      :display "Gcc: execute only [all flags]"
      :command-name "sport-execute"
      :command-line
      (format
       "g++ %s -Wdisabled-optimization -Wfloat-equal -Werror -g && ./a.out"
       (buffer-file-name)))
     (list
      :display "Gcc: compile only [all flags]"
      :command-name "sport-compile"
      :command-line
      (format
       "g++ %s -Wdisabled-optimization -Wfloat-equal -Werror -g"
       (buffer-file-name)))))

  (add-to-list 'run-command-recipes 'run-command-sportprog-recipe))

(provide 'my-c)
;;; my-c.el ends here
