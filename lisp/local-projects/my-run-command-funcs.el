;;; my-run-command-funcs.el --- Some helper commands for `run-command' -*- lexical-binding: t -*-
;;; semenInRussia 2024

;;; Commentary:
;; Some helper commands for `run-command'.

;;; Code:
(declare-function run-command-core-run "run-command")

(defvar run-command-last-recipe nil
  "Last ran recipe of `run-command'.")

;;;###autoload
(defun my-run-command--run--set-last-recipe (recipe)
  "Set `run-command-last-recipe' to a given RECIPE."
  (setq-local run-command-last-recipe recipe))

;;;###autoload
(defun my-run-last-command ()
  "Run command which was ran last, if commands wasn't run do nothing."
  (interactive)
  (if run-command-last-recipe
      (run-command-core-run run-command-last-recipe)
    (message "NOT FOUND!")))

(provide 'my-run-command-funcs)
;;; my-run-command-funcs.el ends here
