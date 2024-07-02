;;; my-jinx.el --- My configuration for jinx: a cool spell checker -*- lexical-binding: t; -*-

;; Copyright (C) 2024 semenInRussia

;; Author: semenInRussia <hrams205@gmail.com>
;; Version: 0.1
;; Homepage: https://github.com/semeninrussia/emacs.el

;;; Commentary:

;; My configuration for jinx: a cool spell checker.

;;; Code:

(require 'my-leaf)

(defvar my-enchant-executable "enchant" "A path to executable of the Enchant program.")

(defun my-jinx-ensure ()
  "Load `jinx' for current buffer."
  (interactive)
  ;; run it with idle timer.  It's useful, because in this case Emacs
  ;; don't need to load `jinx' instantly after somebody open a file,
  ;; so file will be opened more quickly and lsp will be activated
  ;; after some time
  (let ((buf (current-buffer)))
    (run-with-idle-timer 1 nil
                         (lambda ()
                           (when (buffer-live-p buf)
                             (with-current-buffer buf
                               (jinx-mode)))))))

(leaf jinx
  :ensure t
  :when (and (executable-find my-enchant-executable)
             (seq-find #'executable-find '("gcc" "clang" "cc")))
  :custom (jinx-languages . "ru_RU en")
  :commands jinx-mode
  :bind (("M-$" . jinx-correct)
         (:meow-normal-state-keymap
          :package meow-core
          ("$" . jinx-correct)))
  :hook ((text-mode-hook . my-jinx-ensure)
         (prog-mode-hook . my-jinx-ensure)))

;;; my-jinx.el ends here
(provide 'my-jinx)
