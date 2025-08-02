;;; my-autoinsert.el --- My configuration of `autoinsert': automatically insert any initial text into empty files -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2025 semenInRussia

;;; Commentary:

;; My configuration of `autoinsert': insert a template text after file
;; is created.

;;; Code:

(require 'my-leaf)


(leaf autoinsert
  :custom ((auto-insert-alist .
                              '((c++-mode .
                                          (nil
                                           "// semenInRussia "
                                           (my-current-year)
                                           _)))))
  :global-minor-mode auto-insert-mode)

(provide 'my-autoinsert)
;;; my-autoinsert.el ends here
