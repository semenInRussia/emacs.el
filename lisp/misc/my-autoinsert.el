;;; my-autoinsert.el --- My configuration of `autoinsert': automatically insert any initial text into empty files -*- lexical-binding: t; -*-

;; Copyright (C) 2023 semenInRussia
;; Author: semenInRussia <hrams205@gmail.com>
;; Version: 0.1
;; Homepage: https://github.com/semeninrussia/emacs.el

;;; Commentary:

;; My configuration of `autoinsert'.

;;; Code:

(require 'my-leaf)


(leaf autoinsert
  :custom ((auto-insert-alist .
                              '((c++-mode .
                                          (nil
                                           "// Copyright "
                                           (my-current-year)
                                           " semenInRussia"
                                           _)))))
  :global-minor-mode auto-insert-mode)

(provide 'my-autoinsert)
;;; my-autoinsert.el ends here
