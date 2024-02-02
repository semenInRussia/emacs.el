;;; my-multiple-cursors.el --- My configuration for the `multiple-cursors'

;; Copyright (C) 2022-2024 Semen Khramtsov

;; Author: Semen Khramtsov <hrams205@gmail.com>
;; Version: 0.1
;; URL: https://github.com/semenInRussia/emacs.el

;; This file is not part of GNU Emacs.

;;; Commentary:

;; My configuration for the `multiple-cursors'.  I often use multiple
;; cursors, because `meow-beacon' is norm.  But when I need to select
;; two, three words in buffer, i use C-, and C-< it's fast

;;; Code:
(require 'my-leaf)


(leaf multiple-cursors
  :ensure (multiple-cursors :repo "magnars/multiple-cursors.el" :host github)
  :bind (("M-i"       . 'mc/edit-lines)
         ("C-,"       . 'mc/mark-next-like-this-word)
         ("C-<"       . mc/mark-previous-like-this-word)))

(provide 'my-multiple-cursors)
;;; my-multiple-cursors.el ends here
