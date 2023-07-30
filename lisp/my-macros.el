;;; my-macros.el --- Some useful macros which was inspide with Doomemacs -*- lexical-binding: t -*-
;; Copyright (C) 2023 semenInRussia

;;; Commentary:

;; Define some useful macros, each of them ends with ! (inspired with Doomemacs).
;;
;; NOTE that here I don't use the libraries like `dash' or `s', because the
;; macros must can be used in init.el

;;; Code:

(defmacro remove-from-list! (list-var &rest elements)
  "Add ELEMENTS to LIST-VAR.

If element is already inside LIST-VAR, then don't add.  NOTE that LIST-VAR
should be quoted."
  (cons
   'progn
   (mapcar
    (lambda (el)
      `(setq ,list-var (delete ,el ,list-var)))
    elements)))

(defmacro add-to-list! (list-var &rest elements)
  "Add ELEMENTS to LIST-VAR.

If element is already inside LIST-VAR, then don't add.  NOTE that LIST-VAR
should be quoted."
  (cons
   'progn
   (mapcar
    (lambda (el)
      `(add-to-list ,list-var ,el))
    elements)))

(provide 'my-macros)
;;; my-macros.el ends here
