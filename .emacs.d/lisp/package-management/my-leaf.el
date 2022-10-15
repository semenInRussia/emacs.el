;;; my-leaf.el --- My config for `leaf' -*- lexical-binding: t; -*-

;; Copyright (C) 2022 semenInRussia

;; Author: semenInRussia <hrams205@gmail.com>
;; Version: 0.1

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; My config for `leaf'.

;;; Code:

(require 'package)

(eval-and-compile
  (straight-use-package 'leaf)

  (setq leaf-alias-keyword-alist
        (--remove-first
         (eq (car it) :ensure)
         leaf-alias-keyword-alist))

  (add-to-list 'leaf-alias-keyword-alist '(:ensure . :straight))

  (leaf leaf-keywords :ensure t :config (leaf-keywords-init))
  ;; key binding for `leaf-find' you can find in `my-writing-config'
  )

(defun my-plist-get (plist prop &optional default)
  "Wrapper on `plist-get', difference is that, if first elt isn't keyword work.

Also take 3rd optional argument DEFAULT, which will be return if PROP is not
found in PLIST."
  (unless (keywordp (car plist))
    (setq plist (cdr plist)))
  (or (plist-get plist prop) default))

(defvar my-leaf-keywords-before-conditions
  (leaf-list
   :major-mode-map (let*
                       ((arguments (car leaf--value))
                        name major-modes parent)
                     (cond
                      ((eq arguments t)
                       (setq name leaf--name)
                       (setq major-modes (list name)))
                      ((symbolp arguments)
                       (setq name arguments)
                       (setq major-modes (list leaf--name)))
                      ((listp arguments)
                       (setq name
                             (my-plist-get
                              arguments :name
                              ;; nofmt
                              (cl-find-if
                               (lambda (x)
                                 (and (symbolp x) ;nofmt
                                      (not (keywordp x))))
                               arguments)))
                       (setq major-modes
                             (my-plist-get arguments :major-modes
                                           (or
                                            (cl-find-if 'listp
                                                        ;;nofmt
                                                        arguments)
                                            (list name))))
                       (setq parent ;nofmt
                             (my-plist-get arguments :parent)))
                      (t (leaf-error ;nofmt
                          "Expected eiter `symbol', t or `list'")))
                     `((eval-after-load 'xah-fly-keys
                         '(my-define-local-major-mode-map
                           ',name ',major-modes ',parent))
                       ,@leaf--body))))

(defvar my-leaf-keywords-after-require
  (leaf-list
   :fast-exec (let*
                  ((arguments (car leaf--value))
                   (name leaf--name)
                   (bindings
                    (if (consp (car arguments)) arguments (list arguments))))
                (--each bindings
                  (unless (eq (-second-item it) 'quote)
                    (leaf-register-autoload (-second-item it) leaf--name)))
                `((eval-after-load 'fast-exec
                    '(fast-exec-bind ,name
                       (fast-exec-make-some-commands ,@bindings)))
                  ,@leaf--body))))

(defvar my-leaf-keywords-after-config
  (leaf-list
   :aas (let*
            ((arguments (car leaf--value))
             (has-special-keymap
              (and
               (symbolp (car arguments))
               ;; this mean :cond
               (not (keywordp (car arguments)))))
             (keymap (if has-special-keymap (car arguments) leaf--name))
             (bindings (if has-special-keymap (cdr arguments) arguments)))
          `((eval-after-load 'aas
              '(aas-set-snippets ',keymap ,@bindings))
            ,@leaf--body))))

(defun my-flatten-list (lst)
  "LST of the list of the list ... to list of the atom elements.

This is the version of `flatten-list', which isn't change ' to quote symbol."
  (delete 'quote (flatten-list lst)))

(defun my-leaf-keywords-init ()
  "Init of my keywords for `leaf'."
  ;; :preface <this place> :when, :unless
  (setq leaf-keywords-before-conditions
        (append leaf-keywords-before-conditions
                my-leaf-keywords-before-conditions))

  ;; :config <this place> :setq
  (setq leaf-keywords-after-config
        (append leaf-keywords-after-config
                my-leaf-keywords-after-config))

  (setq leaf-keywords-after-require
        (append leaf-keywords-after-require
                my-leaf-keywords-after-require))

  (leaf-keywords-init))

(my-leaf-keywords-init)

(provide 'my-leaf)
;;; my-leaf.el ends here
