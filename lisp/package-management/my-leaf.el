;;; my-leaf.el --- My config for `leaf' -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2023 semenInRussia

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

(require 'cl-lib)
(require 'pam)

(eval-when-compile (require 'fast-exec))
(declare-function straight-use-package "straight")

(eval-and-compile
  ;; `eval-and-compile' really install the package in compile time,
  ;; it's important, because the rest config use `leaf' macro
  (pam-use-package '(leaf :repo "conao3/leaf.el"))
  (require 'leaf)

  (defun my-leaf-keywords-init ()
    "Initialize keywords for macro `leaf'."
    (setq leaf-keywords
          '(:disabled
            (unless
                (eval
                 (car leaf--value))
              `(,@leaf--body))
            :leaf-path
            (if
                (and leaf--body
                     (eval
                      (car leaf--value)))
                `((leaf-handler-leaf-path ,leaf--name)
                  ,@leaf--body)
              `(,@leaf--body))
            :leaf-protect
            (if
                (and leaf--body
                     (eval
                      (car leaf--value)))
                `((leaf-handler-leaf-protect ,leaf--name ,@leaf--body))
              `(,@leaf--body))
            :load-path
            `(,@(mapcar
                 (lambda
                   (elm)
                   `(add-to-list 'load-path ,elm))
                 leaf--value)
              ,@leaf--body)
            :load-path*
            `(,@(mapcar
                 (lambda
                   (elm)
                   `(add-to-list 'load-path
                                 (locate-user-emacs-file ,elm)))
                 leaf--value)
              ,@leaf--body)
            :leaf-autoload
            `(,@(when
                    (car leaf--value)
                  (mapcar
                   (lambda
                     (elm)
                     `(unless
                          (fboundp ',(car elm))
                        (autoload #',(car elm)
                          ,(cdr elm)
                          nil t)))
                   (reverse leaf--autoload)))
              ,@leaf--body)
            :defun
            `(,@(mapcar
                 (lambda
                   (elm)
                   `(declare-function ,(car elm)
                                      ,(symbol-name
                                        (cdr elm))))
                 leaf--value)
              ,@leaf--body)
            :defvar
            `(,@(mapcar
                 (lambda
                   (elm)
                   `(defvar ,elm))
                 leaf--value)
              ,@leaf--body)
            :leaf-defun
            `(,@(when
                    (car leaf--value)
                  (mapcar
                   (lambda
                     (elm)
                     `(declare-function ,(car elm)
                                        ,(cdr elm)))
                   (reverse leaf--autoload)))
              ,@leaf--body)
            :leaf-defvar
            `(,@(mapcar
                 (lambda
                   (elm)
                   `(defvar ,elm))
                 leaf--value)
              ,@leaf--body)
            :preface
            `(,@leaf--value ,@leaf--body)
            :when
            (when leaf--body
              `((when ,@(if
                            (= 1
                               (length leaf--value))
                            leaf--value
                          `((and ,@leaf--value)))
                  ,@leaf--body)))
            :unless
            (when leaf--body
              `((unless ,@(if
                              (= 1
                                 (length leaf--value))
                              leaf--value
                            `((and ,@leaf--value)))
                  ,@leaf--body)))
            :if
            (when leaf--body
              `((if ,@(if
                          (= 1
                             (length leaf--value))
                          leaf--value
                        `((and ,@leaf--value)))
                    (progn ,@leaf--body))))
            :doc
            `(,@leaf--body)
            :req
            `(,@leaf--body)
            :tag
            `(,@leaf--body)
            :comment
            `(,@leaf--body)
            :file
            `(,@leaf--body)
            :url
            `(,@leaf--body)
            :added
            `(,@leaf--body)
            :emacs<
            (when leaf--body
              `((when
                    (version< emacs-version ,leaf--value)
                  ,@leaf--body)))
            :emacs<=
            (when leaf--body
              `((when
                    (version<= emacs-version ,leaf--value)
                  ,@leaf--body)))
            :emacs=
            (when leaf--body
              `((when
                    (version= emacs-version ,leaf--value)
                  ,@leaf--body)))
            :emacs>
            (when leaf--body
              `((when
                    (version< ,leaf--value emacs-version)
                  ,@leaf--body)))
            :emacs>=
            (when leaf--body
              `((when
                    (version<= ,leaf--value emacs-version)
                  ,@leaf--body)))
            :package
            `(,@(mapcar
                 (lambda
                   (elm)
                   `(eval-and-compile
                      (leaf-handler-package ,leaf--name ,(car elm)
                                            ,(cdr elm))))
                 leaf--value)
              ,@leaf--body)
            :feather
            `(,@(mapcar
                 (lambda
                   (elm)
                   `(leaf-handler-package ,leaf--name ,(car elm)
                                          ,(cdr elm)))
                 leaf--value)
              (feather-add-after-installed-hook-sexp ,(caar
                                                       (last leaf--value))
                                                     ,@leaf--body))
            :straight
            `(,@(mapcar (lambda (elm)
	        	  `(eval-and-compile
	        	     (straight-use-package ',(if (eq elm t)
                                                         leaf--name
                                                       elm))))
                        leaf--value)
	      ,@leaf--body)

            :pam
            `(,@(mapcar (lambda (elm)
	        	  `(eval-and-compile
	        	     (pam-use-package ',(if (eq elm t)
                                                    leaf--name
                                                  elm))))
                        leaf--value)
	      ,@leaf--body)
            :pie
            `(,@(mapcar (lambda (elm)
                          `(eval-and-compile
                             (my-pie-recipe ',(if (eq elm t)
                                                  leaf--name
                                                elm))))
                        leaf--value)
              ,@leaf--body)
            :el-get
            `(,@(mapcar
                 (lambda
                   (elm)
                   `(el-get-bundle ,@elm))
                 leaf--value)
              ,@leaf--body)
            :ensure-system-package
            `(,@(mapcar
                 (lambda
                   (elm)
                   (let
                       ((a
                         (car elm))
                        (d
                         (cdr elm)))
                     (cond
                      ((null d)
                       `(unless
                            (executable-find ,(symbol-name a))
                          (system-packages-install ,(symbol-name a))))
                      ((symbolp d)
                       `(unless ,(if
                                     (stringp a)
                                     `(file-exists-p ,a)
                                   `(executable-find ,(symbol-name a)))
                          (system-packages-install ,(symbol-name d))))
                      ((stringp d)
                       `(unless ,(if
                                     (stringp a)
                                     `(file-exists-p ,a)
                                   `(executable-find ,(symbol-name a)))
                          (async-shell-command ,d))))))
                 leaf--value)
              ,@leaf--body)
            :after
            (when leaf--body
              (let
                  ((ret
                    `(progn ,@leaf--body)))
                (dolist
                    (elm leaf--value)
                  (setq ret
                        `(eval-after-load ',elm ',ret)))
                `(,ret)))
            :commands
            (progn
              (leaf-register-autoload leaf--value leaf--name)
              `(,@leaf--body))
            :bind
            (progn
              (leaf-register-autoload
               (cadr leaf--value)
               leaf--name)
              `((leaf-keys ,(car leaf--value))
                ,@leaf--body))
            :bind*
            (progn
              (leaf-register-autoload
               (cadr leaf--value)
               leaf--name)
              `((leaf-keys* ,(car leaf--value))
                ,@leaf--body))
            :bind-keymap
            `((leaf-keys-bind-keymap ,(car leaf--value)
                                     nil ',leaf--name)
              ,@leaf--body)
            :bind-keymap*
            `((leaf-keys-bind-keymap* ,(car leaf--value)
                                      nil ',leaf--name)
              ,@leaf--body)
            :mode
            (progn
              (leaf-register-autoload
               (mapcar #'cdr leaf--value)
               leaf--name)
              `(,@(mapcar
                   (lambda
                     (elm)
                     `(add-to-list 'auto-mode-alist
                                   '(,(car elm)
                                     \,
                                     (cdr elm))))
                   leaf--value)
                ,@leaf--body))
            :interpreter
            (progn
              (leaf-register-autoload
               (mapcar #'cdr leaf--value)
               leaf--name)
              `(,@(mapcar
                   (lambda
                     (elm)
                     `(add-to-list 'interpreter-mode-alist
                                   '(,(car elm)
                                     \,
                                     (cdr elm))))
                   leaf--value)
                ,@leaf--body))
            :magic
            (progn
              (leaf-register-autoload
               (mapcar #'cdr leaf--value)
               leaf--name)
              `(,@(mapcar
                   (lambda
                     (elm)
                     `(add-to-list 'magic-mode-alist
                                   '(,(car elm)
                                     \,
                                     (cdr elm))))
                   leaf--value)
                ,@leaf--body))
            :magic-fallback
            (progn
              (leaf-register-autoload
               (mapcar #'cdr leaf--value)
               leaf--name)
              `(,@(mapcar
                   (lambda
                     (elm)
                     `(add-to-list 'magic-fallback-mode-alist
                                   '(,(car elm)
                                     \,
                                     (cdr elm))))
                   leaf--value)
                ,@leaf--body))
            :hook
            (progn
              (leaf-register-autoload
               (mapcar #'cdr leaf--value)
               leaf--name)
              `(,@(mapcar
                   (lambda
                     (elm)
                     `(add-hook ',(car elm)
                                #',(cdr elm)))
                   leaf--value)
                ,@leaf--body))
            :advice
            (progn
              (leaf-register-autoload
               (cadr leaf--value)
               leaf--name)
              `(,@(mapcar
                   (lambda
                     (elm)
                     `(advice-add ,@elm))
                   (car leaf--value))
                ,@leaf--body))
            :advice-remove
            (progn
              (leaf-register-autoload
               (cadr leaf--value)
               leaf--name)
              `(,@(mapcar
                   (lambda
                     (elm)
                     `(advice-remove ,@elm))
                   (car leaf--value))
                ,@leaf--body))
            :pre-setq
            `(,@(mapcar
                 (lambda
                   (elm)
                   `(setq ,(car elm)
                          ,(cdr elm)))
                 leaf--value)
              ,@leaf--body)
            :pre-setf
            `(,@(mapcar
                 (lambda
                   (elm)
                   `(setf ,(car elm)
                          ,(cdr elm)))
                 leaf--value)
              ,@leaf--body)
            :pre-push
            `(,@(mapcar
                 (lambda
                   (elm)
                   `(push ,(cdr elm)
                          ,(car elm)))
                 leaf--value)
              ,@leaf--body)
            :pl-pre-setq
            `(,@(mapcar
                 (lambda
                   (elm)
                   `(setq ,(car elm)
                          (leaf-handler-auth ,leaf--name ,(car elm)
                                             ,(cdr elm))))
                 leaf--value)
              ,@leaf--body)
            :auth-pre-setq
            `(,@(mapcar
                 (lambda
                   (elm)
                   `(setq ,(car elm)
                          (leaf-handler-auth ,leaf--name ,(car elm)
                                             ,(cdr elm))))
                 leaf--value)
              ,@leaf--body)
            :custom
            `(,@(mapcar
                 (lambda
                   (elm)
                   `(customize-set-variable ',(car elm)
                                            ,(cdr elm)
                                            ,(leaf--create-custom-comment :custom)))
                 leaf--value)
              ,@leaf--body)
            :custom*
            `(,@(mapcar
                 (lambda
                   (elm)
                   `(customize-set-variable ',(car elm)
                                            ,(cdr elm)
                                            ,(leaf--create-custom-comment :custom*)))
                 leaf--value)
              ,@leaf--body)
            :pl-custom
            `(,@(mapcar
                 (lambda
                   (elm)
                   `(customize-set-variable ',(car elm)
                                            (leaf-handler-auth ,leaf--name ,(car elm)
                                                               ,(cdr elm))
                                            ,(leaf--create-custom-comment :pl-custom
                                                                          (cdr elm))))
                 leaf--value)
              ,@leaf--body)
            :auth-custom
            `(,@(mapcar
                 (lambda
                   (elm)
                   `(customize-set-variable ',(car elm)
                                            (leaf-handler-auth ,leaf--name ,(car elm)
                                                               ,(cdr elm))
                                            ,(leaf--create-custom-comment :auth-custom
                                                                          (cdr elm))))
                 leaf--value)
              ,@leaf--body)
            :custom-face
            `((custom-set-faces ,@(mapcar
                                   (lambda
                                     (elm)
                                     `'(,(car elm)
                                        ,(car
                                          (cddr elm))
                                        nil ,(leaf--create-custom-comment :custom-face)))
                                   leaf--value))
              ,@leaf--body)
            :init
            `(,@leaf--value ,@leaf--body)
            :hydra
            (progn
              (leaf-register-autoload
               (cadr leaf--value)
               leaf--name)
              `(,@(mapcar
                   (lambda
                     (elm)
                     `(defhydra ,@elm))
                   (car leaf--value))
                ,@leaf--body))
            :mode-hydra
            (progn
              (leaf-register-autoload
               (cadr leaf--value)
               leaf--name)
              `(,@(mapcar
                   (lambda
                     (elm)
                     `(major-mode-hydra-define+ ,@elm))
                   (car leaf--value))
                ,@leaf--body))
            :pretty-hydra
            (progn
              (leaf-register-autoload
               (cadr leaf--value)
               leaf--name)
              `(,@(mapcar
                   (lambda
                     (elm)
                     `(pretty-hydra-define+ ,@elm))
                   (car leaf--value))
                ,@leaf--body))
            :transient
            (progn
              `(,@(mapcar
                   (lambda
                     (elm)
                     `(transient-define-prefix ,@elm))
                   (car leaf--value))
                ,@leaf--body))
            :combo
            (progn
              (leaf-register-autoload
               (cadr leaf--value)
               leaf--name)
              `(,@(mapcar
                   (lambda
                     (elm)
                     `(key-combo-define ,@elm))
                   (car leaf--value))
                ,@leaf--body))
            :combo*
            (progn
              (leaf-register-autoload
               (cadr leaf--value)
               leaf--name)
              `(,@(mapcar
                   (lambda
                     (elm)
                     `(key-combo-define ,@elm))
                   (car leaf--value))
                ,@leaf--body))
            :smartrep
            (progn
              (leaf-register-autoload
               (cadr leaf--value)
               leaf--name)
              `(,@(mapcar
                   (lambda
                     (elm)
                     `(smartrep-define-key ,@elm))
                   (car leaf--value))
                ,@leaf--body))
            :smartrep*
            (progn
              (leaf-register-autoload
               (cadr leaf--value)
               leaf--name)
              `(,@(mapcar
                   (lambda
                     (elm)
                     `(smartrep-define-key ,@elm))
                   (car leaf--value))
                ,@leaf--body))
            :chord
            (progn
              (leaf-register-autoload
               (cadr leaf--value)
               leaf--name)
              `((leaf-key-chords ,(car leaf--value))
                ,@leaf--body))
            :chord*
            (progn
              (leaf-register-autoload
               (cadr leaf--value)
               leaf--name)
              `((leaf-key-chords* ,(car leaf--value))
                ,@leaf--body))
            :mode-hook
            `(,@(mapcar
                 (lambda
                   (elm)
                   `(leaf-keywords-handler-mode-hook ,leaf--name ,(car elm)
                                                     ,@(cadr elm)))
                 leaf--value)
              ,@leaf--body)
            :require
            `(,@(mapcar
                 (lambda
                   (elm)
                   `(require ',elm))
                 leaf--value)
              ,@leaf--body)
            :global-minor-mode
            (progn
              (mapc
               (lambda
                 (elm)
                 (leaf-register-autoload
                  (car elm)
                  (cdr elm)))
               leaf--value)
              `(,@(mapcar
                   (lambda
                     (elm)
                     `(,(car elm)
                       1))
                   leaf--value)
                ,@leaf--body))
            :delight
            `(,@(mapcar
                 (lambda
                   (elm)
                   `(delight ,@elm))
                 leaf--value)
              ,@leaf--body)
            :diminish
            `((with-eval-after-load ',leaf--name ,@(mapcar
                                                    (lambda
                                                      (elm)
                                                      `(diminish ',(car elm)
                                                                 ,(cdr elm)))
                                                    leaf--value))
              ,@leaf--body)
            :blackout
            `((with-eval-after-load ',leaf--name ,@(mapcar
                                                    (lambda
                                                      (elm)
                                                      `(blackout ',(car elm)
                                                                 ,(cdr elm)))
                                                    leaf--value))
              ,@leaf--body)
            :grugru
            `((grugru-define-multiple ,@leaf--value)
              ,@leaf--body)
            :fast-exec
            (let*
                ((arguments
                  (car leaf--value))
                 (name leaf--name)
                 (bindings
                  (if
                      (consp
                       (car arguments))
                      arguments
                    (list arguments))))
              (--each bindings
                (unless
                    (eq
                     (-second-item it)
                     'quote)
                  (leaf-register-autoload
                   (-second-item it)
                   leaf--name)))
              `((with-eval-after-load 'fast-exec
                  ;; `require' macros needed in `eval-and-compile'
                  (eval-and-compile (require 'fast-exec))
                  (fast-exec-bind ',name
                                  (fast-exec-make-some-commands ,@bindings)))
                ,@leaf--body))
            :leaf-defer
            (let*
                ((eval-after-p
                  (and leaf--body
                       (eval
                        (car leaf--value))
                       (leaf-list-memq leaf-defer-keywords
                                       (leaf-plist-keys leaf--raw))))
                 (file
                  (leaf-this-file))
                 (let-or-progn
                  (if file
                      `(let
                           ((leaf--load-file-name ,file)))
                    '(progn))))
              (if eval-after-p
                  `((eval-after-load ',leaf--name
                      '(,@let-or-progn ,@leaf--body)))
                `(,@leaf--body)))
            :aas
            (let*
                ((arguments
                  (car leaf--value))
                 (has-special-keymap
                  (and
                   (symbolp
                    (car arguments))
                   (not
                    (keywordp
                     (car arguments)))))
                 (keymap
                  (if has-special-keymap
                      (car arguments)
                    leaf--name))
                 (bindings
                  (if has-special-keymap
                      (cdr arguments)
                    arguments)))
              `((eval-after-load 'aas
                  '(aas-set-snippets ',keymap ,@bindings))
                ,@leaf--body))
            :setq
            `(,@(mapcar
                 (lambda
                   (elm)
                   `(setq ,(car elm)
                          ,(cdr elm)))
                 leaf--value)
              ,@leaf--body)
            :setq-default
            `(,@(mapcar
                 (lambda
                   (elm)
                   `(setq-default ,(car elm)
                                  ,(cdr elm)))
                 leaf--value)
              ,@leaf--body)
            :setf
            `(,@(mapcar
                 (lambda
                   (elm)
                   `(setf ,(car elm)
                          ,(cdr elm)))
                 leaf--value)
              ,@leaf--body)
            :push
            `(,@(mapcar
                 (lambda
                   (elm)
                   `(push ,(cdr elm)
                          ,(car elm)))
                 leaf--value)
              ,@leaf--body)
            :pl-setq
            `(,@(mapcar
                 (lambda
                   (elm)
                   `(setq ,(car elm)
                          (leaf-handler-auth ,leaf--name ,(car elm)
                                             ,(cdr elm))))
                 leaf--value)
              ,@leaf--body)
            :auth-setq
            `(,@(mapcar
                 (lambda
                   (elm)
                   `(setq ,(car elm)
                          (leaf-handler-auth ,leaf--name ,(car elm)
                                             ,(cdr elm))))
                 leaf--value)
              ,@leaf--body)
            :pl-setq-default
            `(,@(mapcar
                 (lambda
                   (elm)
                   `(setq-default ,(car elm)
                                  (leaf-handler-auth ,leaf--name ,(car elm)
                                                     ,(cdr elm))))
                 leaf--value)
              ,@leaf--body)
            :auth-setq-default
            `(,@(mapcar
                 (lambda
                   (elm)
                   `(setq-default ,(car elm)
                                  (leaf-handler-auth ,leaf--name ,(car elm)
                                                     ,(cdr elm))))
                 leaf--value)
              ,@leaf--body)
            :config
            `(,@leaf--value ,@leaf--body)
            :defer-config
            `((eval-after-load ',leaf--name
                '(progn ,@leaf--value))
              ,@leaf--body)))
    (setq leaf-alias-keyword-alist '(;; use my small PAckage Manager called
                                     ;; `pam' which is built over `straight'
                                     (:ensure . :pam)
                                     ;; I prefer use `setq' over `custom-set'
                                     ;;
                                     ;; the main reason is speed, it speed up my
                                     ;; config in 2.4 times!!! (17secs => 7secs)
                                     (:custom . :setq))))

  (my-leaf-keywords-init))

(provide 'my-leaf)
;;; my-leaf.el ends here
