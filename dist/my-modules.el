;;; my-straight.el --- My config for support of the `straight' -*- lexical-binding: t; -*-

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

;; My config for support of the `straight'.

;;; Code:

(defvar bootstrap-version)

(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer (url-retrieve-synchronously
                          "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
                          'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'org)



;;; my-straight.el ends here
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

(declare-function 'straight-use-package "straight")

(require 'cl-lib)


(straight-use-package 'leaf)
(require 'leaf)

(defun my-plist-get (plst key &optional def)
  "Return the value at KEY in PLST, if key isn't provided return DEF."
  (let (prev-key
        prev-val
        (result def))
    (unless (keywordp (car plst))
      (setq plst (cdr plst)))
    (while plst
      (when (eq prev-key key)
        (setq result prev-val)
        (setq plst nil)                 ; stop loop
        )
      (setq prev-key (car plst))
      (setq prev-val (cadr plst))
      ;; skip both: value and key
      (setq plst (cddr plst)))
    (when (eq prev-key key)
      (setq result prev-val))
    result))

(defun my-leaf-keywords-init ()
  "Initialize keywords for macro `leaf'."
  (setq leaf-alias-keyword-alist '((:ensure . :straight)))
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
          :major-mode-map
          (let*
              ((arguments
                (car leaf--value))
               name major-modes parent)
            (cond
             ((eq arguments t)
              (setq name leaf--name)
              (setq major-modes
                    (list name)))
             ((symbolp arguments)
              (setq name arguments)
              (setq major-modes
                    (list leaf--name)))
             ((listp arguments)
              (setq name
                    (my-plist-get arguments :name
                                  (cl-find-if
                                   (lambda
                                     (x)
                                     (and
                                      (symbolp x)
                                      (not
                                       (keywordp x))))
                                   arguments)))
              (setq major-modes
                    (my-plist-get arguments :major-modes
                                  (or
                                   (cl-find-if 'listp arguments)
                                   (list name))))
              (setq parent
                    (my-plist-get arguments :parent)))
             (t
              (leaf-error "Expected eiter `symbol', t or `list'")))
            `((eval-after-load 'xah-fly-keys
                '(my-define-local-major-mode-map ',name ',major-modes ',parent))
              ,@leaf--body))
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
                 `(leaf-handler-package ,leaf--name ,(car elm)
                                        ,(cdr elm)))
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
          `(,@(mapcar (lambda (elm) `(straight-use-package ',(if (eq elm t)
                                                                 leaf--name
                                                               elm)))
                      leaf--value) ,@leaf--body)
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
            `((progn
                (fast-exec-bind ',name
                  (fast-exec-make-some-commands ,@bindings))
                ,@leaf--body)))
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
            ,@leaf--body))))

(my-leaf-keywords-init)

(defun my-flatten-list (lst)
  "LST of the lists of the lists ... to list of the atom elements.

This is a version of `flatten-list', but it isn't change \\=' to \\='quote."
  (delete 'quote (flatten-list lst)))



;;; my-leaf.el ends here
;;; my-projects.el --- My source code loading my projects Elisp files

;; Copyright (C) 2022 Semen Khramtsov

;; Author: Semen Khramtsov <hrams205@gmail.com>
;; Version: 0.1
;; URL: https://github.com/semenInRussia/emacs.el

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; My source code loading my projects Elisp files

;;; Code:

(add-to-list 'load-path "~/projects/fast-exec.el")
(add-to-list 'load-path "~/projects/simple-indention.el")

(add-to-list 'load-path (locate-user-emacs-file "site-lisp"))



;;; my-projects.el ends here
;;; my-libs.el --- Some libraries

;; Copyright (C) 2022 Semen Khramtsov

;; Author: Semen Khramtsov <hrams205@gmail.com>
;; Version: 0.1
;; URL: https://github.com/semenInRussia/emacs.el

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Some libraries

;;; Code:



(leaf s :ensure t :require t)

(leaf f :ensure t :require t)

(leaf dash :ensure t :global-minor-mode global-dash-fontify-mode :require t)

(leaf just                              ;nofmt
  :ensure (just :host github :repo "semenInRussia/just.el")
  :require t)

(leaf queue :ensure t)

(leaf request :ensure t)

(leaf async :ensure t)



;;; my-libs.el ends here
;;; my-lib.el --- My small library

;; Copyright (C) 2022 Semen Khramtsov

;; Author: Semen Khramtsov <hrams205@gmail.com>
;; Version: 0.1
;; URL: https://github.com/semenInRussia/emacs.el

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; My small library

;;; Code:

(declare-function sp-get-enclosing-sexp "smartparens")
(declare-function sp-get-sexp "smartparens")
(declare-function sp-get "smartparens")

(require 'dash)
(require 's)
(require 'f)
(require 'just)

(defun my-alist-p (obj)
  "Return t, when OBJ is `alist'."
  (and
   (listp obj)
   (not (null obj))
   (consp (car obj))))

(defun my-symbol-append (&rest symbols)
  "Get symbol which has `symbol-name' as concatenation of the each of SYMBOLS."
  (->> symbols (-map 'symbol-name) (apply 's-concat) (intern)))

(defun my-major-mode-to-hook (major-mode)
  "Return hook for MAJOR-MODE: python-mode => python-mode-hook."
  (my-symbol-append major-mode '-hook))

(defun my-major-mode-to-map (major-mode)
  "Return map for MAJOR-MODE: python-mode => python-mode-map."
  (my-symbol-append major-mode '-map))

(defun my-map-to-major-mode (map)
  "Return `major-mode' of MAP: python-mode-map => `python-mode'."
  (->> map (symbol-name) (s-chop-suffix "-map") (intern)))

(defun my-max (lst) "Max of the LST." (and lst (apply 'max lst)))

(defun my-humanize-string (str)
  "Humanaize STR.  For example: just-word -> Just Word."
  (->> str (s-replace "-" " ") (s-titleized-words)))

(defun my-normalize-string (str)
  "Humanaize STR for computer.  For example: Just Word -> just-word."
  (->> str (s-replace " " "-") (s-downcase)))

(defun select-current-line ()
  "Select as region current line."
  (interactive)
  (just-mark-region-between-movements 'beginning-of-line
                                      'end-of-line))

(defun my-call-interactivelly-or-funcall (symbol)
  "If SYMBOL function is command, `call-interactively', otherwise `funcall'."
  (if (commandp symbol) (call-interactively symbol) (funcall symbol)))

(defmacro define-key-when (fun-name map key def pred)
  "Define to KEY in MAP DEF when PRED return t or run old command.

Instead of KEY will command FUN-NAME"
  (declare (indent 0))
  (let ((old-def (lookup-key (eval map) (eval key))))
    `(unless (eq
              (lookup-key ,map ,key)
              ',fun-name)
       (defun ,fun-name ()
         ,(s-lex-format "Run `${old-def}' or `${def}'.")
         (interactive)
         (if (my-call-interactivelly-or-funcall ,pred)
             (my-call-interactivelly-or-funcall ,def)
           (my-call-interactivelly-or-funcall ',old-def)))
       (define-key ,map ,key #',fun-name))))

(defun repeat-at-last-keystroke ()
  "Define in the tempory keymap at last pressed keystroke `this-command'."
  (one-shot-keybinding
   (char-to-string (event-basic-type last-input-event))
   this-command))

(defun one-shot-keybinding (key command)
  "Bind KEY with COMMAND to one key hitting."
  (set-transient-map
   (let ((map (make-sparse-keymap)))
     (define-key map (kbd key) command)
     map)
   t))

(defun my-use-skeleton (skeleton-path destination replacements)
  "Copy SKELETON-PATH to DESTINATION and do REPLACEMENTS."
  (f-copy skeleton-path destination)
  (my-replace-all-in-dir replacements destination))

(defun my-replace-all-in-dir (replacements dir)
  "Replace each `car' of REPLACEMENTS to respective `cdr' in each file of DIR."
  (->
   dir
   (f-files)
   (--each (my-replace-all-in-file replacements it))))

(defun my-replace-all-in-file (replacements filename)
  "Replace each `car' of REPLACEMENTS to respective `cdr' in file at FILENAME."
  (f-write
   (s-replace-all replacements (f-read filename))
   'utf-8
   filename))

(defun my-goto-lisp-sexp-begin (start-name)
  "Go to backward beginning of Lisp sexp which start with START-NAME."
  (--when-let
      (search-backward-regexp
       (rx "(" (zero-or-more whitespace) (regexp start-name))
       nil t)
    (forward-char)
    it))

(defun my-goto-lisp-sexp-end (start-name)
  "Go to end of the backward Lisp sexp which start with START-NAME.
End of Lisp sexp is point before the last closed paren"
  (my-goto-lisp-sexp-begin start-name)
  (sp-get (sp-get-enclosing-sexp) (goto-char :end-in)))

(defun my-mark-lisp-sexp-inner (start-name)
  "Mark the inner of the Lisp sexp which start with function START-NAME."
  (my-goto-lisp-sexp-begin start-name)
  (forward-char -1)
  (sp-get (sp-get-sexp) (just-mark-region :beg-in :end-in)))

(defun my-in-lisp-sexp-p (start-name &optional pt)
  "Get t, When cursor at PT placed in Lisp sexp which start with START-NAME."
  (save-excursion
    (if pt (goto-char pt) (setq pt (point)))
    (and
     (my-goto-lisp-sexp-begin start-name)
     (sp-get (sp-get-enclosing-sexp) (< :beg pt :end)))))

(defvar my-url-prefixes
  '("https://" "http://" "ftp://" "file://")
  "List of the prefixes, which indicates that is URL.")

(defun my-uri-of-url (url)
  "Get the URI of URL."
  (or
   (-some->> url
     (s-chop-prefixes my-url-prefixes)
     (s-split "/")
     ;; ensure that has some URL parts, otherwise return nil
     (cdr)
     (-last-item)
     (s-split "?")
     (-first-item))
   ""))

(defun my-url-p (str)
  "Return non-nil, if STR is URL."
  (--some (s-prefix-p it str) my-url-prefixes))

(defun my-read-image-url ()
  "Read the URL of a image from the user.

If copied text is a URL, then return.  If region is active, then return a text
in the region.  Otherwise, read a URL from the minibuffer."
  (or
   (my-url-from-kill-ring)
   (just-text-in-region)
   (read-string "Enter URL for image, please: ")))

(defun my-read-url ()
  "Read the URL of from the user.

If copied text is a URL, then return.  If region is active, then return a text
in the region.  Otherwise, read a URL from the minibuffer."
  (or
   (my-url-from-kill-ring)
   (just-text-in-region)
   (read-string "URL, please: ")))

(defun my-url-from-kill-ring ()
  "If the last element of the kill ring is a URL, get it, otherwise get nil."
  (let ((copied (current-kill 0)))
    (and (my-url-p copied) copied)))

(defun my-read-string-or-nil ;nofmt
    (prompt &optional initial-input history default-value inherit-input-method)
  "Read string from the minibuffer, if the user type nothing, return nil.

Pass PROMPT, INITIAL-INPUT, HISTORY, DEFAULT-VALUE, INHERIT-INPUT-METHOD to
`read-string'"
  (let ((input
         (read-string prompt initial-input history default-value
                      inherit-input-method)))
    (unless (s-blank-p input) input)))

(defun my-buffer-file-name-base ()
  "Return a name of file, opened in the current buffer.

If buffer hasn't file then return nil."
  (and (buffer-file-name) (f-base (buffer-file-name))))

(defun my-incs (s)
  "If S is string that can be converted into number, then return incremented.

Otherwise nil"
  (and                                  ;nofmt
   (stringp s)
   (s-numeric-p s)
   (-> s (string-to-number) (1+) (number-to-string))))

(defun my-alist-union (alist1 alist2 &optional testfn)
  "Return union of ALIST1 and ALIST2, if has same keys, set to value of ALIST2.

Using TESTFN in functions sush as `assoc' or `alist-get'"
  (->>
   alist1
   (--remove (assoc (car it) alist2))
   (append alist2)))

(defun my-regexp-opt-of-regexp (regexps)
  "Return the regexp, which will be match to the one of taked REGEXPS."
  (concat "\\(?:" (s-join "\\|" regexps) "\\)"))

(defun my-inc-filename (path)
  "Increment filename of PATH and return updated.

For example a/b/1.exe should be a/b/2.exe"
  (let ((dirname (f-dirname path))
        (base (f-base path))
        (ext (f-ext path)))
    (->                                ;nofmt
     dirname
     (f-join (my-incs base))
     (f-swap-ext ext))))

(defmacro time-it (form &optional iters)
  "Return the average time to evaluate FORM ITERS time.

ITERATIONS defaults to 1"
  (or iters (setq iters 1))
  `(let ((started (current-time)))
     (--dotimes ,iters ,form)
     (/ (float-time (time-since started)) ,iters)))

(defmacro which-faster (iters &rest things)
  "Print name of the most fast things from given THINGS.

Also print average time to one iteration of each thing's call (do ITERS
calls for each thing)

Each thing is binding of name of thing (just a symbol without quote) and form
which should be evaluated"
  `(let ((times
          (list
           ,@(--map
              `(cons ',(car it) (time-it ,(cadr it) ,iters))
              things))))
     (--each
         (--sort (< (cdr it) (cdr other)) times)
       (message "Thing `%s' took `%s's" (car it) (cdr it)))
     (--sort (< (cdr it) (cdr other)) times)))

(defun my-current-year ()
  "Return the current year."
  (format-time-string "%Y"))



;;; my-lib.el ends here
;;; my-aas.el --- My configuration of the `auto-activating-snippets'

;; Copyright (C) 2022 Semen Khramtsov

;; Author: Semen Khramtsov <hrams205@gmail.com>
;; Version: 0.1
;; URL: https://github.com/semenInRussia/emacs.el

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; My configuration of the `auto-activating-snippets'

;;; Code:



(leaf aas :ensure t :global-minor-mode aas-global-mode)



;;; my-aas.el ends here
;;; my-aggressive-indent-mode.el --- My configuration of `aggressive-indent-mode' -*- lexical-binding: t; -*-

;; Copyright (C) 2023 semenInRussia

;; Author: semenInRussia <hrams205@gmail.com>
;; Version: 0.1
;; Homepage: https://github.com/semeninrussia/emacs.el

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

;; My configuration of `aggressive-indent-mode'.

;;; Code:




(leaf aggressive-indent-mode
  :ensure t
  :global-minor-mode aggressive-indent-global-mode
  :config
  (advice-add 'indent-region-line-by-line
              :around
              'my-remove-progresses)
  (advice-add 'lisp-indent-region
              :around
              'my-remove-progresses)

  (defun my-remove-progresses (fn &rest r)
    "Remove displaying of the progresses in FN, call it with R args."
    (cl-letf (((symbol-function 'make-progress-reporter) 'ignore)
              ((symbol-function 'progress-reporter-done) 'ignore)
              ((symbol-function 'progress-reporter-force-update) 'ignore)
              ((symbol-function 'dotimes-with-progress-reporter) 'ignore)
              ((symbol-function 'dolist-with-progress-reporter) 'ignore)
              ((symbol-function 'progress-reporter-do-update) 'ignore))
      (apply fn r))))



;;; my-aggressive-indent-mode.el ends here
;;; my-apheleia.el --- My configuration of the `apheleia': auto format of source code after save

;; Copyright (C) 2022 Semen Khramtsov

;; Author: Semen Khramtsov <hrams205@gmail.com>
;; Version: 0.1
;; URL: https://github.com/semenInRussia/emacs.el

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; My configuration of the `apheleia'

;;; Code:



(require 'f)

(defvar uncrustify-cfg-file (f-full "~/uncrustify.cfg"))

(leaf apheleia
  :ensure t
  :defvar (apheleia-formatters apheleia-mode-alist)
  :global-minor-mode apheleia-global-mode
  :defer-config
  (push '(uncrustify
          . ("uncrustify" "-f" filepath "-c" uncrustify-cfg-file "-o"))
        ;; a formatter for C++
        apheleia-formatters)
  (setf (alist-get 'rustfmt apheleia-formatters)
        '("rustfmt" "--edition" "2021" "--quiet" "--emit" "stdout"))
  (setf (alist-get 'c++-mode apheleia-mode-alist)
        'uncrustify)
  (setf (alist-get 'python-mode apheleia-mode-alist)
        '(yapf isort)))



;;; my-apheleia.el ends here
;;; my-buffer-navigation.el --- My config for navigation beetween buffers

;; Copyright (C) 2022 Semen Khramtsov

;; Author: Semen Khramtsov <hrams205@gmail.com>
;; Version: 0.1
;; URL: https://github.com/semenInRussia/emacs.el

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; My config for navigation beetween buffers

;;; Code:



(require 'dash)
(require 's)

(leaf ace-window
  :ensure t
  :bind ("M-o" . ace-window))

(defun my-visit-last-opened-buffer ()
  "Visit buffer which was opened recently."
  (interactive)
  (switch-to-buffer (my-last-opened-buffer)))

(defun my-last-opened-buffer ()
  "Get buffer which was visited most recently."
  (--find
   (not (my--visit-last-opened-buffer-ignore-p it))
   (cdr (buffer-list))))

(defun my--visit-last-opened-buffer-ignore-p (buffer)
  "Take object of BUFFER and return nil when don't need visit its."
  (->> buffer (buffer-name) (s-trim) (s-prefix-p "*Minibuf")))

(defun my-kill-current-buffer ()
  "Kill current opened buffer."
  (interactive)
  (kill-buffer (current-buffer)))

(leaf-keys
 ("C-TAB" . 'my-visit-last-opened-buffer)
 ("C-x k" . 'my-kill-current-buffer))



;;; my-buffer-navigation.el ends here
;;; my-comment-dwim-2.el --- My configuration for the `comment-dwim-2'

;; Copyright (C) 2022 Semen Khramtsov

;; Author: Semen Khramtsov <hrams205@gmail.com>
;; Version: 0.1
;; URL: https://github.com/semenInRussia/emacs.el

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; My configuration for the `comment-dwim-2'

;;; Code:



(leaf comment-dwim-2
  :ensure t
  :bind ("M-;" . comment-dwim-2))

(provide 'my-comment-dwim-2)
;;; my-comment-dwim-2.el ends here
;;; my-company.el --- My config for `company'

;; Copyright (C) 2022 Semen Khramtsov

;; Author: Semen Khramtsov <hrams205@gmail.com>
;; Version: 0.1
;; URL: https://github.com/semenInRussia/emacs.el

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; My config for `company'

;;; Code:




(leaf company
  :ensure t
  :defvar company-backends
  :global-minor-mode global-company-mode
  :config (add-to-list 'company-backends 'company-keywords)
  :custom ((company-idle-delay                . 0.3)
           (company-minimum-prefix-length     . 2)
           (company-show-numbers              . t)
           (company-tooltip-limit             . 15)
           (company-tooltip-align-annotations . t)
           (company-tooltip-flip-when-above   . t)
           (company-dabbrev-ignore-case       . nil)))



;;; my-company.el ends here
;;; my-drag.el --- My config for the things dragging

;; Copyright (C) 2022 Semen Khramtsov

;; Author: Semen Khramtsov <hrams205@gmail.com>
;; Version: 0.1
;; URL: https://github.com/semenInRussia/emacs.el

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; My config for the things dragging

;;; Code:



(require 'dash)
(require 'just)


(defcustom my-drag-map
  (make-sparse-keymap)
  "Keymap for my own small drag system."
  :type 'keymap
  :group 'my)

(define-prefix-command 'my-drag 'my-drag-map)

(defcustom my-left-draggers nil
  "Functions, which drag stuff to left, or return nil.

It is used in `my-drag-stuff-left'."
  :type '(repeat symbol)
  :group 'my)

(defvar my-last-command-is-drag-stuff nil
  "When is non-nil indicates, then last command was dragged any stuff.")

(defun my-drag-p ()
  "Return non-nil, when last command is drag-command."
  (memq last-command
        '(my-drag-stuff-right
          my-drag-stuff-left
          my-drag-stuff-up
          my-drag-stuff-down)))

(defun my-drag-stuff-left ()
  "My more general and functional version of `drag-stuff-left'."
  (interactive)
  (my-drag-anything my-left-draggers))

(defcustom my-right-draggers nil
  "Functions, which drag stuff to right, or return nil.

 It is used in `my-drag-stuff-right'."
  :type '(repeat symbol)
  :group 'my)

(defun my-drag-stuff-right ()
  "My more general and functional version of `drag-stuff-right'."
  (interactive)
  (my-drag-anything my-right-draggers))

(defcustom my-up-draggers nil
  "Functions, which drag stuff to up, or return nil.

Is used in `my-drag-stuff-up'."
  :type '(repeat symbol)
  :group 'my)

(defun my-drag-stuff-up ()
  "My more general and functional version of `drag-stuff-up'."
  (interactive)
  (my-drag-anything my-up-draggers))

(defcustom my-down-draggers nil
  "Functions, which drag stuff to up, or return nil.

It is used in `my-drag-stuff-down'."
  :type '(repeat symbol)
  :group 'my)

(defun my-drag-stuff-down ()
  "My more general and functional version of `drag-stuff-down'."
  (interactive)
  (my-drag-anything my-down-draggers))

(defun my-drag-anything (draggers)
  "Find and call first found true dragger from the DRAGGERS list.

After a call of the this command, current map will be setted to `my-drag-map'
for return the current map back, press either RET or a key which isn't bound in
`my-drag-map'.

True dragger mean that its function return non-nil when called interactively."
  (--find (call-interactively it) draggers)
  (message "Start dragging, use keys u, i, o, k. Type RET for exit...")
  (set-transient-map my-drag-map))

(defun add-left-dragger (f)
  "Add F to list draggers for `my-drag-stuff-left'."
  (add-to-list 'my-left-draggers f))

(defun add-right-dragger (f)
  "Add F to list draggers for `my-drag-stuff-right'."
  (add-to-list 'my-right-draggers f))

(defun add-up-dragger (f)
  "Add F to list draggers for `my-drag-stuff-up'."
  (add-to-list 'my-up-draggers f))

(defun add-down-dragger (f)
  "Add F to list draggers for `my-drag-stuff-down'."
  (add-to-list 'my-down-draggers f))

(defun stop-drag ()
  "Stop drag, just something print, and nothing do, set to nil something."
  (interactive)
  (setq my-last-command-is-drag-stuff nil)
  (message "Turn `drag' to normal!"))

(leaf-keys
 (my-drag-map
  ("RET" . stop-drag)
  ("i"   . my-drag-stuff-up)
  ("k"   . my-drag-stuff-down)
  ("o"   . my-drag-stuff-right)
  ("u"   . my-drag-stuff-left)))

(leaf drag-stuff
  :ensure t
  :global-minor-mode drag-stuff-global-mode
  :bind ("C-]" . 'my-drag)
  :bind (:my-drag-map
         ("j"       . my-drag-stuff-left-char)
         ("."       . transpose-sexps)
         ("m"       . transpose-sexps)
         ("n"       . avy-transpose-lines-in-region)
         ("l"       . my-drag-stuff-right-char)
         ("t"       . transpose-regions))
  :config

  (defun my-drag-stuff-left-char ()
    "Drag char to left."
    (interactive)
    (transpose-chars -1))

  (defun my-drag-stuff-right-char ()
    "Drag char to right."
    (interactive)
    (transpose-chars 1))

  (add-left-dragger  'drag-stuff-left)
  (add-right-dragger 'drag-stuff-right)
  (add-up-dragger    'drag-stuff-up)
  (add-down-dragger  'drag-stuff-down))



;;; my-drag.el ends here
;;; my-dumb-jump.el --- My configuration of the `dumb-jump'

;; Copyright (C) 2022 Semen Khramtsov

;; Author: Semen Khramtsov <hrams205@gmail.com>
;; Version: 0.1
;; URL: https://github.com/semenInRussia/emacs.el

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; My configuration of the `dumb-jump'

;;; Code:




(leaf rg
  :ensure t)

(leaf dumb-jump
  :ensure t
  :custom ((dumb-jump-prefer-searcher dumb-jump-force-searcher)
           . 'rg)
  :bind (("M-," . xref-go-back)
         ("M-." . xref-find-definitions))
  :hook (xref-backend-functions . dumb-jump-xref-activate))



;;; my-dumb-jump.el ends here
;;; my-editing.el --- My configuration for the custom editing

;; Copyright (C) 2022 Semen Khramtsov

;; Author: Semen Khramtsov <hrams205@gmail.com>
;; Version: 0.1
;; URL: https://github.com/semenInRussia/emacs.el

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; My configuration for the custom editing

;;; Code:

(require 's)


(defun open-line-saving-indent ()
  "Inserting new line, saving position and inserting new line."
  (interactive)
  (newline)
  (unless (s-blank-p (s-trim (thing-at-point 'line t)))
    (indent-according-to-mode))
  (forward-line -1)
  (end-of-line)
  (delete-horizontal-space t))

(defvar yank-indent-modes
  '(prog-mode sgml-mode js2-mode)
  "Modes in which to indent regions that are yanked (or yank-popped).")

(defvar yank-advised-indent-threshold 1000
  "Threshold (# chars) over which indentation does not automatically occur.")

(defun yank-advised-indent-function (beg end)
  "Do indentation, as long as the region beetween BEG END isn't too large."
  (when (<= (- end beg) yank-advised-indent-threshold)
    (indent-region beg end nil)))

(defadvice yank (after yank-indent activate)
  "If current mode is one of 'yank-indent-modes, indent yanked text.
With prefix arg don't indent."
  (if (and
       (not (ad-get-arg 0))
       (--any? (derived-mode-p it) yank-indent-modes))
      (let ((transient-mark-mode nil))
        (yank-advised-indent-function
         (region-beginning)
         (region-end)))))

(defadvice yank-pop (after yank-pop-indent activate)
  "If current mode is one of 'yank-indent-modes, indent yanked text.
With prefix arg don't indent."
  (if (and
       (not (ad-get-arg 0))
       (member major-mode yank-indent-modes))
      (let ((transient-mark-mode nil))
        (yank-advised-indent-function
         (region-beginning)
         (region-end)))))

(defun yank-unindented ()
  "Just `yunk'."
  (interactive)
  (yank 1))

(global-set-key (kbd "C-a") 'beginning-of-line-text)
(global-set-key (kbd "C-o") 'open-line-saving-indent)
(global-set-key (kbd "M-y") 'consult-yank-from-kill-ring)



;;; my-editing.el ends here
;;; my-eglot.el --- My configuration for lsp -*- lexical-binding: t; -*-

;; Copyright (C) 2022 semenInRussia

;; Author: semenInRussia <hrams205@gmail.com>
;; Version: 0.1
;; Homepage: https://github.com/semeninrussia/emacs.el

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

;; My configuration of lsp.  I am use `eglot'

;;; Code:




(require 'fast-exec)

(require 'dash)

(declare-function turn-off-flycheck "my-flycheck.el")


(defcustom my-eglot-major-modes
  '(rust-mode python-mode haskell-mode)
  "List of the major modes in which should work `eglot'."
  :group 'my
  :type '(repeat major-mode))

;; `eglot' use `flymake' instead of `flycheck', so i disable `flycheck'
(add-hook 'eglot-connect-hook #'turn-off-flycheck)


(leaf eglot
  :custom `((eglot-send-changes-idle-time . 1) ; in seconds
            )
  :custom-face (eglot-highlight-symbol-face . '((t (:inherit lazy-highlight))))
  :bind (("C-c lr" . 'eglot-rename)
         ("<f6>"   . 'eglot-rename)
         ("C-c la"  . 'eglot-code-actions)
         (:eglot-mode-map
          ([remap my-format-expression] . 'eglot-format)))
  :fast-exec (("Start a LSP Server for Current Buffer" 'eglot)
              ("Reconnect the LSP Server" 'eglot-reconnect)
              ("Disable the LSP Server" 'eglot-shutdown))
  :config                               ;nofmt
  (leaf flymake
    :require t
    :bind (:flymake-mode-map
           ("C-c fd" . 'flymake-show-project-diagnostics)
           ([remap next-error] . 'flymake-goto-next-error)
           ([remap prev-error] . 'flymake-goto-prev-error)))

  (leaf eldoc-box
    :after my-eldoc
    :hook (eglot-managed-mode-hook . eldoc-box-hover-mode)))



;;; my-eglot.el ends here
;;; my-eldoc.el --- My configuration of the `eldoc'

;; Copyright (C) 2022 Semen Khramtsov

;; Author: Semen Khramtsov <hrams205@gmail.com>
;; Version: 0.1
;; URL: https://github.com/semenInRussia/emacs.el

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; My configuration of the `eldoc'

;;; Code:





(leaf eldoc                             ;elfmt
  :ensure t
  :custom (eldoc-idle-delay . 0.2)
  :config (leaf eldoc-box
            :ensure t
            :defun eldoc-box-hover-mode
            :defvar global-eldoc-box-hover-mode
            :bind ("C-x C-x" . 'eldoc-box-quit-frame)
            :init (define-global-minor-mode global-eldoc-box-hover-mode
                    eldoc-box-hover-mode
                    (lambda () (eldoc-box-hover-mode 1)))
            :global-minor-mode global-eldoc-box-hover-mode))



;;; my-eldoc.el ends here
;;; my-embrace.el --- My configuration of the `embrace'

;; Copyright (C) 2022 Semen Khramtsov

;; Author: Semen Khramtsov <hrams205@gmail.com>
;; Version: 0.1
;; URL: https://github.com/semenInRussia/emacs.el

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; My configuration of the `embrace'

;;; Code:



(leaf embrace
  :ensure t
  :defvar embrace-semantic-units-alist
  :setq-default (embrace-show-help-p . nil)
  :bind ("C-."       . embrace-commander)
  :hook (emacs-lisp-mode-hook . embrace-emacs-lisp-mode-hook)
  :config                             ;nofmt
  (unless (assq ?n embrace-semantic-units-alist)
    (setq-default embrace-semantic-units-alist
                  (cons
                   '(?n . embrace-avy-semantic-unit)
                   embrace-semantic-units-alist)))

  (defun embrace-avy-semantic-unit ()
    "Semantic unit for `embrace' which ask expression with the `avy'."
    (call-interactively 'avy-mark-word)))



;;; my-embrace.el ends here
;;; my-expand-region.el --- My configuration of the `expand-region'

;; Copyright (C) 2022 Semen Khramtsov

;; Author: Semen Khramtsov <hrams205@gmail.com>
;; Version: 0.1
;; URL: https://github.com/semenInRussia/emacs.el

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; My configuration of the `expand-region'

;;; Code:



(require 'dash)
(require 'just)


(leaf expand-region
  :ensure t
  :bind ("C-x C-<SPC>" . er/expand-region))



;;; my-expand-region.el ends here
;;; my-fast-exec.el --- My configuration of the `fast-exec'

;; Copyright (C) 2022 Semen Khramtsov

;; Author: Semen Khramtsov <hrams205@gmail.com>
;; Version: 0.1
;; URL: https://github.com/semenInRussia/emacs.el

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; My configuration of the `fast-exec'

;;; Code:




(require 'dash)

(declare-function visual-fill "my-lang-utils")

(leaf fast-exec
  :load-path "~/projects/fast-exec.el/"
  :defun fast-exec-use
  :require t
  :bind ("M-=" . fast-exec-exec)
  :commands fast-exec-exec
  :config (add-hook 'fast-exec-hint-buffer-mode-hook #'visual-fill)
  (require 'my-fast-exec-misc))



;;; my-fast-exec.el ends here
;;; my-flycheck.el --- My configuration of the `flycheck'

;; Copyright (C) 2022, 2023 Semen Khramtsov

;; Author: Semen Khramtsov <hrams205@gmail.com>
;; Version: 0.1
;; URL: https://github.com/semenInRussia/emacs.el

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; My configuration of the `flycheck'

;;; Code:



(leaf flycheck                          ;nofmt
  :ensure t
  :bind (:flycheck-mode-map
         ([remap next-error] . 'flycheck-next-error)
         ([remap previous-error] . 'flycheck-previous-error))
  :defun flycheck-mode
  :global-minor-mode global-flycheck-mode
  :config                             ;nofmt
  (defun turn-off-flycheck (&rest _)
    "Disable `flycheck-mode' locally for current buffer."
    (interactive)
    (flycheck-mode 0)))



;;; my-flycheck.el ends here
;;; my-goto-last-change.el --- My configuration of `goto-last-change' -*- lexical-binding: t; -*-

;; Copyright (C) 2022 semenInRussia

;; Author: semenInRussia <hrams205@gmail.com>
;; Version: 0.1
;; Homepage: https://github.com/semeninrussia/emacs.el

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

;; My configuration of `goto-last-change'.

;;; Code:




(leaf goto-last-change
  :ensure t
  :bind ("C-_" . 'goto-last-change))



;;; my-goto-last-change.el ends here
;;; my-imenu.el --- My configuration of the `imenu'

;; Copyright (C) 2022 Semen Khramtsov

;; Author: Semen Khramtsov <hrams205@gmail.com>
;; Version: 0.1
;; URL: https://github.com/semenInRussia/emacs.el

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; My configuration of the `imenu'

;;; Code:




(leaf consult-imenu
  :custom (imenu-auto-rescan . t))



;;; my-imenu.el ends here
;;; my-indent.el --- My configuration for the indentation

;; Copyright (C) 2022 Semen Khramtsov

;; Author: Semen Khramtsov <hrams205@gmail.com>
;; Version: 0.1
;; URL: https://github.com/semenInRussia/emacs.el

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; My configuration for the indentation

;;; Code:





;; disable tabs
(setq-default indent-tabs-mode nil)

(defun my-indent-line-or-region ()
  "If text selected, then indent it, otherwise indent current line."
  (interactive)
  (save-excursion
    (if (use-region-p)
        (indent-region (region-beginning) (region-end))
      (funcall indent-line-function))))

(leaf-keys
 (prog-mode-map ("RET" . newline-and-indent)))

(leaf-keys
 ("RET"   . newline-and-indent)
 ("q"     . my-indent-line-or-region)
 ("SPC q" . join-line))



;;; my-indent.el ends here
;;; my-lsp-bridge.el --- My configuration of `lsp-bridge': the fastest LSP client -*- lexical-binding: t; -*-

;; Copyright (C) 2022 semenInRussia

;; Author: semenInRussia <hrams205@gmail.com>
;; Version: 0.1
;; Homepage: https://github.com/semeninrussia/emacs.el

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

;; My configuration of `lsp-bridge'.  The fastest LSP client

;;; Code:




(require 'dash)
(require 'company)

(require 'fast-exec)


(leaf posframe :ensure t)

(leaf lsp-bridge
  :load-path* "lisp/site-lisp/lsp-bridge"
  :hook ((lsp-bridge-user-multiserver-dir . "~/lsp/multi")
         (lsp-bridge-user-langserver-dir . "~/lsp/single/")
         (lsp-bridge-mode-hook . (lambda () (company-mode 0))))
  :custom (;; features
           (lsp-bridge-enable-hover-diagnostic . t)
           (acm-enable-tabnine . nil)
           (acm-enable-quick-access . t)
           (lsp-bridge-python-command . "python.exe")
           ;; choose LSP servers
           (lsp-bridge-tex-lsp-server . 'texlab)
           (lsp-bridge-multi-lang-server-extension-list . nil)
           ;; misc
           (lsp-bridge-diagnostic-display-errors-delay . 0.9)
           ;; use `consult' instead of popup for autofixes
           (lsp-bridge-code-action-enable-popup-menu . nil)
           (lsp-bridge-code-action-preview-delay . 20)
           (lsp-bridge-signature-show-function
            . 'eldoc-box--eldoc-message-function))
  :bind (:lsp-bridge-mode-map
         ("C-c C-d" . 'lsp-bridge-popup-documentation)
         ("<f6>"    . 'lsp-bridge-rename)
         ("C-c ll"  . 'lsp-bridge-code-action)
         ("M-,"     . 'lsp-bridge-find-references)
         ([remap xref-pop-marker-stack] . 'lsp-bridge-pop)
         ([remap xref-find-definitions] . 'lsp-bridge-find-def)
         ([remap consult-imenu-multi] . 'lsp-bridge-workspace-list-symbols)
         ([remap my-format-expression] . 'lsp-bridge-code-format)
         ([remap next-error] . 'lsp-bridge-diagnostic-jump-next)
         ([remap previous-error] . 'lsp-bridge-diagnostic-jump-prev))
  (:lsp-bridge-call-hierarchy-mode-map
   ("ESC" . nil)
   ("M-n" . 'lsp-bridge-call-hierarchy-next)
   ("M-p" . 'lsp-bridge-call-hierarchy-prev))
  :fast-exec (("Start a LSP Server for Current Buffer" 'lsp-bridge-mode)
              ("Reconnect the LSP Server" 'lsp-bridge-restart-process))
  :config (add-hook 'lsp-bridge-mode-hook #'turn-off-flycheck))



;;; my-lsp-bridge.el ends here
;;; my-lsp.el --- My choose between LSP clieents -*- lexical-binding: t; -*-

;; Copyright (C) 2023 semenInRussia

;; Author: semenInRussia <hrams205@gmail.com>
;; Version: 0.0.1
;; Package-Requires: ((dash "2.18.0") (s "1.12.0"))

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

;; My choose between LSP clieents.

;;; Code:

(declare-function eglot-ensure "eglot.el")
(declare-function lsp-bridge-mode "lsp-bridge.el")

(defun my-lsp-ensure ()
  "Run choosen lsp client for the current buffer."
  (interactive)
  (eglot-ensure))



;;; my-lsp.el ends here
;;; my-meow.el --- My configuration of `meow' -*- lexical-binding: t; -*-

;; Copyright (C) 2023 semenInRussia

;; Author: semenInRussia <hrams205@gmail.com>
;; Version: 0.1
;; Homepage: https://github.com/semeninrussia/emacs.el

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

;; My configuration of `meow'.

;;; Code:





(leaf meow
  :ensure t
  :defvar (meow-cheatsheet-layout meow-cheatsheet-layout-qwerty)
  :defun (my-meow-setup . my-meow)
  :defun (meow-global-mode
          meow-motion-overwrite-define-key
          meow-motion-overwrite-define-key
          meow-leader-define-key
          meow-normal-define-key)
  :require t
  :custom (meow-use-clipboard . t)
  :bind ("M-." . xref-find-definitions)
  :config
  (defun my-meow-setup ()
    (setq meow-cheatsheet-layout meow-cheatsheet-layout-qwerty)
    (meow-motion-overwrite-define-key
     '("j" . meow-next)
     '("k" . meow-prev)
     '("<escape>" . ignore))
    (meow-leader-define-key
     ;; SPC j/k will run the original command in MOTION state.
     '("j" . "H-j")
     '("k" . "H-k")
     ;; Use SPC (0-9) for digit arguments.
     '("1" . meow-digit-argument)
     '("2" . meow-digit-argument)
     '("3" . meow-digit-argument)
     '("4" . meow-digit-argument)
     '("5" . meow-digit-argument)
     '("6" . meow-digit-argument)
     '("7" . meow-digit-argument)
     '("8" . meow-digit-argument)
     '("9" . meow-digit-argument)
     '("0" . meow-digit-argument)
     '("/" . meow-keypad-describe-key)
     '("?" . meow-cheatsheet))
    (meow-normal-define-key
     '("0" . meow-expand-0)
     '("9" . meow-expand-9)
     '("8" . meow-expand-8)
     '("7" . meow-expand-7)
     '("6" . meow-expand-6)
     '("5" . meow-expand-5)
     '("4" . meow-expand-4)
     '("3" . meow-expand-3)
     '("2" . meow-expand-2)
     '("1" . meow-expand-1)
     '("-" . negative-argument)
     '(";" . meow-reverse)
     '("," . meow-inner-of-thing)
     '("." . meow-bounds-of-thing)
     '("[" . meow-beginning-of-thing)
     '("]" . meow-end-of-thing)
     '("a" . meow-append)
     '("A" . meow-open-below)
     '("b" . meow-back-word)
     '("B" . meow-back-symbol)
     '("c" . meow-change)
     '("d" . meow-delete)
     '("D" . meow-backward-delete)
     '("e" . meow-next-word)
     '("E" . meow-next-symbol)
     '("f" . meow-find)
     '("g" . meow-cancel-selection)
     '("G" . meow-grab)
     '("h" . meow-left)
     '("H" . meow-left-expand)
     '("i" . meow-insert)
     '("I" . meow-open-above)
     '("j" . meow-next)
     '("J" . meow-next-expand)
     '("k" . meow-prev)
     '("K" . meow-prev-expand)
     '("l" . meow-right)
     '("L" . meow-right-expand)
     '("m" . meow-join)
     '("n" . meow-search)
     '("o" . meow-block)
     '("O" . meow-to-block)
     '("p" . meow-yank)
     '("P" . meow-yank-pop)
     '("q" . meow-quit)
     '("Q" . meow-goto-line)
     '("r" . meow-replace)
     '("R" . meow-swap-grab)
     '("s" . meow-kill)
     '("t" . meow-till)
     '("u" . meow-undo)
     '("U" . meow-undo-in-selection)
     '("v" . meow-visit)
     '("w" . meow-mark-word)
     '("W" . meow-mark-symbol)
     '("x" . meow-line)
     '("X" . meow-goto-line)
     '("y" . meow-save)
     '("Y" . meow-sync-grab)
     '("z" . meow-pop-selection)
     '("Z" . meow-comment)
     '("'" . repeat)
     '(">" . meow-find-ref)
     '("<escape>" . ignore)))
  (my-meow-setup)
  (meow-global-mode t))



;;; my-meow.el ends here
;;; my-multiple-cursors.el --- My configuration for the `multiple-cursors'

;; Copyright (C) 2022 Semen Khramtsov

;; Author: Semen Khramtsov <hrams205@gmail.com>
;; Version: 0.1
;; URL: https://github.com/semenInRussia/emacs.el

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; My configuration for the `multiple-cursors'

;;; Code:



(declare-function consult-buffer "consult")

(leaf multiple-cursors
  :ensure t
  :defun (mc/vertical-align-with-space
          mc/edit-lines
          mc/mark-all-words-like-this)
  :defvar multiple-cursors-mode
  :custom (my-mc-cmds-to-run-once .
                                  '(my-mark-all
                                    my-bob-or-mc-align
                                    my-eob-or-align-with-spaces
                                    my-mc-mark-like-this-or-edit-lines
                                    my-mc-mark-like-this-or-edit-lines
                                    toggle-input-method))
  :bind (("M-i"       . 'mc/edit-lines)
         ("C-,"       . 'mc/mark-next-like-this-word)
         ("C-<"       . mc/mark-previous-like-this-word)
         ;; ("SPC TAB 7" . mc/reverse-regions)
         ;; ("SPC d 7"   . mc/unmark-next-like-this)
         ("M-<"     . my-bob-or-mc-align)
         ("M->"     . my-eob-or-mc-align-with-space)
         ("C-x C-," . my-mark-all))
  :config

  (defun my-mark-all ()
    "Mark all words like this for `multiple-cursors', otherwise mark buffer."
    (interactive)
    (if multiple-cursors-mode
        (mc/mark-all-words-like-this)
      (call-interactively 'mark-whole-buffer)))

  (defun my-bob-or-mc-align ()
    "If enabled `multiple-cursors', then mark then align by regexp, other bob.

BOB - is `beginning-of-buffer'"
    (interactive)
    (if multiple-cursors-mode
        (call-interactively 'mc/vertical-align)
      (call-interactively 'beginning-of-buffer)))

  (defun my-eob-or-mc-align-with-space ()
    "If enabled `multiple-cursors', then align by spaces, other eob.

EOB - is `end-of-buffer'"
    (interactive)
    (if multiple-cursors-mode
        (mc/vertical-align-with-space)
      (goto-char (point-max)))))



;;; my-multiple-cursors.el ends here
;;; my-save-place-mode.el --- My configuration of `save-place-mode' -*- lexical-binding: t; -*-

;; Copyright (C) 2022 semenInRussia

;; Author: semenInRussia <hrams205@gmail.com>
;; Version: 0.1
;; Homepage: https://github.com/semeninrussia/emacs.el

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

;; My configuration of `save-place-mode'.

;;; Code:




(leaf save-place-mode :global-minor-mode save-place-mode)



;;; my-save-place-mode.el ends here
;;; my-search.el --- My configuration of the search

;; Copyright (C) 2022 Semen Khramtsov

;; Author: Semen Khramtsov <hrams205@gmail.com>
;; Version: 0.1
;; URL: https://github.com/semenInRussia/emacs.el

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; My configuration of the search

;;; Code:



(leaf ctrlf
  :ensure t
  :bind ("C-s" . ctrlf-forward-default))

(leaf visual-regexp
  :ensure t
  :bind ("M-%" . vr/query-replace))

(leaf deadgrep
  :ensure t
  :bind ("C-c S" . deadgrep))



;;; my-search.el ends here
;;; my-smartparens.el --- My configuration for the `smartparens'

;; Copyright (C) 2022 Semen Khramtsov

;; Author: Semen Khramtsov <hrams205@gmail.com>
;; Version: 0.1
;; URL: https://github.com/semenInRussia/emacs.el

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; My configuration for the `smartparens'

;;; Code:






(leaf smartparens
  :ensure t
  :global-minor-mode smartparens-global-mode
  :require smartparens-config
  :defun (sp-clone-sexp sp-use-paredit-bindings)
  :bind (:smartparens-mode-map
         ("C-c C-w" . 'sp-splice-sexp-killing-backward)
         ("C-x C-y" . 'my-sp-clone)
         ("C-c DEL" . 'sp-change-enclosing)
         ("C-M-."   . 'sp-forward-slurp-sexp)
         ("C-M-,"   . 'sp-forward-barf-sexp)
         ("C-M->"   . 'sp-backward-slurp-sexp)
         ("C-M-<"   . 'sp-backward-barf-sexp))
  :config
  (sp-use-paredit-bindings)
  (defun my-sp-clone ()
    (interactive)
    (sp-clone-sexp)
    (repeat-at-last-keystroke))

  (defun delete-only-1-char ()
    "Delete only 1 character before point."
    (interactive)
    (backward-char)
    (delete-char 1)))



;;; my-smartparens.el ends here
;;; my-whitespace-cleanup-mode.el --- My configuration of `whitespaces-cleanup-mode' -*- lexical-binding: t; -*-

;; Copyright (C) 2022 semenInRussia

;; Author: semenInRussia <hrams205@gmail.com>
;; Version: 0.1
;; Homepage: https://github.com/semeninrussia/emacs.el

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

;; My configuration of `whitespace-cleanup-mode'.

;;; Code:




(leaf whitespace-cleanup-mode
  :ensure t
  :require t
  :global-minor-mode global-whitespace-cleanup-mode)



;;; my-whitespace-cleanup-mode.el ends here
;;; my-yas.el --- My configuration for the `yasnippet'

;; Copyright (C) 2022 Semen Khramtsov

;; Author: Semen Khramtsov <hrams205@gmail.com>
;; Version: 0.1
;; URL: https://github.com/semenInRussia/emacs.el

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; My configuration for the `yasnippet'

;;; Code:



(require 's)

(leaf yasnippet
  :ensure t
  :defun (yas--table-hash
          yas--filter-templates-by-condition
          yas--namehash-templates-alist)
  :global-minor-mode yas-global-mode
  :custom `((yas-snippet-dirs .
                              ',(list
                                 (locate-user-emacs-file "snippets")))
            (yas-wrap-around-region . t)))

(leaf yasnippet
  :doc "Load `fast-exec' keymaps for `yasnippet'."
  :after fast-exec
  :config                               ;nofmt
  (defun yas--fetch (table key)
    "Fetch templates in TABLE by KEY.

Return a list of cons (NAME . TEMPLATE) where NAME is a
string and TEMPLATE is a `yas--template' structure."
    (let* ((key (s-downcase key))
	   (keyhash (yas--table-hash table))
	   (namehash (and keyhash (gethash key keyhash))))
      (when namehash
	(yas--filter-templates-by-condition
	 (yas--namehash-templates-alist namehash))))))



;;; my-yas.el ends here
;;; my-lisp.el --- my-lisp

;; Copyright (C) 2022 Semen Khramtsov

;; Author: Semen Khramtsov <hrams205@gmail.com>
;; Version: 0.1
;; URL: https://github.com/semenInRussia/emacs.el

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:




(declare-function meow-insert "meow-commands.el")

(require 'dash)



(defun my-lisp-sexp-whole-line-p ()
  "Return t, when the Lisp sexp at the point being at whole of line."
  (interactive "P")
  (-let
      (((beg . end)
        (paxedit-sexp-region)))
    (and
     (= beg (save-excursion (beginning-of-line-text) (point)))
     (= end (pos-eol)))))

(defun my-paxedit-transpose-forward ()
  (interactive)
  (call-interactively #'paxedit-transpose-forward)
  (repeat-at-last-keystroke))

(defun my-paxedit-transpose-backward ()
  (interactive)
  (call-interactively #'paxedit-transpose-backward)
  (repeat-at-last-keystroke))

(leaf paxedit
  :ensure t
  :defun (paxedit-delete
          paxedit-sexp-region
          paxedit-transpose-backward
          paxedit-transpose-forward)
  :bind ((:paxedit-mode-map
          ("C-c C-t" . 'my-paxedit-transpose-forward)
          ("C-c C-u C-t" . 'my-paxedit-transpose-backward)
          ("C-c C-w" . 'paxedit-kill)
          ("C-c C-;" . 'my-paxedit-comment)
          ("C-c C-d" . 'paxedit-symbol-kill)
          ("C-c C-q" . 'paxedit-compress)
          ("C-c C-k" . 'paxedit-delete-whitespace)
          ("C-c C-y" . 'my-paxedit-duplicate)))
  :hook ((emacs-lisp-mode-hook . paxedit-mode)
         (racket-mode-hook . paxedit-mode))
  :config                               ;nofmt
  (defun my-paxedit-comment ()
    "Comment the Lisp expression at the cursor."
    (interactive)
    (-let
        (((beg . end)
          (paxedit-sexp-region)))
      (comment-region beg end)))

  (defun my-paxedit-change ()
    "Kill the Lisp expression at the cursor and activate insert mode."
    (interactive)
    (paxedit-delete)
    (meow-insert))

  (defun my-paxedit-duplicate ()
    "Make copy of the Lisp expression at the cursor."
    (interactive)
    (let* ((reg (paxedit-sexp-region))
           (beg (car reg))
           (end (cdr reg))
           (sexp (buffer-substring beg end))
           (sep (if (my-lisp-sexp-whole-line-p) "\n" " ")))
      (goto-char end)
      (insert sep sexp))))

(leaf lisp-mode :custom (lisp-body-indent . 2))



;;; my-lisp.el ends here
;;; my-autoformat.el --- My function for `autoformat'

;; Copyright (C) 2022-2023 Semen Khramtsov

;; Author: Semen Khramtsov <hrams205@gmail.com>
;; Version: 0.1
;; URL: https://github.com/semenInRussia/emacs.el

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; My function for `autoformat'

;;; Code:



(require 'just)
(require 'dash)

(defgroup my-autoformat nil
  "Automatically format of source code (add spaces, capitalze and etc)."
  :group 'editing)

(defcustom my-autoformat-sentence-end "[.?!]  "
  "Regexp that indicates the end of a sentence for `autoformat'."
  :group 'my-autoformat
  :type 'regexp)

(defvar my-autoformat-local-functions nil
  "Autoformat functions that will be called to format a code.

This variable is local to each buffer, so every buffer can has own special
formatting functions.")

(make-local-variable 'my-autoformat-local-functions)

(defcustom my-autoformat-global-functions
  nil
  "Autoformat functions that working everywhere.

This be like on `my-autoformat-local-functions', but works globally"
  :group 'my-autoformat
  :type '(repeat function))

(defun my-autoformat-sentence-capitalization (&optional prev-line-can-be-text)
  "Auto-capitalize the first letter of a sentence.

Either at the beginning of a line, or after a sentence end, if
PREV-LINE-CAN-BE-TEXT is nil (by default), then capitalize only when
previous line is empty."
  (interactive)
  (just-call-on-backward-char*
   (and
    (looking-at-p "[[:alpha:]]")
    (or prev-line-can-be-text
        (just-call-on-prev-line 'just-line-is-whitespaces-p)
        (equal (line-beginning-position) (point-min)))
    (or
     (just-beginning-of-line-text-p)
     (bobp)
     (looking-back my-autoformat-sentence-end nil))
    (upcase-char 1))))

(defun my-previous-line-is-empty ()
  "Return non-nil value, if the previous line is empty.

See `just-line-is-whitespaces-p' to understand what \"empty\" is mean"
  (just-call-on-prev-line 'just-line-is-whitespaces-p))

(defun my-autoformat-do ()
  "Funcall each of autoformat functions.

See variable `my-autoformat-local-functions' to know about autoformat
functions working locally in the buffer and `my-autoformat-global-functions'
to know about autoformat functions working everywhere"
  (interactive)
  (-each my-autoformat-local-functions 'funcall)
  (-each my-autoformat-global-functions 'funcall))

(define-minor-mode my-autoformat-mode
  "Toggle `my-autoformat-mode'.

If the ARG is non-nil, then enable the mode, otherwise disable it.

In this mode each keyboard press calls functions to format a code.  These
functions can be defined either locally (see `my-autoformat-local-functions')
or everywhere (see `my-autoformat-global-functions')."
  :init-value nil
  (if my-autoformat-mode
      (progn
        (my-autoformat-activate-for-major-mode)
        (add-hook 'post-self-insert-hook #'my-autoformat-do nil t))
    (my-autoformat-activate-for-major-mode)
    (remove-hook 'post-self-insert-hook #'my-autoformat-do t)))

(define-globalized-minor-mode
  my-autoformat-global-mode
  my-autoformat-mode
  my-autoformat-turn-on-mode)

(defun my-autoformat-turn-on-mode ()
  "Enable `my-autoformat-mode' locally."
  (my-autoformat-mode t))

(defvar my-autoformat-functions-of-major-modes nil
  "Alist from keys `major-mode' s and values their autoformat functions.")

(defun my-autoformat-activate-for-major-mode (&optional mm)
  "Change the autoformat local functions depends on major mode (MM).

MM defaults to value of the `major-mode'"
  (interactive)
  (or mm (setq mm major-mode))
  (setq-local my-autoformat-local-functions
              (alist-get mm
                         my-autoformat-functions-of-major-modes
                         '(ignore)
                         nil
                         'eq)))

(defun my-autoformat-bind-for-major-mode (mode &rest functions)
  "Bind autoformat FUNCTIONS as local to major-mode MODE."
  (->>
   my-autoformat-functions-of-major-modes
   (assq-delete-all mode)
   (cons (cons mode functions))
   (setq my-autoformat-functions-of-major-modes)))

(my-autoformat-global-mode t)



;;; my-autoformat.el ends here
;;; my-elisp.el --- My configuration of the elisp

;; Copyright (C) 2022 Semen Khramtsov

;; Author: Semen Khramtsov <hrams205@gmail.com>
;; Version: 0.1
;; URL: https://github.com/semenInRussia/emacs.el

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; My configuration of the `emacs-lisp-mode'

;;; Code:






(require 's)
(require 'dash)

(leaf elisp-mode
  :config (add-hook 'emacs-lisp-mode 'paxedit-mode)
  (leaf inspector
    :ensure t
    :bind (:emacs-lisp-mode-map
           :package elisp-mode
           ("C-c C-i" . inspector-inspect-last-sexp)))

  (leaf paredit
    :ensure t
    :hook emacs-lisp-mode-hook))

(leaf suggest :ensure t)

(leaf mocker :ensure t :doc "A library for testing `elisp' with mocks")

(leaf my-elisp-embrace
  :hook (emacs-lisp-mode-hook . my-embrace-emacs-lisp-mode-hook))



;;; my-elisp.el ends here
;;; my-racket.el --- My Configuration For The Lanugage `racket'

;; Copyright (C) 2022 Semen Khramtsov

;; Author: Semen Khramtsov <hrams205@gmail.com>
;; Version: 0.1
;; URL: https://github.com/semenInRussia/emacs.el

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; My Configuration For The Lanugage `racket'

;;; Code:

(leaf racket-mode
  :ensure t
  :major-mode-map (racket               ;nofmt
                   :modes (racket-mode racket-repl-mode)
                   :parent my-lisp-map)
  :defvar (my-racket-meta-return-functions
           my-racket-meta-return-cond-clauses-expression-names)
  :defun ((my-racket-meta-return-contracted .
                                            (my-racket
                                             my-racket-meta-return-test-case
                                             my-racket-meta-return-let)))
  :bind ((:racket-mode-map               ;nofmt
          ("M-RET" . 'my-racket-meta-return))
         (:my-racket-local-map
          ("i"     . 'racket-add-require-for-identifier)))
  :hook ((racket-mode-hook . racket-xp-mode)
         ;; `flycheck' is very slow plus `racket-xp-mode' highlight
         ;; errors too, so i disable `flycheck' for Racket
         (racket-mode-hook . turn-off-flycheck)
         ;; enable structured editing for the `racket-mode'
         (racket-mode-hook . paxedit-mode))
  :custom (racket-xp-mode-hook . nil)
  :config                               ;nofmt
  (remove-hook 'racket-mode-hook 'racket-mode) ;fix a bug

  (defcustom my-racket-meta-return-functions nil
    "List of functions for M-ret in racket.

Each function should return t, if it should be called and should stop next
calls of functions."
    :type '(repeat function)
    :group 'my)

  (defun my-racket-meta-return ()
    "Try use one of M-ret functions for racket.
Depends on `my-racket-meta-return-functions'."
    (interactive)
    (unless (-find #'funcall my-racket-meta-return-functions)
      (message "Sorry, function not found!")))

  (defun my-racket-meta-return-let ()
    "Add a binding to the let expression of the Racket.
One of `my-racket-meta-return-functions'"
    (when (my-in-lisp-sexp-p "let")
      (my-goto-lisp-sexp-begin "let")
      (search-forward "(." nil t)
      (sp-get
          (sp-get-sexp)
        (goto-char :end-in)
        (newline-and-indent)
        (insert "[]")
        (forward-char -1)
        t)))

  (add-to-list 'my-racket-meta-return-functions #'my-racket-meta-return-let)

  (defun my-racket-meta-return-test-case ()
    "Add a test case to current test module in racket.
One of `my-racket-meta-return-functions'"
    (when (my-in-lisp-sexp-p "module\+\\W*test")
      (my-goto-lisp-sexp-begin "module\+\\W*test")
      (forward-char -1)
      (sp-get (sp-get-sexp) (goto-char :end-in))
      (newline-and-indent)
      (insert "(check-equal? )")
      (forward-char -1)
      t))

  (add-to-list 'my-racket-meta-return-functions
               #'my-racket-meta-return-test-case)

  (defcustom my-racket-meta-return-cond-clauses-expression-names
    '("cond" "match" "define/match")
    "List of the racket expressions names in which should work `M-ret'."
    :type '(repeat string)
    :group 'my)

  (defun my-racket-meta-return-cond-clauses ()
    "Add new clause to racket expression which has syntax like on `cond'.

One of `my-racket-meta-return-functions'.

List of racket expressions in which this function should work:

- `cond'
- `match'
- `define/match'"
    (interactive)
    (--when-let
        (-find
         #'my-in-lisp-sexp-p
         my-racket-meta-return-cond-clauses-expression-names)
      (my-goto-lisp-sexp-begin it)
      (forward-char -1)
      (forward-sexp)
      (forward-char -1)
      (newline-and-indent)
      (insert "[]")
      (forward-char -1)
      t))

  (add-to-list
   'my-racket-meta-return-functions
   'my-racket-meta-return-cond-clauses)

  (defun my-racket-meta-return-contracted ()
    "Add new argument form to the expression of the Racket `contracted'."
    (interactive)
    (when (my-in-lisp-sexp-p "contracted")
      (my-goto-lisp-sexp-end "contracted")
      (newline)
      (insert "[]")
      (my-mark-lisp-sexp-inner "contracted")
      (align-regexp
       (region-beginning)
       (region-end)
       "\\[[^ ]+ *\\( \\)[^ ]")
      (beginning-of-line-text)
      (forward-char 1)
      t))

  (add-to-list 'my-racket-meta-return-functions
               #'my-racket-meta-return-contracted))

(leaf scribble-mode
  :ensure t
  :config                               ;nofmt
  (my-autoformat-bind-for-major-mode 'scribble-mode
                                     'my-autoformat-sentence-capitalization))



;;; my-racket.el ends here
;;; my-bib.el --- My configuration of `bib' -*- lexical-binding: t; -*-

;; Copyright (C) 2022 semenInRussia

;; Author: semenInRussia <hrams205@gmail.com>
;; Version: 0.1
;; Homepage: https://github.com/semeninrussia/emacs.el

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

;; My configuration of `bib'.

;;; Code:



(require 'dash)

(leaf bibtex
  :custom ((bibtex-align-at-equal-sign  . t)
           (bibtex-user-optional-fields .
                                        '(("file" "Link to document file."
                                           ":")))
           (bibtex-dialect . 'biblatex))
  :bind ((:bibtex-mode-map
          ([remap my-format-expression] . 'bibtex-reformat)))
  :config                               ;nofmt
  (leaf bibtex-utils :ensure t))

(leaf company-bibtex
  :ensure (company-bibtex :repo "semenInRussia/company-bibtex")
  :defvar company-backends
  :hook (org-mode-hook . company-bibtex-org-mode-hook)
  :custom (company-bibtex-org-citation-regex . "\\(ebib:\\|cite:@\\)")
  :config (add-to-list 'company-backends 'company-bibtex)
  (defun company-bibtex-org-mode-hook ()
    "Hook for `org-mode' enabling `comapany-bibtex' for current buffer."
    (interactive "P")
    (->>
     company-backends
     (--remove
      (and (listp it) (eq (car it) 'company-bbdb)))
     (setq-local company-backends))))



;;; my-bib.el ends here
;;; my-c.el --- My configuration of c and c++ languages -*- lexical-binding: t; -*-

;; Copyright (C) 2023 semenInRussia

;; Author: semenInRussia <hrams205@gmail.com>
;; Version: 0.1
;; Homepage: https://github.com/semeninrussia/emacs.el

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

;; My configuration of c and c++ languages

;;; Code:



(leaf cc-mode
  :ensure t
  :config                               ;nofmt

  (leaf google-c-style
    :ensure t
    :hook (c++-mode-hook . google-set-c-style)))



;;; my-c.el ends here
;;; my-calc.el --- My configuration of `calc' -*- lexical-binding: t; -*-

;; Copyright (C) 2022 semenInRussia

;; Author: semenInRussia <hrams205@gmail.com>
;; Version: 0.1
;; Homepage: https://github.com/semeninrussia/emacs.el

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

;; My configuration of `calc'.  For main configuration of the `calc' visit
;; the file ~/.emacs.d/calc.el created automatically by `calc'

;;; Code:




(require 'dash)
(require 's)

(defvar my-calc-operations
  '(calcDigit-start
    calc-convert-units
    calc-algebraic-entry
    calc-solve-for
    calc-store)
  "List of the function after which will be actived insert mode.")

(leaf calc
  :defun (calc-yank-internal calc-pack calc-vector-mean)
  :bind ((:calc-mode-map                ;nofmt
          :package calc
          ("v" . nil)
          ("v y" . my-calc-mean-yank))
         (:calc-edit-mode-map
          :package calc-yank
          ([remap save-buffer] . calc-edit-finish)))
  :config                               ;nofmt
  (defun my-calc-mean-yank (vec)
    "Yank to calculator vector of numbers VEC as string and compute mean.

When call interactively, VEC equal lines of the clipboard as numbers, same
mechanism use `calc-yank'"
    (interactive
     (list
      (->>
       (current-kill 0 t)
       (s-split-words)
       (-remove-item "•")
       (s-join "\n"))))
    (calc-yank-internal 0 vec)
    (calc-pack (length (s-lines vec)))
    (calc-vector-mean nil)))



;;; my-calc.el ends here
;;; my-css.el --- My configuration for `css'

;; Copyright (C) 2022 Semen Khramtsov

;; Author: Semen Khramtsov <hrams205@gmail.com>
;; Version: 0.1
;; URL: https://github.com/semenInRussia/emacs.el

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; My configuration for `css'

;;; Code:



(require 'dash)
(require 'fast-exec)

(leaf facemenu
  :fast-exec ("Display All Colors" #'list-colors-display))

(leaf css-mode
  :config                               ;nofmt
  (leaf css-eldoc
    :ensure t
    :hook (((css-mode-hook web-mode-hook)
            . css-eldoc-enable))))



;;; my-css.el ends here
;;; my-docker.el --- My configuration of `docker' -*- lexical-binding: t; -*-

;; Copyright (C) 2022 semenInRussia

;; Author: semenInRussia <hrams205@gmail.com>
;; Version: 0.1
;; Homepage: https://github.com/semeninrussia/emacs.el

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

;; My configuration of `docker'.

;;; Code:



(leaf dockerfile-mode :ensure t)
;; (leaf docker :ensure t)



;;; my-docker.el ends here
;;; my-elm.el --- My configuration for the elm language -*- lexical-binding: t; -*-

;; Copyright (C) 2023 semenInRussia

;; Author: semenInRussia <hrams205@gmail.com>
;; Version: 0.1
;; Homepage: https://github.com/semeninrussia/emacs.el

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

;; My configuration for the elm language.

;;; Code:




(leaf elm-mode
  :ensure t
  :hook (elm-mode-hook . my-lsp-ensure)
  :bind (:elm-mode-map
         ([remap my-format-expression] . elm-format)))



;;; my-elm.el ends here
;;; my-feature.el --- My configuration of `feature' -*- lexical-binding: t; -*-

;; Copyright (C) 2022 semenInRussia

;; Author: semenInRussia <hrams205@gmail.com>
;; Version: 0.1
;; Homepage: https://github.com/semeninrussia/emacs.el

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

;; My configuration of `feature-mode'.

;;; Code:




;; NOTE that I support only `ecukes' now
(leaf feature-mode
  :ensure t
  :major-mode-map feature
  :hook (feature-mode-hook . my-feature-mode-hook)
  :bind (:feature-mode-map
         ("RET" . newline-and-indent)
         ("M-RET" . my-feature-add-and-statement))
  :config                               ;nofmt
  (defun my-feature-mode-hook ()
    "My hook of the `feature-mode'."
    (setq-local outline-regexp "\\( *Feature:\\| *Scenario:\\)"))

  (defun my-feature-add-and-statement ()
    "Add a \"And\" feature statement, like to statement at the current line."
    (interactive)
    (duplicate-current-line 1)
    (forward-line 1)))



;;; my-feature.el ends here
;;; my-go.el --- My configuration of the `go'

;; Copyright (C) 2022 Semen Khramtsov

;; Author: Semen Khramtsov <hrams205@gmail.com>
;; Version: 0.1
;; URL: https://github.com/semenInRussia/emacs.el

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; My configuration of the `go'

;;; Code:



(declare-function my-lsp-ensure "my-lsp")

(leaf go-mode
  :ensure t
  :major-mode-map go
  :hook (go-mode-hook . my-lsp-ensure))



;;; my-go.el ends here
;;; my-haskell.el --- My config for `haskell'

;; Copyright (C) 2022 Semen Khramtsov

;; Author: Semen Khramtsov <hrams205@gmail.com>
;; Version: 0.1
;; URL: https://github.com/semenInRussia/emacs.el

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; My config for `haskell'.  Heavily inspired with `emacs-haskell-tutorial'
;; https://github.com/serras/emacs-haskell-tutorial/

;;; Code:



(leaf haskell-mode
  :ensure t
  ;; :ensure-system-package (("hoogle" . "cabal install hoogle"))
  :hook ((haskell-mode-hook . haskell-indent-mode)
         (haskell-mode-hook . interactive-haskell-mode)
         (haskell-mode-hook . my-lsp-ensure)))



;;; my-haskell.el ends here
;;; my-html.el --- My configuration for html

;; Copyright (C) 2022 Semen Khramtsov

;; Author: Semen Khramtsov <hrams205@gmail.com>
;; Version: 0.1
;; URL: https://github.com/semenInRussia/emacs.el

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; My configuration for html

;;; Code:


(require 'dash)
(require 'custom)

(defvar my-html-suported-modes
  '(web-mode mhtml-mode)
  "List of `html` major modes."
  ;; :group 'my
  ;; :type '(repeat symbol)
  )

(defun my-html-suported-modes-hooks ()
  "Return list from the hooks for each of `my-html-suported-modes'."
  (-map 'my-major-mode-to-hook my-html-suported-modes))

(defun my-html-suported-modes-maps ()
  "Return list from the maps for each of `my-html-suported-modes'."
  (-map 'my-major-mode-to-map my-html-suported-modes))

(leaf mhtml-mode
  :ensure t
  :mode "\\.html$"
  :major-mode-map `(html ,my-html-suported-modes)
  :hook (mhtml-mode-hook . my-lsp-ensure)
  :config                               ;nofmt
  (leaf auto-rename-tag
    :ensure t
    :hook `(,(my-html-suported-modes-hooks)
            . auto-rename-tag-mode))

  (leaf tagedit
    :ensure t
    :bind `(,(--map
              `(,it
                :package ,(my-map-to-major-mode it)
                ([remap sp-kill-hybrid-sexp] . tagedit-kill)
                ([remap sp-join-sexp]        . tagedit-join-tags)
                ([remap sp-raise-sexp]       . tagedit-raise-tag)
                ([remap sp-splice-sexp]      . tagedit-splice-tag)
                ([remap sp-change-enclosing]  . tagedit-kill-attribute))
              (my-html-suported-modes-maps))))

  (leaf emmet-mode :ensure t :hook mhtml-mode-hook)

  (leaf impatient-mode
    :ensure t
    :defun (imp-visit-buffer impatient-mode)
    :bind (:my-html-local-map
           :package mhtml-mode
           ("e" . my-enable-impatient-mode))
    :config                             ;nofmt
    (defun my-enable-impatient-mode ()
      "Enable `impatient-mode' open page of the file in the web browser."
      (interactive)
      (impatient-mode +1)
      (imp-visit-buffer))))



;;; my-html.el ends here
;;; my-js.el --- My configuration for JavaScript and TypeScript

;; Copyright (C) 2022 Semen Khramtsov

;; Author: Semen Khramtsov <hrams205@gmail.com>
;; Version: 0.1
;; URL: https://github.com/semenInRussia/emacs.el

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>

;;; Commentary:

;; My configuration for JavaScript and TypeScript

;;; Code:




(leaf js
  :ensure t
  :defvar lsp-bridge-single-lang-server-mode-list
  :hook (js-mode-hook . my-lsp-ensure)
  :defvar lsp-bridge-multi-lang-server-mode-list
  :mode "\\.js$"
  :config                               ;nofmt
  (require 'lsp-bridge)
  (add-to-list 'lsp-bridge-multi-lang-server-mode-list
               '((typescript-mode js-mode)
                 . "typescript_rome"))
  (leaf js-comint :ensure t))

(leaf typescript-mode
  :ensure t
  :hook (typescript-mode-hook . my-lsp-ensure)
  :custom (typescript-indent-level . 2)
  :config                               ;nofmt
  (require 'lsp-bridge)
  (add-to-list 'lsp-bridge-multi-lang-server-mode-list
               '((typescript-mode js-mode)
                 . "typescript_rome")))



;;; my-js.el ends here
;;; my-json.el --- My configuration for json

;; Copyright (C) 2022 Semen Khramtsov

;; Author: Semen Khramtsov <hrams205@gmail.com>
;; Version: 0.1
;; URL: https://github.com/semenInRussia/emacs.el

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; My configuration for json

;;; Code:




(leaf json-mode
  :ensure t
  :bind (:json-mode-map
         ([:remap my-format-expression] . json-pretty-print-buffer))
  :hook (json-mode-hook . my-json-fix-indent-funcs)
  :config                               ;nofmt
  (leaf json-snatcher
    :ensure t
    :bind (:json-mode-map
           :package json-mode
           ("C-c M-w" . jsons-print-path)))

  (defun my-json-fix-indent-funcs ()
    "Fix the functions that changes indent in JSON files."
    (interactive)
    (setq-local js-indent-level 2)))



;;; my-json.el ends here
;;; my-lang-utils.el --- My utils for language configuration

;; Copyright (C) 2022 Semen Khramtsov

;; Author: Semen Khramtsov <hrams205@gmail.com>
;; Version: 0.1
;; URL: https://github.com/semenInRussia/emacs.el

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:




(require 'face-remap)

(leaf visual-fill-column
  :ensure t
  :commands ()
  :defun visual-fill-column-mode)

(add-hook 'prog-mode-hook #'visual-fill)

(defun visual-fill (&optional width)
  "Make text buffer more pretty with centering it at horizontal.

WIDTH is the amount of characters that will be located within display"
  (interactive)
  (or width (setq width 70))
  (setq-default visual-fill-column-width width
                visual-fill-column-center-text t)
  (text-scale-mode 0)
  (visual-fill-column-mode 1))

(add-hook 'prog-mode-hook
          (lambda () (interactive) (toggle-truncate-lines 1)))



;;; my-lang-utils.el ends here
;;; my-latex.el --- My config for LaTeX

;; Copyright (C) 2022 Semen Khramtsov

;; Author: Semen Khramtsov <hrams205@gmail.com>
;; Version: 0.1
;; URL: https://github.com/semenInRussia/emacs.el

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; My config for LaTeX

;;; Code:



(require 'f)
(require 'yasnippet)
(require 'dash)




(require 'smartparens)

(declare-function aas-set-snippets "aas.el")

(defvar autoformat-latex-enumerate-list-items-triggers)
(defvar autoformat-latex-itemized-list-items-triggers)

(declare-function my-embrace-add-paren-of-cdlatex-math "my-latex.el")
(declare-function my-embrace-add-paren-latex-command "my-latex.el")
(declare-function my-embrace-add-paren-latex-style-command "my-latex.el")
(declare-function my-latex-style-command-left-paren-regexp "my-latex.el")
(declare-function my-latex-style-command-left-paren "my-latex.el")

(declare-function autoformat-latex-expand-to-enumerate-list-item-p "my-latex.el")

(declare-function TeX-master-file-ask "tex.el")
(declare-function cdlatex-wrap-environment "cdlatex.el")

(declare-function texmathp "texmathp.el")

(defvar cdlatex-command-alist-comb)
(defvar cdlatex-math-modify-alist)
(defvar cdlatex-math-modify-alist-default)

(defcustom my-latex-master-files-alist
  '(("~/zms/*/solutions/*.tex" . "../Solution.tex"))
  "Associated list, keys are wildcards, values are him master files."
  :type '(alist :key-type string :value-type string)
  :group 'my)

(defun my-latex-find-master-file ()
  "Find auctex master file for the current buffer."
  (interactive)
  (setq-local TeX-master
              (and
               (buffer-file-name)
               (or
                (my-latex-lookup-master-file-of (buffer-file-name))
                (TeX-master-file-ask)))))

(defun my-latex-lookup-master-file-of (filename)
  "Lookup a auctex master file for the file with FILENAME."
  (->>
   my-latex-master-files-alist
   (--find (my-latex-master-file-key-matches-with-p it filename))
   (cdr)))

(defun my-latex-master-file-key-matches-with-p (master-file-key filename)
  "Return t, when master file key alist MASTER-FILE-KEY match with FILENAME."
  (->
   master-file-key
   (car)
   (f-full)
   (wildcard-to-regexp)
   (string-match-p filename)))

(defvar latex-documentclasses
  '("article"
    "reoport"
    "book"
    "proc"
    "minimal"
    "slides"
    "memoir"
    "letter"
    "beamer")
  "List of the names for built-in LaTeX documentclasses.")

(add-hook 'LaTeX-mode-hook 'visual-fill)
(add-hook 'LaTeX-mode-hook 'turn-off-flycheck)

(add-hook 'LaTeX-mode-hook 'my-latex-find-master-file)
(add-hook 'LaTeX-mode-hook 'my-latex-expansion-mode)
(add-hook 'LaTeX-mode-hook 'my-latex-disable-auto-fill)

(leaf latex
  :ensure auctex
  :mode ("\\.tex$" . latex-mode)
  :defun (((my-latex-command-left-paren my-latex-command-left-paren-regexp)
           . my-latex)
          ((er/mark-LaTeX-math er/mark-LaTeX-inside-environment)
           . expand-region)
          (latex-complete-envnames . tex-mode)
          ((LaTeX-current-environment
            LaTeX-insert-item
            LaTeX-env-item
            LaTeX-find-matching-begin
            LaTeX-find-matching-end
            LaTeX-mark-section)
           . latex))
  :custom (TeX-master . nil)
  :bind (:LaTeX-mode-map
         ("C-c C-@"  . my-latex-mark-inside-environment-or-math)
         ("C-c C-\\" . my-latex-equation-to-split)
         ("C-c C-w"  . my-latex-kill-section))
  :config                               ;nofmt
  (leaf xenops
    :hook LaTeX-mode-hook
    :ensure t
    :custom (xenops-math-image-scale-factor . 2))

  (leaf my-latex-insert
    :load-path* "lisp/languages/latex/"
    :hook (LaTeX-mode-hook . my-latex-expansion-mode))

  (leaf yasnippet
    :bind (:yas-keymap
           ("<tab>" . yas-next-field-or-cdlatex)
           ("TAB"   . yas-next-field-or-cdlatex))
    :config                             ;nofmt
    (require 'calc-lang)
    (require 'font-latex)

    (defun cdlatex-in-yas-field ()
      (when-let* ((_ (overlayp yas--active-field-overlay))
                  (end (overlay-end yas--active-field-overlay)))
        (if (>= (point) end)
            (let ((s (thing-at-point 'sexp)))
              (unless (and s
                           (assoc
                            (substring-no-properties s)
                            cdlatex-command-alist-comb))
                (yas-next-field-or-maybe-expand)
                t))
          (let (cdlatex-tab-hook minp)
            (setq minp
                  (min
                   (save-excursion (cdlatex-tab) (point))
                   (overlay-end yas--active-field-overlay)))
            (goto-char minp)
            t))))

    (defun yas-next-field-or-cdlatex nil
      "Jump to the next Yas field correctly with cdlatex active."
      (interactive)
      (if (or
           (bound-and-true-p cdlatex-mode)
           (bound-and-true-p org-cdlatex-mode))
          (cdlatex-tab)
        (yas-next-field-or-maybe-expand))))

  (defun my-latex-mark-inside-environment-or-math ()
    "If the cursor place inside of the math environment mark that."
    (interactive)
    (if (texmathp)
        (er/mark-LaTeX-math)
      (er/mark-LaTeX-inside-environment)))

  (leaf laas
    :ensure t
    :hook LaTeX-mode-hook
    :aas (laas-mode
          :cond #'texmathp
          ;; Some Physics Units
          "As" "\\mathrm{А}"
          "Vs"  "\\mathrm{В}"
          "Oms"  "\\mathrm{Ом}"
          "cls" "^\\circ C"

          ;; Some Physics Sheet
          "eqv" "\\mathrm{Экв.}"

          ;; Some Cool Symbols
          "trg" "\\triangle"
          "agl" "\\angle"
          "grd" "^\\circ"))

  (leaf cdlatex
    :ensure t
    :hook ((cdlatex-tab-hook . yas-expand)
           (cdlatex-tab-hook . cdlatex-in-yas-field)
           (LaTeX-mode-hook  . turn-on-cdlatex))
    :bind (:cdlatex-mode-map
           ("<tab>" . cdlatex-tab)
           (";" . my-latex-dollar)
           ("(" .  self-insert-command)
           (")" .  self-insert-command)
           ("{" .  self-insert-command)
           ("}" .  self-insert-command)
           ("[" .  self-insert-command)
           ("]" .  self-insert-command)
           ("\"" . self-insert-command)
           ("\\" . self-insert-command))
    ;; fields
    :custom (cdlatex-math-modify-alist
             .
             '((?q "\\sqrt" nil t nil nil)
               (?u "\\breve" "\\uline" t nil nil)
               (?v "\\vec" nil t nil nil)))
    :config                             ;nofmt
    (defun my-latex-dollar ()
      "Insert dollars and turn input method into English."
      (interactive)
      ;; when current-input-method isn standard
      (if (not current-input-method)
          ;; then
          (insert ";")
        ;; else
        (toggle-input-method)
        (if (use-region-p)
            (sp-wrap-with-pair "$")
          (sp-insert-pair "$")))))

  (leaf embrace
    :after (embrace cdlatex)
    :defun (embrace-build-help embrace-add-pair-regexp)
    :hook ((LaTeX-mode-hook . embrace-LaTeX-mode-hook)
           (LaTeX-mode-hook . my-embrace-LaTeX-mode-hook))
    :config                             ;nofmt
    (defun my-embrace-LaTeX-mode-hook ()
      "My additional `embrace-LaTeX-mode-hook'."
      (interactive)
      (setq-local embrace-show-help-p nil)
      (--each
          (-concat cdlatex-math-modify-alist-default
                   cdlatex-math-modify-alist)
        (my-embrace-add-paren-of-cdlatex-math it))
      (my-embrace-add-paren-latex-command ?a "answer")
      (embrace-add-pair-regexp ?\\
                               (rx "\\"
                                   (1+ wordchar)
                                   (* space)
                                   (? "[" (*? any) "]" (* space))
                                   "{")
                               "}"
                               'my-embrace-with-latex-command
                               (embrace-build-help "\\name{" "}"))
      (embrace-add-pair-regexp ?d
                               "\\\\left."
                               "\\\\right."
                               'my-embrace-with-latex-left-right
                               (embrace-build-help
                                "\\left(" "\\right)"))
      (embrace-add-pair-regexp
       ?e
       "\\\\begin{\\(.*?\\)}\\(\\[.*?\\]\\)*"
       "\\\\end{\\(.*?\\)}"
       'my-embrace-with-latex-env
       (embrace-build-help "\\begin{name}" "\\end{name}")
       t))

    (defun my-embrace-add-paren-of-cdlatex-math (element)
      "Add an ELEMENT of the `cdlatex-math-modify-alist' to the `embrace' parens."
      (let* ((key (-first-item element))
             (cmd
              (s-chop-prefix
               "\\"
               (or (-third-item element) (-second-item element))))
             (type (-fourth-item element)))
        (if type
            (my-embrace-add-paren-latex-command key cmd)
          (my-embrace-add-paren-latex-style-command key cmd))))

    (defun my-embrace-add-paren-latex-command (key name)
      "Add paren at KEY for the LaTeX command with NAME in `embrace'."
      (embrace-add-pair-regexp
       key
       (my-latex-command-left-paren-regexp name)
       "}"
       (-const (cons (my-latex-command-left-paren name) "}"))
       (embrace-build-help (my-latex-command-left-paren name) "}")))

    (defun my-latex-command-left-paren (name)
      "Return paren right of the LaTeX command named NAME."
      (s-concat "\\" name "{"))

    (defun my-latex-command-left-paren-regexp (name)
      (rx "\\"
          (literal name)
          (* space)
          (? "[" (*? any) "]" (* space))
          "{"))

    (defun my-embrace-add-paren-latex-style-command (key name)
      "Add paren at KEY for the style LaTeX command with NAME in `embrace'."
      (embrace-add-pair-regexp key
                               (my-latex-style-command-left-paren-regexp name)
                               "}"
                               (-const
                                (cons
                                 (my-latex-style-command-left-paren name)
                                 "}"))
                               (embrace-build-help
                                (my-latex-style-command-left-paren name)
                                "}")))

    (defun my-latex-style-command-left-paren (name)
      "Return paren right of the LaTeX command named NAME."
      (s-concat "{\\" name " "))

    (defun my-latex-style-command-left-paren-regexp (name)
      (rx "{" (* space) "\\" (literal name) (* space)))

    (defun my-embrace-with-latex-command ()
      "Return pair from the left and right pair for a LaTeX command."
      (let ((name (read-string "Name of a LaTeX command, please: ")))
        (cons (s-concat "\\" name "{") "}")))

    (defun my-embrace-with-latex-left-right ()
      "Return pair from the left and right pair for the LaTeX command \\left."
      (cons
       (s-concat "\\left" (read-char "Left paren, please: "))
       (s-concat "\\right" (read-char "Right paren, please: "))))

    (defun my-embrace-with-latex-env ()
      "Return pair from the left and right pair for the LaTeX command \\left."
      (let ((env
             (read-string "Name of the environment, please: "
                          (latex-complete-envnames))))
        (cons
         (s-concat "\\begin{" env "}")
         (s-concat "\\end{" env "}")))))

  (leaf smartparens-latex
    :after smartparens
    :config                             ;nofmt
    (sp-with-modes
        '(tex-mode plain-tex-mode latex-mode LaTeX-mode)
      (sp-local-pair " ``"
                     "''"
                     :trigger "\""
                     :unless '(sp-latex-point-after-backslash sp-in-math-p)
                     :post-handlers '(sp-latex-skip-double-quote))
      (sp-local-pair " \"<"
                     "\">"
                     :trigger "<"
                     :unless '(sp-latex-point-after-backslash sp-in-math-p))))

  (leaf latex-extra
    :ensure t
    :hook ((LaTeX-mode-hook . latex-extra-mode)
           (LaTeX-mode-hook . visual-line-mode))
    :bind (:LaTeX-mode-map
           :package tex
           ("C-c C-c" . latex/compile-commands-until-done)
           ("C-c C-n" . latex/next-section-same-level)
           ("C-c C-p" . latex/previous-section-same-level)))

  (leaf company-math
    :ensure t
    :hook (LaTeX-mode-hook . my-company-math-setup)
    :config                             ;nofmt
    (defun my-company-math-setup ()
      "Setup for `company-math'."
      (add-to-list 'company-backends 'company-math-symbols-latex)
      (add-to-list 'company-backends 'company-latex-commands)))

  (leaf company-auctex
    :ensure t
    :require t
    :defun company-auctex-init
    :after auctex
    :config (company-auctex-init))

  (leaf my-latex-math-spaces :hook latex-mode)

  (leaf latex-r
    :load-path "~/projects/latex-r"
    :bind (:LaTeX-mode-map
           :package latex
           ("C-c M-n" . 'latex-r-cycle-math-parens)
           ("C-c C-s" . 'latex-r-split-environment)))

  (defun my-latex-disable-auto-fill ()
    "Disable `auto-fill-mode'."
    (interactive)
    (auto-fill-mode 0))

  (leaf my-latex-drag
    :defun ((add-up-dragger add-down-dragger) . my-drag)
    :commands (my-latex-try-drag-left-list-item
               my-latex-try-drag-right-list-item)
    :init
    (add-up-dragger 'my-latex-try-drag-left-list-item)
    (add-down-dragger 'my-latex-try-drag-right-list-item)))



;;; my-latex.el ends here
;;; my-lyrics.el --- My configuration of `lyrics' -*- lexical-binding: t; -*-

;; Copyright (C) 2022 semenInRussia

;; Author: semenInRussia <hrams205@gmail.com>
;; Version: 0.1
;; Homepage: https://github.com/semeninrussia/emacs.el

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

;; My configuration of `lyrics'.

;;; Code:





(require 'just)

(leaf my-lyrics
  :mode ("\\.lrc$" . my-lyrics-mode)
  :defer-config                               ;nofmt
  (defun my-lyrics-outline-level ()
    "Return of the heading level for `my-lyrics-mode'.

In `my-lirics-mode' outline line is the following:

[intro]

If you should define outline heading with greater level, then you should add
spaces before [, for example

  [intro]"
    (-
     (save-excursion (beginning-of-line-text) (point))
     (pos-bol)))

  (defun my-lyrics-indent-line ()
    "Indent current line."
    (unless (just-line-prefix-p "[" nil t)
      (beginning-of-line-text)
      (delete-region (point) (pos-bol))
      (insert
       (just-call-on-prev-line*
        (when (just-line-is-whitespaces-p) (forward-line -1))
        (beginning-of-line)
        (make-string
         ;; it's return spaces at the line start
         (skip-chars-forward " ")
         ? )))))

  (defvar my-lyrics-outline-regexp "^ *\\[.*?]"
    "Regexp indicating outline heading line in `my-lyrics-mode'.")

  (defvar my-lyrics-highlights
    `((,my-lyrics-outline-regexp . font-lock-keyword-face))
    "Thing for `my-lyrics-mode' font-lock.")

  (defface my-lyrics-outline-face
    '((t (:foreground "#Ee9a00")))
    "Face to fontify outline headings in `my-lyrics-mode'.

Outline headings demote lines which has the following form

[intro]

instead of intro can be other word"
    :group 'my)

  (define-derived-mode my-lyrics-mode text-mode "Lyrics"
    "Major mode to edit lyrics for songs (panches)."
    (setq-local outline-regexp my-lyrics-outline-regexp)
    (setq-local outline-level 'my-lyrics-outline-level)
    (setq-local comment-start "#")
    (setq-local indent-line-function 'my-lyrics-indent-line)
    (setq-local indent-region-function 'indent-region-line-by-line)
    (visual-fill)
    (setq-local font-lock-defaults '(my-lyrics-highlights))))



;;; my-lyrics.el ends here
;;; my-markdown.el --- My configuration for `markdown-mode'

;; Copyright (C) 2022 Semen Khramtsov

;; Author: Semen Khramtsov <hrams205@gmail.com>
;; Version: 0.1
;; URL: https://github.com/semenInRussia/emacs.el

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; My configuration for `markdown-mode'

;;; Code:



(require 'just)



(defcustom my-markdown-imenu-generic-expression
  '(("title""^\\(.*\\)[\n]=+$" 1)
    ("h2-" "^\\(.*\\)[\n]-+$" 1)
    ("h1"   "^# \\(.*\\)$" 1)
    ("h2"   "^## \\(.*\\)$" 1)
    ("h3"   "^### \\(.*\\)$" 1)
    ("h4"   "^#### \\(.*\\)$" 1)
    ("h5"   "^##### \\(.*\\)$" 1)
    ("h6"   "^###### \\(.*\\)$" 1)
    ("fn" "^\\[\\^\\(.*\\)\\]" 1))
  "List of the specific for `markdown-mode' generic expressions.

See `imenu-generic-expression'"
  :group 'my
  :type '(repeat string))

(defun my-markdown-first-letter-of-heading-p ()
  "Return non-nil, when the cursor placed at the `markdown' heading start."
  (save-excursion
    (forward-char -1)
    (skip-chars-backward " #")
    (bolp)))

(defun autoformat-markdown-capitalize-heading-line ()
  "Capitalize first letter of a heading line (lines which started with #)."
  (and
   (just-line-prefix-p "#")
   (my-markdown-first-letter-of-heading-p)
   (upcase-char -1)))

(leaf markdown-mode
  :ensure t
  :hook (markdown-mode-hook .
                            (lambda ()
                              (setq-local
                               imenu-generic-expression
                               my-markdown-imenu-generic-expression)))
  :config                               ;nofmt
  (add-hook 'markdown-mode-hook 'visual-fill)
  (leaf markdown-toc
    :ensure t
    :bind (:markdown-mode-map
           :package markdown-mode
           ("C-T" . markdown-toc-generate-or-refresh-toc)))

  (leaf edit-indirect :ensure t)

  (my-autoformat-bind-for-major-mode
   'markdown-mode
   'autoformat-markdown-capitalize-heading-line
   'my-autoformat-sentence-capitalization))



;;; my-markdown.el ends here
;;; my-nushell.el --- My configuration of `nushell' -*- lexical-binding: t; -*-

;; Copyright (C) 2022 semenInRussia

;; Author: semenInRussia <hrams205@gmail.com>
;; Version: 0.1
;; Homepage: https://github.com/semeninrussia/emacs.el

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

;; My configuration of `nushell'.

;;; Code:


(defvar aggressive-indent-excluded-modes)

(leaf nushell-mode
  :ensure (nushell-mode :host github :repo "azzamsa/emacs-nushell"))

(leaf yenushell
  :load-path "~/projects/yenushell/"
  :mode ("\\.nu$" . yenushell-mode)
  :config (add-to-list
           'aggressive-indent-excluded-modes
           'yenushell-mode))



;;; my-nushell.el ends here
;;; my-org.el --- My configuration for `org-mode'

;; Copyright (C) 2022 Semen Khramtsov

;; Author: Semen Khramtsov <hrams205@gmail.com>
;; Version: 0.1
;; URL: https://github.com/semenInRussia/emacs.el

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; My configuration for `org-mode'

;;; Code:



(require 's)
(require 'just)


(require 'dash)



(declare-function meow-insert "meow-command.el")
(declare-function aas-set-snippets "aas.el")
(declare-function my-org-list-item-p "my-org.el")
(declare-function my-org-properties-end-p "my-org.el")
(declare-function my-org-heading-p "my-org.el")
(declare-function my-autoformat-bind-for-major-mode "my-autoformat.el")

(leaf org
  :ensure t
  :defun ((transient-define-prefix . transient)
          (my-org-keywords . my-org)
          (my-org-skip-backward-keyword . my-org)
          (embrace-org-mode-hook . my-org)
          org-current-level)
  :custom ((org-refile-use-outline-path . nil)
           (org-fold-core-style  . 'overlays)
           (org-refile-targets   . '((org-agenda-files :maxlevel . 2)))
           (org-startup-folded   . t)
           (org-startup-indented . t)
           (org-startup-with-inline-images . t)
           (org-file-apps .
                          '(("\\.\\'" . default)
                            ("\\.pdf\\'" . "start %s")
                            ("\\.png\\'" . "start %s")
                            ("\\.jpg\\'" . "start %s"))))
  :defvar (my-org-list-item-prefix-regexp
           my-org-keywords)
  :hook ((org-mode-hook . org-cdlatex-mode))
  :bind (("<f5>" . org-ctrl-c-ctrl-c)
         ("C-c c" . org-capture)
         (:org-mode-map
          ;; Insert anything
          ("C-c M-i"   . my-org-insert-image)
          ("C-c M-u"   . my-org-insert-img-at-url)

          ;; Manipulations with a subtree
          ("C-c C-w"   . my-org-cut)
          ;; heading => plain text
          ;; 8 is * without shift
          ("C-c C-M-w"   . my-org-clear-subtree)
          ([tab] . org-refile)
          ("C-c C-t"   . my-org-todo)
          ("C-c C-j"   . org-id-get-create)))
  ;; the following code should add some auto activating snippets, for example,
  ;; if I type "exthe", then it should be extended to the "Explore the"
  ;; see `aas-mode'
  :aas (org-mode
        "exthe" "explore the"
        "Exthe" "Explore the"
        "misc " "miscellaneous"
        "Misc " "Miscellaneous"
        "iau" "I am use")
  :config                               ;nofmt
  (add-hook 'org-mode-hook 'visual-fill)
  (add-hook 'org-mode-hook 'visual-line-mode)
  (add-hook 'org-mode-hook 'aas-activate-for-major-mode)
  (leaf my-org-editing
    :commands (my-org-clear-subtree
               my-org-clear-subtree
               my-org-cut
               my-org-indent-subtree
               my-org-indent-subtree
               my-org-insert-image
               my-org-schedule-to-today
               my-org-todo
               my-org-todo))

  (leaf xenops :ensure t :hook org-mode-hook)

  (leaf my-org-autoformat
    :config                             ;nofmt
    (defcustom my-org-list-labels-regexps
      '("\\+" "-" "[0-9]+\\.")
      "List of the regexp prefixes indicates a label of a `org-mode' list item.

Label is thing which just decorates a list, but it's not item content, for
example in the following list

- a
- b
- c

Label is \"-\""
      :group 'my
      :type '(repeat string))

    (defcustom my-org-keywords
      '("TODO" "DONE")
      "List of the `org-mode' keywords sush as TODO or DONE."
      :group 'my
      :type '(repeat string))

    (defcustom my-org-list-label-regexp
      (my-regexp-opt-of-regexp my-org-list-labels-regexps)
      "Regexp indicates a item of a `org-mode' list.

Label is thing which just decorates a list, but it's not item content, for
example in the following list

- a
- b
- c

Label is \"-\", you should consider that spaces before label shouldn't be in the
regexp"
      :group 'my
      :type '(repeat string))

    (defcustom my-org-list-item-checkbox-regexp
      "\\[.\\]"
      "Regexp indicates a `org-mode' checkbox."
      :group 'my
      :type 'regexp)

    (defcustom my-org-list-item-prefix-regexp
      (rx line-start
          (0+ " ")
          (regexp my-org-list-label-regexp)
          (? (1+ " ") (regexp my-org-list-item-checkbox-regexp))
          (0+ " "))
      "Regexp indicates a list item."
      :group 'my
      :type 'regexp)

    (my-autoformat-bind-for-major-mode
     'org-mode
     'my-org-sentence-capitalization
     'my-org-list-item-capitalization
     'my-org-heading-capitalization)

    (defun my-org-sentence-capitalization ()
      "Capitalize first letter of a sentence in the `org-mode'."
      (interactive)
      (cond
       ((just-call-on-prev-line*
         (or
          (just-line-is-whitespaces-p)
          (my-org-heading-p)
          (my-org-properties-end-p)
          (my-org-list-item-p)))
        (my-autoformat-sentence-capitalization t))
       ((just-call-on-prev-line* (equal (pos-bol) (point-min)))
        (my-autoformat-sentence-capitalization))
       (t
        (just-call-on-backward-char*
         (and
          (looking-back my-autoformat-sentence-end nil)
          (looking-at-p "[[:alpha:]]")
          (upcase-char 1))))))

    (defun my-org-heading-p ()
      "Return t, when the cursor located at a `org-mode' heading text."
      ;; NOTE: don't handle cases when bold word located at the beginning of the
      ;; line.  For example:
      ;;
      ;; *bold word*
      ;;
      ;; this function in the above case return t, but excepted nil
      (just-line-prefix-p "*"))

    (defun my-org-list-item-capitalization ()
      "Capitalize first letter of a itemized list item."
      (interactive)
      (just-call-on-backward-char*
       (and
        (looking-at-p "[[:alpha:]]")
        (my-org-list-item-p)
        (looking-back my-org-list-item-prefix-regexp nil)
        (upcase-char 1))))

    (defun my-org-list-item-p ()
      "Return t, when the cursor located at an item of a `org-mode' list."
      (just-line-regexp-prefix-p my-org-list-item-prefix-regexp))

    (defun my-org-properties-end-p ()
      "Get t, if the point placed at the end of `org-mode' subtree properties."
      (string-equal (just-text-at-line nil t) ":END:"))

    (defun my-org-heading-capitalization ()
      "Capitalize first letter of a `org-mode' heading.

When `org-mode' heading has any keyword (like to TODO or DONE) first letter
demotes a first letter after keyword word."
      (interactive "d")
      (when (just-call-on-backward-char*
             (and
              (my-org-heading-p)
              (looking-at-p "[[:alpha:]]")
              (progn
                (skip-chars-backward " ")
                (my-org-skip-backward-keyword)
                (skip-chars-backward " *")
                (bolp))))
        (upcase-char -1)))

    (defun my-org-skip-backward-keyword ()
      "If right at the cursor placed `org-mode' keyword, then skipt it."
      (when (member (thing-at-point 'symbol) my-org-keywords)
        (backward-sexp))))

  (leaf org-download
    :ensure t
    :hook (dired-mode-hook . org-download-enable))

  (leaf org-keys
    :require t
    :custom ((org-use-speed-commands . t)
             (org-speed-commands
              .
              '(("k" . org-forward-heading-same-level)
                ("i" . org-backward-heading-same-level)
                ("j" . org-previous-visible-heading)
                ("l" . org-next-visible-heading)
                ("h" . my-org-goto-parent)
                ("z" . org-toggle-comment)
                ("x" . org-cut-subtree)
                ("d" . org-deadline)
                ("s" . org-schedule)
                ("w" . my-org-schedule-to-today)
                (" " . my-add-org-subtree-to-targets-on-day)
                ("'" . org-toggle-narrow-to-subtree)
                ("f" .
                 (progn
                   (skip-chars-forward "*")
                   (forward-char)
                   (meow-insert 0)))
                ("B" . org-previous-block)
                ("F" . org-next-block)
                ("g" . (org-refile t))
                ("c" . org-cycle)
                ("=" . org-columns)
                ("I" . org-metaup)
                ("K" . org-metadown)
                ("o" . org-metaright)
                ("u" . org-metaleft)
                ("O" . org-shiftmetaright)
                ("U" . org-shiftmetaleft)
                ("n"
                 .
                 (progn
                   (forward-char 1)
                   (call-interactively 'org-insert-heading-respect-content)))
                ("a" . org-archive-subtree-default-with-confirmation)
                ("6" . org-mark-subtree)
                ("t" . org-todo)
                ("," . (org-priority))
                ("0" . (org-priority 32))
                ("1" . (org-priority 65))
                ("2" . (org-priority 66))
                ("3" . (org-priority 67))
                (":" . org-set-tags-command)
                ("e" . org-set-effort)
                ("E" . org-inc-effort)
                ("/" . org-sparse-tree)
                ("?" . org-speed-command-help))))
    :bind (:org-mode-map
           :package org
           ("C-c M-{" . my-org-to-heading-start))
    :config                             ;nofmt
    (defun my-org-to-heading-start ()
      "Go to the beginning of the heading after activate insert mode."
      (interactive)
      (end-of-line)
      (search-backward-regexp "^\*" nil t)
      (meow-insert)
      (unless (eq current-input-method nil) (toggle-input-method)))

    (defun my-org-goto-parent ()
      "Go to the parent of the `org-mode' heading at the cursor."
      (interactive)
      (->>
       (->
        (org-current-level)
        (1-)
        (s-repeat "*")
        (regexp-quote))
       (s-prepend "^")
       (s-append " ")
       (re-search-backward))))

  (leaf consult
    :bind (:org-mode-map
           ([remap consult-imenu] . consult-outline)))

  ;; I am bind the command `org-export' with \"SPC l e\" in the root `leaf'
  (leaf ox
    :custom ((org-export-coding-system . 'utf-8)
             (org-export-with-smart-quotes . t)
             (org-latex-caption-above . '(table))
             (org-latex-default-figure-position . "H")
             (org-latex-image-default-width . "5cm")
             (org-latex-packages-alist .
                                       '(("AUTO" "babel" nil ("pdflatex"))
                                         ("AUTO" "polyglossia" t ("xelatex"))
                                         ("" "cmap" nil ("pdflatex"))
                                         ("" "float" nil
                                          ("pdflatex" "xelatex")))))
    :defer-config                       ;nofmt
    (leaf latex-extra
      :ensure t
      :defun latex/compile-commands-until-done
      :config                           ;nofmt
      (defun my-org-latex-compile (filename &optional snippet)
        "My version of the `ox-latex' way to compile a TeX file.

Using `latex/compile-commands-until-done'

TEXFILE is the name of the file being compiled.  Processing is
done through the command specified in `org-latex-pdf-process',
which see.  Output is redirected to \"*Org PDF LaTeX Output*\"
buffer.

When optional argument SNIPPET is non-nil, TEXFILE is a temporary
file used to preview a LaTeX snippet.  In this case, do not
create a log buffer and do not remove log files.

Return PDF file name or raise an error if it couldn't be
produced."
        (find-file filename)
        ;; if snippet - t, then not clean
        (latex/compile-commands-until-done (not snippet)))

      (defalias 'org-latex-compile 'my-org-latex-compile))

    (leaf ox-json :ensure t :require t)

    (leaf ox-beamer :require t))

  (leaf my-org-do-tidy
    :bind (:org-mode-map
           :package org
           ("C-c M-q" . my-org-tidy)))

  (leaf my-org-options
    :bind (:org-mode-map
           :package org
           ("C-c C-." . my-org-options-transient)))

  (leaf embrace
    :ensure t
    :hook (org-mode-hook . my-embrace-org-mode-hook)
    :config                             ;nofmt
    (defun my-embrace-org-mode-hook ()
      "Enable `embrace' specially for `org-mode'."
      (embrace-org-mode-hook)
      (setq-local embrace-show-help-p nil)))

  (leaf org-table-sticky-header :ensure t :hook org-mode-hook)
  (leaf org-autolist :ensure t :hook org-mode-hook)

  ;; (leaf rorg
  ;;   :load-path "~/projects/rorg/"
  ;;   :bind (:org-mode-map
  ;;          :package org
  ;;          ("C-c C-x C-x" . rorg-splice-subtree)
  ;;          ("C-c C-0" . rorg-wrap-region-or-current-heading)
  ;;          ("C-c C-{" . rorg-forward-slurp-subtree)
  ;;          ("C-c C-}" . rorg-backward-barf-subtree)
  ;;          ("{" . rorg-backward-slurp-subtree)
  ;;          ("[" . rorg-forward-barf-subtree))
  ;;   )

  (leaf my-org-drag
    :defun ((add-right-dragger
             add-left-dragger
             add-down-dragger
             add-up-dragger)
            . my-drag)
    :commands (my-drag-org-right
               my-drag-org-left
               my-drag-org-down
               my-drag-org-up)
    :init
    (add-right-dragger 'my-drag-org-right)
    (add-left-dragger 'my-drag-org-left)
    (add-down-dragger 'my-drag-org-down)
    (add-up-dragger 'my-drag-org-up)))



;;; my-org.el ends here
;;; my-python.el --- My configuration for `python'

;; Copyright (C) 2022 Semen Khramtsov

;; Author: Semen Khramtsov <hrams205@gmail.com>
;; Version: 0.1
;; URL: https://github.com/semenInRussia/emacs.el

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; My configuration for `python'

;;; Code:


(require 'dash)
(require 's)
(require 'just)
(require 'smartparens)

(leaf python-mode
  :ensure t
  :mode "\\.py\\'"
  :custom (python-shell-interpreter . "python")
  :hook ((python-mode-hook . my-python-fix-whitespaces-mode)
         (python-mode-hook . my-lsp-ensure))
  :custom ((lsp-bridge-python-lsp-server . nil)
           (lsp-bridge-python-multi-lsp-server . "pyright_ruff"))
  :bind (:python-mode-map
         ("C-c C-i" . py-sort-imports)
         ("C-c C-o" . my-python-optional-type)
         ("C-c M-p" . my-python-split-params))
  :config                               ;nofmt
  (defun my-python-split-multi-imports-in-1-line ()
    "Find all lines importing more then 1 thing from module and split it."
    (interactive)
    (save-excursion
      (goto-char (point-min))
      (while (search-forward-regexp
              "^from +\\(.*?\\) +import +\\(\\(.*?\\), *\\)+" nil t)
        (let ((line (just-text-at-line))
              (from (match-string 1)))
          (delete-region (pos-bol) (pos-eol))
          (--each
              (->>
               line
               (s-split "import")
               (-last-item)
               (s-split ",")
               (-map 's-trim))
            (insert "from " from " import " it)
            (newline))))))

  (defun my-python-optional-type (beg end)
    "Turn a python type in the active region into optional.

Active region is region from BEG to END"
    (interactive "r")
    (let ((init-pos (point)))
      (goto-char end)
      (insert "]")
      (goto-char beg)
      (insert "Optional[")))

  (defun my-python-fix-whitespaces-mode ()
    "Fix `whitespace-mode' for `python-mode'."
    (interactive)
    (setq-local whitespace-line-column 88))

  (defun my-python-split-params ()
    "Split params of a python def block into some lines."
    (interactive)
    (save-excursion
      (end-of-line)
      (search-backward "def")
      (forward-sexp)
      (sp-get
          (sp-get-sexp)
        (replace-string-in-region "," ",\n" :beg :end))
      (sp-get (sp-get-sexp) (indent-region-line-by-line :beg :end)))))



;;; my-python.el ends here
;;; my-rust.el --- My configuration for rust

;; Copyright (C) 2022 Semen Khramtsov

;; Author: Semen Khramtsov <hrams205@gmail.com>
;; Version: 0.1
;; URL: https://github.com/semenInRussia/emacs.el

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; My configuration for rust

;;; Code:



(require 'dash)
(require 's)
(require 'just)


(require 'f)

(declare-function embrace-add-pair-regexp "embrace.el")
(declare-function embrace-add-pair "embrace.el")

(autoload 'my-rust-find-Cargo.toml-in-directory "my-rust.el")


(defcustom my-rust-maybe-pub-words
  '(async fn mod struct enum type trait)
  "List of the symbols indicating words which can be public in Rust."
  :type '(repeat symbol)
  :group 'my)

(leaf rust-mode
  :ensure t
  :hook (rust-mode-hook . my-rust-whitespace-mode)
  :bind (:rust-mode-map
         ("C-c M-p" . 'my-rust-toggle-pub)
         ("C-c C-t" . 'my-rust-visit-Cargo.toml)
         ("C-c C-m" . 'rust-toggle-mutability))
  :config                               ;nofmt
  (add-hook 'rust-mode-hook #'my-lsp-ensure)

  (defun my-rust-toggle-pub ()
    "Toggle public/private scope of the current rust function/imple/struct."
    (interactive)
    (let ((line-start (pos-bol)))
      (save-excursion
        (end-of-line)
        (or
         ;; try search a keyword in the current line
         (--first
          (search-backward-regexp
           (s-concat (symbol-name it) " ")
           line-start t)
          my-rust-maybe-pub-words)
         ;; otherwise try search a keyword in the current buffer
         (--first
          (search-backward-regexp
           (s-concat (symbol-name it) " ")
           nil t)
          my-rust-maybe-pub-words))
        (if (looking-back "pub *" nil)
            (just-delete-word -1)
          (insert "pub ")))
      (repeat-at-last-keystroke)))

  (defun my-rust-find-Cargo.toml-in-directory (&optional dir)
    "Find closest Cargo.toml in the DIR and return path to it."
    (interactive)
    (setq dir (or dir default-directory))
    (let ((cargo.toml (f-join dir "Cargo.toml")))
      (if (f-exists-p cargo.toml)
          cargo.toml
        (my-rust-find-Cargo.toml-in-directory (f-parent dir)))))

  (defun my-rust-visit-Cargo.toml ()
    "Visit Cargo.toml file of current rust crate."
    (interactive)
    (find-file (my-rust-find-Cargo.toml-in-directory)))

  (leaf embrace
    :after embrace
    :hook (rust-mode-hook . my-rust-embrace-hook)
    :config
    (defun my-rust-embrace-hook ()
      "Add parens to `embrace' parens for `rust-mode'."
      (interactive)
      (embrace-add-pair ?v "Vec<" ">")
      (embrace-add-pair ?d "dbg!(" ")")
      (embrace-add-pair ?b "Box<" ">")
      (embrace-add-pair ?o "Option<" ">")
      (embrace-add-pair ?r "Result<" ">")
      (embrace-add-pair-regexp ?p
                               "print\\(ln\\)?!(\".*?\"," ")"
                               (lambda ()
                                 (interactive)
                                 (cons
                                  (concat
                                   "println!("
                                   (read-string
                                    "Template to format string: "
                                    "\"{}\"")
                                   ", ")
                                  ");"))))

    (defun my-rust-whitespace-mode ()
      "Change the `whitespace-mode' for `rust-mode'."
      (interactive)
      (whitespace-mode 0))))



;;; my-rust.el ends here
;;; my-sql.el --- My configuration of `sql' -*- lexical-binding: t; -*-

;; Copyright (C) 2022 semenInRussia

;; Author: semenInRussia <hrams205@gmail.com>
;; Version: 0.1
;; Homepage: https://github.com/semeninrussia/emacs.el

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

;; My configuration of `sql'.

;;; Code:



(leaf sql
  :ensure t
  :fast-exec ("Open SQL Lite Connection" 'sql-sqlite))



;;; my-sql.el ends here
;;; my-typst.el --- My configuration of `typst' -*- lexical-binding: t; -*-

;; Copyright (C) 2023 semenInRussia

;; Author: semenInRussia <hrams205@gmail.com>
;; Version: 0.1
;; Homepage: https://github.com/semeninrussia/emacs.el

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

;; My configuration of `typst'.

;;; Code:




(declare-function visual-fill "my-lang-utils.el")

(leaf typst-mode
  :ensure t
  :commands typst-mode
  :config (add-hook 'typst-mode-hook #'visual-fill))



;;; my-typst.el ends here
;;; my-dired.el --- My configuration of the `dired'

;; Copyright (C) 2022 Semen Khramtsov

;; Author: Semen Khramtsov <hrams205@gmail.com>
;; Version: 0.1
;; URL: https://github.com/semenInRussia/emacs.el

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; My configuration of the `dired'

;;; Code:




(defvar dired-filter-map)
(defvar my-dired-commands-using-minibuffer)

;; the most heavy functions placed at the `my-dired-commands'
;; and will be evaluated when really needed
(declare-function my-dired-save-excursion "my-dired-commands.el")
(declare-function visual-fill "my-lang-utils.el")

(require 's)
(require 'dash)
(require 'just)
(require 'f)

(leaf dired
  :hook ((dired-mode-hook . dired-hide-details-mode))
  :defun ((dired-mark
           dired-do-rename
           dired-current-directory
           dired-jump
           dired-get-file-for-visit)
          (org-link-open-from-string . ol))
  :bind (:dired-mode-map
         (";"       . dired-avy)
         ("A"       . agnifize-dwim)
         ("~"       . my-dired-jump-to-home)
         ("a"       . execute-extended-command)
         (","       . ace-window))

  :config
  (leaf dired-async
    :ensure async
    :defun dired-async-mode
    :global-minor-mode dired-async-mode)

  (leaf my-dired-commands
    :bind (:dired-mode-map
           :package dired
           ;; Mark anything
           ("C-x h"   . my-dired-mark-all-files)
           ;; Manipulation with file(s)
           ;; copy/move/paste also defines in the section "Dired Hacks: Ranger"
           ("k"       . my-dired-delete)
           ("C-c C-x" . my-dired-delete-all-files)
           ("C-y"     . my-dired-duplicate)
           ("R"       . my-dired-rename)
           ("TAB"     . my-dired-move)
           ("o"       . my-dired-new-file)
           ("b"       . my-dired-goto-parent-dir)))

  (leaf dired-filter
    :ensure t
    :require t
    :bind-keymap (:dired-mode-map
                  :package dired
                  ("." . 'dired-filter-map)))

  (leaf dired-open
    :ensure t
    :bind (:dired-mode-map
           :package dired
           ("RET" . 'dired-open-file))
    :push ((dired-open-functions . 'my-dired-open-function-pdf))
    :defvar dired-open-functions
    :defun (my-dired . (my-pdf-file my-try-open-pdf-file))
    :config                             ;nofmt
    (defun my-dired-open-function-pdf ()
      "Open function for `dired-open-functions'."
      (my-try-open-pdf-file (dired-get-file-for-visit)))

    (defun my-try-open-pdf-file (filename)
      "If file at FILENAME is a pdf file, then open as pdf, other return nil."
      (when (my-pdf-file filename)
        (org-link-open-from-string filename)
        t))

    (defun my-pdf-file (filename)
      "Return t, when FILENAME is path to a PDF file."
      (s-suffix-p ".pdf" filename)))

  (leaf dired-ranger
    :ensure t
    :bind (:dired-mode-map
           :package dired
           ("m" . 'dired-ranger-move)
           ("v" . 'dired-ranger-paste)
           ("c" . 'dired-ranger-copy)))

  (leaf dired-subtree
    :ensure t
    :defun dired-subtree-beginning
    :bind (:dired-mode-map
           :package dired
           ("TAB" . 'dired-subtree-cycle)
           ("/"   . 'my-dired-subtree-in-special-buffer))
    :config                           ;nofmt
    (defun my-dired-subtree-in-special-buffer ()
      "Open current `dired-subtree' in the separate `dired' buffer."
      (interactive)
      (my-dired-save-excursion
       (dired-subtree-beginning)
       (forward-line -1)
       (dired (thing-at-point 'filename)))))

  (leaf dired-collapse
    :ensure t
    :hook (dired-mode-hook . dired-collapse-mode))

  (defcustom my-dired-commands-using-minibuffer
    '(dired-filter-by-file
      dired-filter-by-extension
      my-dired-new-file
      dired-byte-compile
      dired-do-delete
      dired-create-directory
      dired-isearch-filenames)
    "List of the `dired' commands using the minubuffer."
    :type '(repeat symbol)
    :group 'my)

  ;; Command for printing file
  (with-eval-after-load 'lpr (setq lpr-command "PDFToPrinter"))

  (remove-hook 'dired-mode-hook 'dired-mode))



;;; my-dired.el ends here
;;; my-dwim-shell-command.el --- My configuration of `dwim-shell-command' -*- lexical-binding: t; -*-

;; Copyright (C) 2022 semenInRussia

;; Author: semenInRussia <hrams205@gmail.com>
;; Version: 0.1
;; Homepage: https://github.com/semeninrussia/emacs.el

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

;; My configuration of `dwim-shell-command'.

;;; Code:




(leaf dwim-shell-command
  :ensure t
  :bind ("M-!" . dwim-shell-command))



;;; my-dwim-shell-command.el ends here
;;; my-git.el --- My config for the Git: the most popular version control

;; Copyright (C) 2022 Semen Khramtsov

;; Author: Semen Khramtsov <hrams205@gmail.com>
;; Version: 0.1
;; URL: https://github.com/semenInRussia/emacs.el

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>

;;; Commentary:

;; My config for the Git: the most popular version control.

;;; Code:




(require 'fast-exec)

(require 'dash)


(leaf magit
  :ensure t
  :hook (magit-mode-hook . visual-fill)
  :custom ((magit-refresh-status-buffer . nil)
           (magit-disabled-section-inserters
            . '(magit-insert-push-branch-header
                magit-insert-tags-header
                magit-insert-unpushed-to-upstream-or-recent
                magit-insert-unpulled-from-upstream))))

(leaf git-timemachine
  :ensure (git-timemachine :host gitlab :repo "pidu/git-timemachine")
  :fast-exec ("Git Timemachine" 'git-timemachine))

(leaf git-modes :ensure t)

(leaf gitignore-templates
  :fast-exec ("Insert Git Ignore" 'gitignore-templates-insert))

(leaf github-clone
  :ensure t
  :custom (github-clone-directory . "~/projects")
  :fast-exec ("Clone a GitHub Project" 'github-clone))

(leaf line-reminder
  :ensure t
  :hook prog-mode-hook
  :custom ((line-reminder-bitmap . 'filled-rectangle)
           (line-reminder-show-option . 'indicators)))



;;; my-git.el ends here
;;; my-hl-todo.el --- My config source code for highlight todo commentaries

;; Copyright (C) 2022 Semen Khramtsov

;; Author: Semen Khramtsov <hrams205@gmail.com>
;; Version: 0.1
;; URL: https://github.com/semenInRussia/emacs.el

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; My config source code for highlight todo commentaries

;;; Code:



(leaf hl-todo                           ;nofmt
  :ensure t
  :global-minor-mode global-hl-todo-mode)



;;; my-hl-todo.el ends here
;;; my-olimipium.el --- My configuration of `olimipium' -*- lexical-binding: t; -*-

;; Copyright (C) 2022 semenInRussia

;; Author: semenInRussia <hrams205@gmail.com>
;; Version: 0.1
;; Homepage: https://github.com/semeninrussia/emacs.el

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

;; My configuration of `olimipium'.

;;; Code:




(declare-function fast-exec-make-command "fast-exec")

(require 'dash)
(require 'f)

(defcustom my-olimpium-dir "~/olimpium"
  "Directory in which should be located solutions of the olimpium tasks."
  :type 'string
  :group 'my)

(defun my-olimpium-new-solution ()
  "New solution of the olimpium task."
  (interactive)
  (let ((default-directory my-olimpium-dir))
    (->>                                  ;nofmt
     my-olimpium-dir
     (f-directories)
     ;; return dir with maximum number in the name, because dirs sorted
     ;; alphabetically
     (--max-by
      (>
       (string-to-number (f-base it))
       (string-to-number (f-base other))))
     (f-files)
     (cons "0.py")                        ;if directory is empty
     ;; return filename with maximum number in the name, analogy with previos
     (--max-by
      (>
       (string-to-number (f-base it))
       (string-to-number (f-base other))))
     (my-inc-filename)
     (find-file))))

(defvar fast-exec--commands-bindings (make-hash-table :test 'eq))

(with-eval-after-load 'fast-exec
  (require 'fast-exec)
  (puthash 'olimpium
           (list
            (fast-exec-make-command "New Olimpium Task"
                                    'my-olimpium-new-solution))
           fast-exec--commands-bindings))



;;; my-olimipium.el ends here
;;; my-organization.el --- My configuration for the my organization

;; Copyright (C) 2022 Semen Khramtsov

;; Author: Semen Khramtsov <hrams205@gmail.com>
;; Version: 0.1
;; URL: https://github.com/semenInRussia/emacs.el

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; My configuration for the my organization

;;; Code:



(require 'just)
(require 'dash)



(declare-function org-schedule "org.el")
(declare-function org-mark-subtree "org.el")
(declare-function my-open-main-agenda-file "my-organization-commands.el")

(declare-function my-goto-targets-on-day "my-organization.el")
(declare-function my-delete-and-get-text-of-org-subtree "my-organization.el")

(leaf org-agenda                        ;nofmt
  :custom ((org-agenda-files .
                             '("~/agenda.org"
                               "~/tasks-archive/task-archive.org"))
           (org-agenda-span . 14))
  :bind ("C-c aa"      . org-agenda))

(require 'fast-exec)

(leaf org-agenda
  :after org
  :fast-exec ("Plane New Day" 'my-agenda-plan-new-day)
  :config                               ;nofmt

  (defun my-goto-targets-on-day ()
    "Visit `org-mode' subtree of the targets on day."
    (my-open-main-agenda-file)
    (goto-char (point-min))
    (search-forward "* Targets on Day")
    (forward-char))

  (defun my-delete-and-get-text-of-org-subtree (&optional pt)
    "Parse a `org-mode' subtree at the PT, delete it and return text of subtree."
    (or pt (setq pt (point)))
    (org-mark-subtree)
    (prog1
        (just-text-in-region)
      (delete-region (region-beginning) (region-end))))

  (defun my-add-org-subtree-to-targets-on-day ()
    "Add an `org-mode' subtree at the point to the targets on day."
    (interactive)
    (save-excursion
      (let ((subtree-text (my-delete-and-get-text-of-org-subtree)))
        (my-goto-targets-on-day)
        (newline)
        (insert subtree-text)
        (delete-char -1)
        (org-schedule t (format-time-string "%Y-%m-%d"))))))

(leaf-keys
 ("C-c at" . my-add-org-subtree-to-targets-on-day))

(leaf org-capture
  :commands org-capture
  :custom ((org-capture-templates
            .
            '(("d"
               "Target on Day"
               entry
               (file+headline "~/agenda.org" "Targets on Day")
               "* TODO %?\n  SCHEDULED: %t\n  \n")
              ("w"
               "Target on Week"
               entry
               (file+headline "~/agenda.org" "Targets on Week")
               "* TODO %?\n  \n")
              ("f"
               "Film for See"
               entry
               (file+headline "~/agenda.org" "Films")
               (function my-films-format-as-org-heading)))))
  :bind (:org-capture-mode-map
         ([remap save-buffer] . org-capture-finalize)))



;;; my-organization.el ends here
;;; my-project.el --- My configration for project.el

;; Copyright (C) 2022-2023 Semen Khramtsov

;; Author: Semen Khramtsov <hrams205@gmail.com>
;; Version: 0.1
;; URL: https://github.com/semenInRussia/emacs.el

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; My configration for project.el that built-in Emacs by default.

;;; Code:





(leaf project
  :ensure t
  :bind-keymap ("C-c p" . project-prefix-map))



;;; my-project.el ends here
;;; my-run-command.el --- My configuration for `run-command'

;; Copyright (C) 2022 Semen Khramtsov

;; Author: Semen Khramtsov <hrams205@gmail.com>
;; Version: 0.1
;; URL: https://github.com/semenInRussia/emacs.el

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; My configuration for `run-command'

;;; Code:





(leaf run-command
  :ensure (run-command
           :type git
           :host github
           :repo "bard/emacs-run-command"
           :branch "develop")
  :defun (run-command-runner-compile
          run-command-run)
  :custom (run-command-default-runner . #'run-command-runner-compile)
  :bind ("<f5>" . run-command)
  :config
  (advice-add 'run-command-run :before #'my-run-command--run--set-last-recipe)

  (leaf run-command-recipes
    :load-path "~/projects/emacs-run-command-recipes"
    :require t
    :defun run-command-recipes-use-all
    :config (run-command-recipes-use-all)))

(global-set-key (kbd "S-<f5>")  #'my-run-last-command)

(defvar run-command-last-recipe nil
  "Last runned recipe of `run-command'.")

(defun my-run-command--run--set-last-recipe (recipe)
  "Set `run-command-last-recipe' to a given RECIPE."
  (setq-local run-command-last-recipe recipe))

(defun my-run-last-command ()
  "Run command which was runned last, if commands wasn't run do nothing."
  (interactive)
  (if run-command-last-recipe
      (run-command-run run-command-last-recipe)
    (message "NOT FOUND!")))




;;; my-run-command.el ends here
;;; my-zms.el --- My configuration for management of the `zms' tasks

;; Copyright (C) 2022 Semen Khramtsov

;; Author: Semen Khramtsov <hrams205@gmail.com>
;; Version: 0.1
;; URL: https://github.com/semenInRussia/emacs.el

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; My configuration for management of the `zms' tasks

;;; Code:

(require 'dash)
(require 's)
(require 'f)



(require 'fast-exec)


(defgroup my-zms nil
  "Group for the management of the `zms' tasks."
  :group 'tool)

(defcustom my-zms-compile-command
  (s-concat
   "pdflatex "
   "-interaction nonstopmode -file-line-error "
   "--output-directory=\"{output-directory}\" "
   "\"{solution.tex}\"")
  "Command which compile Solution.tex file of the ZMS section.

{solution.tex} will be replaced with path to Solution.tex file of the ZMS
section"
  :type 'string
  :group 'my-zms)

(defcustom my-zms-directory "~/zms"
  "Path to the directory in which will be saved files of the ZMS."
  :type 'string
  :group 'my-zms)

(defcustom my-zms-view-solution-latex "\\inputsolution{./solutions/%s.tex}"
  "LaTeX source code which should view solution of the ZMS task."
  :type 'string)

(defcustom my-zms-section-template-directory "~/zms/_template/"
  "Path to the directory which will be temlate for the section of the ZMS."
  :type 'string
  :group 'my-zms)

(defcustom my-zms-section-solutions-relatieve-path "solutions/"
  "Path relatieve with the section subdirectory to the task solutions files."
  :type 'string
  :group 'my-zms)

(require 'eieio)

(defclass my-zms-section ()
  ((name :initarg :name :accessor my-zms-section-name)
   (number :initarg :number :accessor my-zms-section-num))
  "Section of the zms.")

(defun my-zms-new-section (section-name)
  "New section called SECTION-NAME of the ZMS tasks, solutions and other."
  (interactive "sName of the ZMS section, please: ")
  (my-zms-section-save (my-zms-next-section-called section-name)))

(defun my-zms-delete-section (section)
  "Delete SECTION of the ZMS tasks, solutions and other."
  (interactive (list (my-zms-read-section)))
  (-> section (my-zms-section-path) (f-delete t)))

(defun my-zms-section-save (section)
  "Save the ZMS SECTION into the file system."
  (let ((path (my-zms-section-path section))
        (name (my-zms-section-name section))
        (num (number-to-string (my-zms-section-num section))))
    ;; it makes section directory a project
    ;; that can be deteceted with project.el
    (f-mkdir (f-join path ".git"))
    (my-use-skeleton my-zms-section-template-directory
                     path
                     `(("_section-number" . ,num)
                       ("_section-name_" . ,name)))))

(defun my-zms-section-path (section)
  "Return path to the directory of the ZMS section SECTION."
  (->> section (my-zms-section-dirname) (f-join my-zms-directory)))

(defun my-zms-section-dirname (section)
  "Return name of the directory for the ZMS SECTION."
  (format "%s-%s"
          (my-zms-section-num section)
          (my-normalize-string (my-zms-section-name section))))

(defun my-zms-next-section-called (section-name)
  "Return a object of the `my-zms-section' called SECTION-NAME.

Number will be automatically initialized, depends on the previous sections."
  (my-zms-section
   :name section-name
   :number (my-zms-next-section-number)))

(defun my-zms-next-section-number ()
  "Return number of the next ZMS section."
  (--if-let (my-zms-last-section) (1+ (my-zms-section-num it)) 1))

(defun my-zms-last-section ()
  "Return the last section (section with greatest number) of the ZMS sections."
  (-some->> (my-zms-sections) (-max-by (-on '> 'my-zms-section-num))))

(defun my-zms-sections ()
  "Return list of the all ZMS sections."
  (->>
   (my-zms-sections-dirs-names)
   (-map 'my-zms-section-dirname-to-section)))

(defun my-zms-sections-dirs-names ()
  "Return list of the names of the dirictories of the all ZMS sections."
  (->>
   my-zms-directory
   (f-directories)
   (-map 'f-base)
   (--remove (s-prefix-p "_" it))))

(defun my-zms-section-dirname-to-section (section-dirname)
  "Convert ZMS SECTION-DIRNAME to an object of the `my-zms-section'."
  (let* ((dirname-parts
          (s-split " " (my-humanize-string section-dirname)))
         (number (string-to-number (car dirname-parts)))
         (name (s-join " " (cdr dirname-parts))))
    (my-zms-section :name name :number number)))

(defun my-zms-new-solution-in-current-section ()
  "Create solution file for the section in the current active directory."
  (interactive)
  (my-zms-new-solution (my-zms-current-section)))

(defun my-zms-new-solution (section)
  "Create a new LaTeX file into subdir of the ZMS SECTION dir named solutions."
  (interactive (list (my-zms-read-section)))
  (let ((task-number (my-zms-section-next-solution-number section)))
    (my-zms-insert-solution-to-solution.tex section task-number)
    (my-zms-find-solution section task-number)))

(defun my-zms-insert-solution-to-solution.tex (section number)
  "Insert a command viewing solution with NUMBER to Solution.tex of SECTION."
  (find-file (my-zms-section-solution.tex-path section))
  (goto-char (point-max))
  (search-backward "\\end{document}")
  (newline)
  (forward-char -1)
  (insert (format my-zms-view-solution-latex number)))

(defun my-zms-find-solution (section number)
  "Find/visit file of the ZMS SECTION solution with NUMBER file."
  (interactive
   (list
    (my-zms-read-section)
    (read-number "Number of the solution, please:")))
  (find-file (my-zms-section-solution-number-to-path section number)))

(defun my-zms-delete-solution (section number)
  "Find/visit file of the ZMS SECTION solution with NUMBER file."
  (interactive
   (list
    (my-zms-read-section)
    (read-number "Number of the solution, please:")))
  (delete-file
   (my-zms-section-solution-number-to-path section number)))

(defun my-zms-read-section ()
  "Read ZMS SECTION from the user."
  (completing-read
   "Choose a ZMS section:"
   (my--zms-read-section-candidates)))

(defun my--zms-read-section-candidates ()
  "Candidates for the `my-zms-read-section' function."
  (->>
   (my-zms-sections)
   (--map (cons (my-zms-format-section it) it))))

(defun my-zms-format-section (section)
  "Format SECTION of the ZMS to a string."
  (format "%s. %s"
          (my-zms-section-num section)
          (my-zms-section-name section)))

(defun my-zms-section-next-solution-number (section)
  "Return number of the next task solution of the ZMS SECTION."
  (--if-let (my-zms-section-last-solution-number section) (1+ it) 1))

(defun my-zms-section-solution-number-to-path (section number)
  "Return path to the task solution of the ZMS SECTION with NUMBER."
  (f-join
   (my-zms-section-solutions-path section)
   (format "%s.tex" number)))

(defun my-zms-section-last-solution-number (section)
  "Return number of the last task solution of the SECTION."
  (->>
   section
   (my-zms-section-solutions-path)
   (f-files)
   (--map (string-to-number (f-base it)))
   (my-max)))

(defun my-zms-section-solutions-path (section)
  "Return path to the subdirectory with solutions of the SECTION subdir."
  (f-join
   (my-zms-section-path section)
   my-zms-section-solutions-relatieve-path))

(defun my-zms-section-solution.tex-path (section)
  "Return path to the Solution.tex file of the ZMS SECTION."
  (f-join (my-zms-section-path section) "Solution.tex"))

(defun my-zms-current-section ()
  "Get either ZMS session placed in the current directory or last created."
  (or
   (my-zms-path-to-section default-directory)
   (my-zms-last-section)))

(defun my-zms-path-to-section (path)
  "Convert PATH to a file of the ZMS SECTION to an object of `my-zms-section'."
  (and
   (my-zms-path-p path)
   (->>
    (f-full path)
    (s-chop-prefix (f-full my-zms-directory))
    (s-split "/")
    (car)
    (my-zms-section-dirname-to-section))))

(defun my-zms-path-p (&optional path)
  "Return non-nil when PATH is the part os the ZMS tasks."
  (or path (setq path (buffer-file-name)))
  (and
   path
   (s-starts-with-p (f-full my-zms-directory) (f-full path))))

(defun my-zms-download-tasks (section url)
  "Download a pdf at URL file as tasks for a ZMS SECTION.

Save it at filename Tasks.pdf"
  (interactive (list (my-zms-read-section) (my-read-url)))
  (url-copy-file url (my-zms-section-tasks-path section) t))

(defun my-zms-download-theory (section url)
  "Download a pdf at URL file as theory for a ZMS SECTION.

Save it at filename Theory.pdf"
  (interactive (list (my-zms-read-section) (my-read-url)))
  (url-copy-file url (my-zms-section-theory-path section) t))

(defun my-zms-section-theory-path (section)
  "Return path to a pdf theory file for a ZMS SECTION."
  (f-join (my-zms-section-path section) "Theory.pdf"))

(defun my-zms-section-tasks-path (section)
  "Return path to a pdf tasks file for a ZMS SECTION."
  (f-join (my-zms-section-path section) "Tasks.pdf"))

(eval-after-load 'fast-exec
  '(progn
     (require 'fast-exec)
     (fast-exec-bind
      'zms
      (fast-exec-make-some-commands
       ("New ZMS Task Solution"     'my-zms-new-solution)
       ("Forward ZMS Task Solution" 'my-zms-new-solution-in-current-section)
       ("New ZMS Section"           'my-zms-new-section)
       ("Delete ZMS Section"        'my-zms-delete-section)
       ("Download ZMS Answers File" 'my-zms-download-tasks)
       ("Download ZMS Theory File"  'my-zms-download-theory)
       ("Delete ZMS Task Solution"  'my-zms-delete-solution)))))

(defun my-zms-run-command-recipe ()
  "Recipe of `run-command' for ZMS."
  (when (my-zms-path-p)
    (list
     (list
      :command-name "zms-compile-section"
      :display "Compile Section.tex file of the ZMS section via `pdflatex'"
      :command-line (my-zms--get-compile-command)
      :working-dir (my-zms-section-path (my-zms-current-section))))))

(defun my-zms--get-compile-command ()
  "Get command for compiling of the section file Solution.tex.

See `my-zms-compile-command'"
  (->>
   my-zms-compile-command
   (s-replace "{solution.tex}"
              (my-zms-section-solution.tex-path
               (my-zms-current-section)))
   (s-replace "{output-directory}"
              (my-zms-section-output-path
               (my-zms-current-section)))))

(defun my-zms-section-output-path (section)
  "Get path to the output directory of compiling Solution.tex file of SECTION."
  (-> section (my-zms-section-path) (f-join "destination")))

(eval-after-load 'run-command
  '(progn
     (defvar run-command-recipes)
     (add-to-list 'run-command-recipes 'my-zms-run-command-recipe)))



;;; my-zms.el ends here
;;; my-doom-themes.el --- My file loading `doom-themes'

;; Copyright (C) 2022 Semen Khramtsov

;; Author: Semen Khramtsov <hrams205@gmail.com>
;; Version: 0.1
;; URL: https://github.com/semenInRussia/emacs.el

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; My file loading `doom-themes'

;;; Code:



(leaf doom-themes :ensure t)



;;; my-doom-themes.el ends here
;;; my-gruber-darker-theme.el --- My file loading `gruber-darker-theme'

;; Copyright (C) 2022 Semen Khramtsov

;; Author: Semen Khramtsov <hrams205@gmail.com>
;; Version: 0.1
;; URL: https://github.com/semenInRussia/emacs.el

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; My file loading `gruber-darker-theme'

;;; Code:



(leaf gruber-darker-theme :ensure t :require t)



;;; my-gruber-darker-theme.el ends here
;;; my-load-theme --- Load the current theme

;; Copyright (C) 2022 Semen Khramtsov

;; Author: Semen Khramtsov <hrams205@gmail.com>
;; Version: 0.1
;; URL: https://github.com/semenInRussia/emacs.el

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:
;; See `doom-themes' (list of themes at starting comments)

;; List of my favorite themes:
;; - `gruber-darker'
;; - `doom-monokai-classic'
;; - `solarized'
;; - `flatland-theme'

(setq font-lock-maximum-decoration t
      truncate-partial-width-windows nil)

(require 'doom-themes-autoloads)
(load-theme 'doom-1337 t)

(custom-set-faces
 `(region
   ((t (:background "white" :foreground "black" :inherit t)))))



;;; my-load-theme.el ends here
;;; my-monokai.el --- Install the `monokai-theme' (clone of the sublime theme) ' -*- lexical-binding: t; -*-

;; Copyright (C) 2022 semenInRussia

;; Author: semenInRussia <hrams205@gmail.com>
;; Version: 0.1
;; Homepage: https://github.com/semeninrussia/emacs.el

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

;; My configuration of `monokai'.

;;; Code:



(leaf monokai-theme :ensure t)



;;; my-monokai.el ends here
;;; my-default.el --- My some default config

;; Copyright (C) 2022 Semen Khramtsov

;; Author: Semen Khramtsov <hrams205@gmail.com>
;; Version: 0.1
;; URL: https://github.com/semenInRussia/emacs.el

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; My some default config

;;; Code:
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(add-to-list 'default-frame-alist '(fullscreen . maximized))
(add-hook 'emacs-startup-hook 'toggle-frame-fullscreen)



;;; my-default.el ends here
;;; my-focus.el --- My configuration of `focus' -*- lexical-binding: t; -*-

;; Copyright (C) 2022 semenInRussia

;; Author: semenInRussia <hrams205@gmail.com>
;; Version: 0.1
;; Homepage: https://github.com/semeninrussia/emacs.el

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

;; My configuration of `focus'.

;;; Code:



(leaf focus
  :ensure t
  :custom-face (focus-unfocused . '((t :inherit shadow))))



;;; my-focus.el ends here
;;; my-fonts.el --- My configuration for fonts

;; Copyright (C) 2022, 2023 Semen Khramtsov

;; Author: Semen Khramtsov <hrams205@gmail.com>
;; Version: 0.1
;; URL: https://github.com/semenInRussia/emacs.el

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; My configuration for fonts

;;; Code:


(require 'dash)

;; you can install this font, usin `nerd-fonts' (search in the `github')
(defcustom my-fonts-main
  "Hack Nerd Font Mono"
  "Name of the main font to display all."
  :group 'my
  :type 'string)

(set-face-attribute 'default nil :font my-fonts-main :height 210)

;; (leaf unicode-fonts
;;   :ensure t
;;   :defun (unicode-fonts-setup unicode-fonts--instructions)
;;   :defvar (unicode-fonts
;;            unicode-fonts--instructions
;;            ...)
;;   :require t
;;   :init (defvar ... nil)
;;   :config (setq unicode-fonts--instructions
;;                 (-remove-item '... unicode-fonts--instructions))
;;   (unicode-fonts-setup))

(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-language-environment 'utf-8)
(set-selection-coding-system 'utf-8)



;;; my-fonts.el ends here
;;; my-modeline.el --- My configuration for modeline

;; Copyright (C) 2022 Semen Khramtsov

;; Author: Semen Khramtsov <hrams205@gmail.com>
;; Version: 0.1
;; URL: https://github.com/semenInRussia/emacs.el

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; My configuration for `modeline'

;;; Code:

(defvar flycheck-mode-line)



(require 'dash)


(leaf moody
  :ensure t
  :require t
  :custom (flycheck-mode-line-prefix . "")
  :defun (moody-replace-vc-mode
          moody-replace-mode-line-buffer-identification)
  :config
  (setq x-underline-at-descent-line t)
  (setq-default mode-line-format
                '(" " (:eval (meow-indicator))
                  mode-line-modified
                  mode-line-client
                  mode-line-frame-identification
                  mode-line-buffer-identification
                  " " (:eval (my-modeline-pomidor))
                  " " (:eval (my-display-time-string))
                  " " (vc-mode vc-mode)
                  (multiple-cursors-mode mc/mode-line)
                  (lsp-bridge-mode mc/mode-line)
                  flycheck-mode-line
                  mode-line-end-spaces))
  (moody-replace-mode-line-buffer-identification)
  (moody-replace-vc-mode))


(declare-function pomidor--overwork-duration "pomidor.el")
(declare-function pomidor--work-duration "pomidor.el")
(declare-function pomidor-overwork-p "pomidor.el")
(declare-function pomidor--break-duration "pomidor.el")
(declare-function pomidor--current-state "pomidor.el")


(defcustom my-modeline-time-segment-format-string " [%H-%M]"
  "By this format string will draw time in modeline.

See `format-time-string' for see what format string"
  :type 'string
  :group 'my)

(defface my-modeline-time-morning-face
  '((t (:foreground "#ff4500" :weight bold)))
  "Face for view of the time in modeline in the morning."
  :group 'my)

(defface my-modeline-time-evening-face
  '((t (:foreground "#dcdcdc" :weight bold)))
  "Face for view of the time in modeline in the evening."
  :group 'my)

(defun my-display-time-string ()
  "Return the string that tell current time in the modeline."
  (let* ((hour (string-to-number (format-time-string "%H"))))
    (propertize
     (format-time-string my-modeline-time-segment-format-string)
     'face
     (if (< 4 hour 19)
         'my-modeline-time-morning-face 'my-modeline-time-evening-face))))

(defun my-modeline-pomidor ()
  "Return format string for `pomidor', view remainders minuts for break/work."
  (and
   (featurep 'pomidor)
   (--some (equal (buffer-name it) "*pomidor*") (buffer-list))
   (my-pomidor-format-remaining-time)))

(defun my-pomidor-kind ()
  "Return kind of curent `pomidor' state, either break, work or overwork."
  (cond
   ((plist-get (pomidor--current-state) :break)
    'break)
   ((pomidor-overwork-p)
    'overwork)
   ((plist-get (pomidor--current-state) :started)
    'work)))

(defface my-modeline-pomidor-break-face
  '((t :foreground "#ff4500" :underline t :weight bold))
  "Face showing in the mode line at time when `pomidor' has status break."
  :group 'my)

(defface my-modeline-pomidor-overwork-face
  '((t :foreground "#Ffa500" :underline t :weight bold))
  "Face showing in the mode line at time when `pomidor' has status overwork."
  :group 'my)

(defface my-modeline-pomidor-work-face
  '((t :foreground "#7cfc00" :underline t :weight bold))
  "Face showing in the mode line at time when `pomidor' has work status."
  :group 'my)

(defun my-pomidor-face ()
  "Return face for the current status of the current `pomidor' state."
  (cl-case
      (my-pomidor-kind)
    ((break)
     'my-modeline-pomidor-break-face)
    ((work)
     'my-modeline-pomidor-work-face)
    ((overwork)
     'my-modeline-pomidor-overwork-face)))

(defun my-pomidor-remaining-time ()
  "Return remaining time to the end of the pomidor work or break period.

Format of time is the list form the hours, minutes, seconds and zero?"
  (cl-case
      (my-pomidor-kind)
    ((work)
     (pomidor--work-duration (pomidor--current-state)))
    ((overwork)
     (pomidor--overwork-duration (pomidor--current-state)))
    ((break)
     (pomidor--break-duration (pomidor--current-state)))))

(defcustom my-pomidor-modeline-time-format "%M min"
  "String defining format of string viewing pomodoro time at the modeline."
  :group 'my
  :type 'string)

(defun my-pomidor-format-remaining-time ()
  "Format remaining time to the end of the pomidor work or break period."
  (propertize
   (format-time-string my-pomidor-modeline-time-format
                       (my-pomidor-remaining-time))
   'face
   (my-pomidor-face)))



;;; my-modeline.el ends here
;;; my-page-break-lines.el --- My config for `page-break-lines'

;; Copyright (C) 2022 Semen Khramtsov

;; Author: Semen Khramtsov <hrams205@gmail.com>
;; Version: 0.1
;; URL: https://github.com/semenInRussia/emacs.el

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; My config for `page-break-lines'

;;; Code:



(leaf page-break-lines
  :ensure t
  :global-minor-mode global-page-break-lines-mode)



;;; my-page-break-lines.el ends here
;;; my-prettify-mode.el --- My config for `prettify-mode'

;; Copyright (C) 2022 Semen Khramtsov

;; Author: Semen Khramtsov <hrams205@gmail.com>
;; Version: 0.1
;; URL: https://github.com/semenInRussia/emacs.el

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; My config for `prettify-mode'

;;; Code:



(leaf prog-mode :hook (LaTeX-mode-hook . prettify-symbols-mode))



;;; my-prettify-mode.el ends here
;;; my-annotate.el --- My configuration of `annotate' -*- lexical-binding: t; -*-

;; Copyright (C) 2022 semenInRussia
;; Author: semenInRussia <hrams205@gmail.com>
;; Version: 0.1
;; Homepage: https://github.com/semeninrussia/emacs.el

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

;; My configuration of `annotate'.

;;; Code:



(leaf annotate
  :ensure t
  :hook org-mode-hook
  :custom ((annotate-use-echo-area . t)
           (annotate-print-annotation-under-cursor . t)
           (annotate-print-annotation-under-cursor-prefix . "[ann] "))
  :bind (:org-mode-map
         :package org
         ("C-c M-a" . 'annotate-annotate)
         ("C-c C-u M-a" . 'annotate-delete-annotation)))



;;; my-annotate.el ends here
;;; my-auto-compile.el --- My configuration for auto compile lisp files of config -*- lexical-binding: t; -*-

;; Copyright (C) 2022 semenInRussia
;; Author: semenInRussia <hrams205@gmail.com>
;; Version: 0.1
;; Homepage: https://github.com/semeninrussia/emacs.el

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

;; My configuration of `auto-compile'.

;;; Code:

(require 'f)

(declare-function async-byte-compile-file "async-bytecomp.el")


(defun my-auto-recompile-current-elisp-file ()
  "If the opened buffer is an Elisp file and it was be compiled, recompile it."
  (and
   (buffer-file-name)
   (eq major-mode 'emacs-lisp-mode)
   (f-exists-p (f-swap-ext (buffer-file-name) "elc"))
   (async-byte-compile-file (buffer-file-name))))

(define-minor-mode my-auto-recompile-current-elisp-file-mode
  "After each save of an Elisp file that was be compiled, recompile it.

If the ARG is non-nil, then enable the mode, otherwise disable it."
  :init-value t
  (if my-auto-recompile-current-elisp-file-mode
      (add-hook 'after-save-hook 'my-auto-recompile-current-elisp-file t)
    (remove-hook 'after-save-hook 'my-auto-recompile-current-elisp-file t)))

(define-global-minor-mode my-global-auto-recompile-current-elisp-file-mode
  my-auto-recompile-current-elisp-file-mode
  (lambda () (my-auto-recompile-current-elisp-file-mode t)))

(my-global-auto-recompile-current-elisp-file-mode t)



;;; my-auto-compile.el ends here
;;; my-autoinsert.el --- My configuration of `autoinsert': automatically insert any initial text into empty files -*- lexical-binding: t; -*-
;; Copyright (C) 2023 semenInRussia
;; Author: semenInRussia <hrams205@gmail.com>
;; Version: 0.1
;; Homepage: https://github.com/semeninrussia/emacs.el
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
;; My configuration of `autoinsert'.
;;; Code:


(leaf autoinsert
  :ensure t
  :custom ((auto-insert-alist .
                              '((c++-mode .
                                          (nil
                                           "// Copyright "
                                           (my-current-year)
                                           " semenInRussia"
                                           _)))))
  :global-minor-mode auto-insert-mode)


;;; my-autoinsert.el ends here
;;; my-calendar.el --- My configuration of `calendar' -*- lexical-binding: t; -*-

;; Copyright (C) 2022 semenInRussia
;; Author: semenInRussia <hrams205@gmail.com>
;; Version: 0.1
;; Homepage: https://github.com/semeninrussia/emacs.el

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

;; My configuration of `calendar'.

;;; Code:




(leaf calendar
  :ensure t
  :defvar calendar-holidays
  :bind ("C-c C" . calendar)
  :config                               ;nofmt
  (leaf russian-holidays
    :ensure t
    :require t
    :defvar russian-holidays
    :config (setq calendar-holidays russian-holidays)))



;;; my-calendar.el ends here
;;; my-command-log-mode.el --- My config for `command-log-mode'
;; Copyright (C) 2022 Semen Khramtsov
;; Author: Semen Khramtsov <hrams205@gmail.com>
;; Version: 0.1
;; URL: https://github.com/semenInRussia/emacs.el
;; This file is not part of GNU Emacs.
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;; Commentary:
;; My config for `command-log-mode'
;;; Code:


(leaf command-log-mode :ensure t)


;;; my-command-log-mode.el ends here
;;; my-consult.el --- My config for `consult'

;; Copyright (C) 2022, 2023 Semen Khramtsov
;; Author: Semen Khramtsov <hrams205@gmail.com>
;; Version: 0.1
;; URL: https://github.com/semenInRussia/emacs.el

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; My config for `consult'

;;; Code:




(require 'dash)


(leaf vertico
  :ensure (vertico :host github
                   :repo "minad/vertico"
                   :files ("*.el" "extensions/*.el"))
  :global-minor-mode vertico-mode
  :init
  (leaf vertico-directory
    :bind (:vertico-map
           :package vertico
           ("RET" . vertico-directory-enter)
           ("DEL" . vertico-directory-delete-char)
           ("M-DEL" . vertico-directory-delete-word))))

(leaf consult
  :ensure t
  :defun ((consult-register-format
           consult-register-window
           consult-xref
           ;; for my consult-ripgrep
           consult--grep
           consult--ripgrep-make-builder)
          ((project-current project-root) . project))
  :defvar (consult-narrow-key consult-project-function)
  :bind (:minibuffer-local-map
         ("M-s" . consult-history) ;; orig. next-matching-history-element
         ("M-r" . consult-history))
  :bind (("C-x C-b" . consult-buffer)
         ("C-c i" . consult-imenu)
         ("C-c n" . consult-imenu-multi))
  :bind (;; C-c bindings in `mode-specific-map'
         ("C-c M-x" . consult-mode-command)
         ("C-c s" . consult-ripgrep)
         ("C-c k" . consult-kmacro)
         ("C-c m" . consult-man)
         ([remap Info-search] . consult-info)
         ("M-#" . consult-register-load)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ;; M-g bindings in `goto-map'
         ("M-g e" . consult-compile-error)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings in `search-map'
         ("M-s d" . consult-find)
         ("M-s D" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         ;; Minibuffer history
         )
  :custom ((register-preview-delay  . 0.5)
           (register-preview-function . #'consult-register-format))
  :hook ((completion-list-mode-hook . consult-preview-at-point-mode))

  ;; The :init configuration is always executed (Not lazy)
  :init

  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Use Consult to select xref locations with preview
  (leaf xref
    :defvar (xref-show-xrefs-function
             xref-show-definitions-function)
    :custom ((xref-show-xrefs-function . #'consult-xref)
             (xref-show-definitions-function . #'consult-xref)))

  (leaf orderless
    :ensure t
    :require t
    :custom (completion-styles . '(orderless)))

  (leaf marginalia
    :ensure t
    :global-minor-mode marginalia-mode)

  (leaf embark-consult :ensure t)

  (leaf embark
    :ensure t
    :bind (:minibuffer-mode-map
           :package vertico
           ("M-." . embark-act)
           ([remap write-file] . embark-export)
           ([remap save-buffer] . embark-collect)))

  (defvar string-width 0)

  (defun compat-call (&rest b)
    0))



;;; my-consult.el ends here
;;; my-cowsay.el --- My config of `cowsay'

;; Copyright (C) 2022 Semen Khramtsov
;; Author: Semen Khramtsov <hrams205@gmail.com>
;; Version: 0.1
;; URL: https://github.com/semenInRussia/emacs.el

;; This file is not part of GNU Emacs.
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; My config of `cowsay'

;;; Code:



(require 'fast-exec)
(require 'dash)

(leaf cowsay
  :ensure t
  :defun cowsay--get-default-cow
  :defvar cowsay-cows
  :custom (cowsay-directories . '("~/.emacs.d/cows"))
  :defer-config (cowsay-load-cows)
  :fast-exec (("Cow Say String..."  'cowsay-string)
              ("Cow Say Region..."  'cowsay-region)
              ("Cow Say and Insert" 'cowsay-replace-region)
              ("Load Cows"  'cowsay-load-cows))
  :config (defun cowsay--prompt-for-cow
              (&rest _ignored)
            "Read any cow name from the minibuffer."
            (let ((default (cowsay--get-default-cow)))
              (completing-read
               "Cow: "
               cowsay-cows
               nil t
               default
               'cowsay-cow-history
               default))))



;;; my-cowsay.el ends here
;;; my-devdocs.el --- My config for `devdocs'

;; Copyright (C) 2022 Semen Khramtsov
;; Author: Semen Khramtsov <hrams205@gmail.com>
;; Version: 0.1
;; URL: https://github.com/semenInRussia/emacs.el

;; This file is not part of GNU Emacs.
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; My config for `devdocs'

;;; Code:


(require 'dash)
(require 'fast-exec)

(leaf devdocs
  :ensure t
  :hook ((python-mode-hook . my-devdocs-python-hook)
         (emacs-lisp-mode-hook . my-devdocs-emacs-lisp-hook)
         (rust-mode-hook . my-devdocs-rust-hook)
         (c++-mode-hook . my-devdocs-c++-hook)
         (LaTeX-mode-hook . my-devdocs-latex-hook)
         (haskell-mode-hook . my-devdocs-haskell-hook))
  :fast-exec (("Install DevDocs Docset" 'devdocs-install)
              ("Delete DevDocs Docset" 'devdocs-delete))
  :bind ("C-c d" . 'devdocs-lookup)
  :config                               ;nofmt

  (defun my-devdocs-python-hook ()
    "Set docsets of `devdocs' for `python-mode'."
    (setq-local devdocs-current-docs '("python~3.11")))

  (defun my-devdocs-latex-hook ()
    "Set docsets of `devdocs' for `latex-mode'."
    (setq-local devdocs-current-docs '("latex")))

  (defun my-devdocs-emacs-lisp-hook ()
    "Set docsets of `devdocs' for `emacs-lisp-mode'."
    (setq-local devdocs-current-docs '("elisp")))

  (defun my-devdocs-rust-hook ()
    "Set docsets of `devdocs' for `rust-mode'."
    (setq-local devdocs-current-docs '("rust")))

  (defun my-devdocs-c++-hook ()
    "Set docsets of `devdocs' for `c++-mode'."
    (setq-local devdocs-current-docs '("gcc~12_cpp" "cpp")))

  (defun my-devdocs-haskell-hook ()
    "Set docsets of `devdocs'for `haskell-mode'."
    (setq-local devdocs-current-docs '("haskell~9"))))



;;; my-devdocs.el ends here
;;; my-eshell.el --- My configuration of `eshell' -*- lexical-binding: t; -*-

;; Copyright (C) 2022 semenInRussia
;; Author: semenInRussia <hrams205@gmail.com>
;; Version: 0.1
;; Homepage: https://github.com/semeninrussia/emacs.el

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

;; My configuration of `eshell'.

;;; Code:


(require 'dash)


(defcustom my-eshell-commands-using-minibuffer
  '(completion-at-point)
  "List of the `eshell' commands using the minubuffer."
  :type '(repeat symbol)
  :group 'my)

(leaf eshell
  :ensure t
  :bind (:eshell-mode-map
         :package esh-mode
         ([remap beginning-of-line] . 'eshell-begin-on-new-line)
         ([remap beginning-of-line-text] . 'eshell-begin-on-new-line))
  :config                               ;nofmt
  (leaf company-shell
    :ensure t
    :require t)

  ;; (--each my-eshell-commands-using-minibuffer
  ;;   (advice-add it :after
  ;;               (lambda (&rest _) (meow-insert))
  ;;               '((name . meow-insert))))
  )



;;; my-eshell.el ends here
;;; my-figlet.el --- My configuration of `figlet' -*- lexical-binding: t; -*-
;; Copyright (C) 2022 semenInRussia
;; Author: semenInRussia <hrams205@gmail.com>
;; Version: 0.1
;; Homepage: https://github.com/semeninrussia/emacs.el
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
;; My configuration of `figlet'.
;;; Code:




(require 'dash)
(leaf figlet
  :ensure t
  :custom ((figlet-default-font . "Star Wars")
           (figlet-fonts .
                         '("1Row"
                           "3-D"
                           "3D Diagonal"
                           "3D-ASCII"
                           "3x5"
                           "4Max"
                           "5 Line Oblique"
                           "Acrobatic"
                           "Alligator"
                           "Alligator2"
                           "Alpha"
                           "Alphabet"
                           "AMC 3 Line"
                           "AMC 3 Liv1"
                           "AMC AAA01"
                           "AMC Neko"
                           "AMC Razor"
                           "AMC Razor2"
                           "AMC Slash"
                           "AMC Slider"
                           "AMC Thin"
                           "AMC Tubes"
                           "AMC Untitled"
                           "ANSI Regular"
                           "ANSI Shadow"
                           "Arrows"
                           "ASCII New Roman"
                           "Avatar"
                           "B1FF"
                           "Banner"
                           "Banner3-D"
                           "Banner3"
                           "Banner4"
                           "Barbwire"
                           "Basic"
                           "Bear"
                           "Bell"
                           "Benjamin"
                           "Big Chief"
                           "Big Money-ne"
                           "Big Money-nw"
                           "Big Money-se"
                           "Big Money-sw"
                           "Big"
                           "Bigfig"
                           "Binary"
                           "Block"
                           "Blocks"
                           "Bloody"
                           "Bolger"
                           "Braced"
                           "Bright"
                           "Broadway KB"
                           "Broadway"
                           "Bubble"
                           "Bulbhead"
                           "Caligraphy"
                           "Caligraphy2"
                           "Calvin S"
                           "Cards"
                           "Catwalk"
                           "Chiseled"
                           "Chunky"
                           "Coinstak"
                           "Cola"
                           "Colossal"
                           "Computer"
                           "Contessa"
                           "Contrast"
                           "Cosmike"
                           "Crawford"
                           "Crawford2"
                           "Crazy"
                           "Cricket"
                           "Cursive"
                           "Cyberlarge"
                           "Cybermedium"
                           "Cybersmall"
                           "Cygnet"
                           "DANC4"
                           "Dancing Font"
                           "Decimal"
                           "Def Leppard"
                           "Delta Corps Priest 1"
                           "Diamond"
                           "Diet Cola"
                           "Digital"
                           "Doh"
                           "Doom"
                           "DOS Rebel"
                           "Dot Matrix"
                           "Double Shorts"
                           "Double"
                           "Dr Pepper"
                           "DWhistled"
                           "Efti Chess"
                           "Efti Font"
                           "Efti Italic"
                           "Efti Piti"
                           "Efti Robot"
                           "Efti Wall"
                           "Efti Water"
                           "Electronic"
                           "Elite"
                           "Epic"
                           "Fender"
                           "Filter"
                           "Fire Font-k"
                           "Fire Font-s"
                           "Flipped"
                           "Flower Power"
                           "Four Tops"
                           "Fraktur"
                           "Fun Face"
                           "Fun Faces"
                           "Fuzzy"
                           "Georgi16"
                           "Georgia11"
                           "Ghost"
                           "Ghoulish"
                           "Glenyn"
                           "Goofy"
                           "Gothic"
                           "Graceful"
                           "Gradient"
                           "Graffiti"
                           "Greek"
                           "Heart Left"
                           "Heart Right"
                           "Henry 3D"
                           "Hex"
                           "Hieroglyphs"
                           "Hollywood"
                           "Horizontal Left"
                           "Horizontal Right"
                           "ICL-1900"
                           "Impossible"
                           "Invita"
                           "Isometric1"
                           "Isometric2"
                           "Isometric3"
                           "Isometric4"
                           "Italic"
                           "Ivrit"
                           "Jacky"
                           "Jazmine"
                           "Jerusalem"
                           "JS Block Letters"
                           "JS Bracket Letters"
                           "JS Capital Curves"
                           "JS Cursive"
                           "JS Stick Letters"
                           "Katakana"
                           "Kban"
                           "Keyboard"
                           "Knob"
                           "Konto Slant"
                           "Konto"
                           "Larry 3D 2"
                           "Larry 3D"
                           "LCD"
                           "Lean"
                           "Letters"
                           "Lil Devil"
                           "Line Blocks"
                           "Linux"
                           "Lockergnome"
                           "Madrid"
                           "Marquee"
                           "Maxfour"
                           "Merlin1"
                           "Merlin2"
                           "Mike"
                           "Mini"
                           "Mirror"
                           "Mnemonic"
                           "Modular"
                           "Morse"
                           "Morse2"
                           "Moscow"
                           "Mshebrew210"
                           "Muzzle"
                           "Nancyj-Fancy"
                           "Nancyj-Improved"
                           "Nancyj-Underlined"
                           "Nancyj"
                           "Nipples"
                           "NScript"
                           "NT Greek"
                           "NV Script"
                           "O8"
                           "Octal"
                           "Ogre"
                           "Old Banner"
                           "OS2"
                           "Pagga"
                           "Patorjk's Cheese"
                           "Patorjk-HeX"
                           "Pawp"
                           "Peaks Slant"
                           "Peaks"
                           "Pebbles"
                           "Pepper"
                           "Poison"
                           "Puffy"
                           "Puzzle"
                           "Pyramid"
                           "Rammstein"
                           "Rectangles"
                           "Red Phoenix"
                           "Relief"
                           "Relief2"
                           "Reverse"
                           "Roman"
                           "Rot13"
                           "Rotated"
                           "Rounded"
                           "Rowan Cap"
                           "Rozzo"
                           "Runic"
                           "Runyc"
                           "S Blood"
                           "Santa Clara"
                           "Script"
                           "Serifcap"
                           "Shadow"
                           "Shimrod"
                           "Short"
                           "SL Script"
                           "Slant Relief"
                           "Slant"
                           "Slide"
                           "Small Caps"
                           "Small Isometric1"
                           "Small Keyboard"
                           "Small Poison"
                           "Small Script"
                           "Small Shadow"
                           "Small Slant"
                           "Small Tengwar"
                           "Small"
                           "Soft"
                           "Speed"
                           "Spliff"
                           "Stacey"
                           "Stampate"
                           "Stampatello"
                           "Standard"
                           "Star Strips"
                           "Star Wars"
                           "Stellar"
                           "Stforek"
                           "Stick Letters"
                           "Stop"
                           "Straight"
                           "Stronger Than All"
                           "Sub-Zero"
                           "Swamp Land"
                           "Swan"
                           "Sweet"
                           "Tanja"
                           "Tengwar"
                           "Term"
                           "Test1"
                           "The Edge"
                           "Thick"
                           "Thin"
                           "THIS"
                           "Thorned"
                           "Three Point"
                           "Ticks Slant"
                           "Ticks"
                           "Tiles"
                           "Tinker-Toy"
                           "Tombstone"
                           "Train"
                           "Trek"
                           "Tsalagi"
                           "Tubular"
                           "Twisted"
                           "Two Point"
                           "Univers"
                           "USA Flag"
                           "Varsity"
                           "Wavy"
                           "Weird"
                           "Wet Letter"
                           "Whimsy"
                           "Wow")))
  :fast-exec (("View a Text" 'figlet)
              ("Insert a Text as Comment" 'figlet-comment)))


;;; my-figlet.el ends here
;;; my-gdz.el --- My configuration of `gdz' -*- lexical-binding: t; -*-
;; Copyright (C) 2022 semenInRussia
;; Author: semenInRussia <hrams205@gmail.com>
;; Version: 0.1
;; Homepage: https://github.com/semeninrussia/emacs.el
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
;; My configuration of `gdz'.
;;; Code:


(require 'dash)
(require 's)
;; ;; (defun my-gdz (lesson)
;; ;;   "Interactively Open the gdz web-page."
;; ;;   (interactive (list )
;;                ))
(defun my-gdz-geo (paragraph)
  "Open the web page for done tasks of PARAGRAPH."
  (interactive "nParagraph of geo, please: ")
  (->>
   paragraph
   (+ (- 7))
   (number-to-string)
   (s-prepend
    "https://resheba.me/gdz/geografija/9-klass/alekseev-bolysov/paragraph-")
   (browse-url)))


;;; my-gdz.el ends here
;;; my-go-translate.el --- My config `go-translate'

;; Copyright (C) 2022 Semen Khramtsov
;; Author: Semen Khramtsov <hrams205@gmail.com>
;; Version: 0.1
;; URL: https://github.com/semenInRussia/emacs.el
;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; My config `go-translate'

;;; Code:



(require 'fast-exec)
(require 'dash)

(leaf go-translate
  :ensure t
  :custom (gts-translate-list . '(("en" "ru")))
  :defvar gts-default-translator
  :defun (gts-buffer-render gts-translator gts-prompt-picker gts-google-engine)
  :fast-exec ("Translate a String" 'gts-do-translate)
  :defer-config (setq gts-default-translator
                      (gts-translator :picker
                                      (gts-prompt-picker)
                                      :engines  ;nofmt
                                      (list (gts-google-engine))
                                      :render (gts-buffer-render))))


;;; my-go-translate.el ends here
;;; my-goto-line-preview.el --- My configuration of `goto-line-preview' -*- lexical-binding: t; -*-
;; Copyright (C) 2022 semenInRussia
;; Author: semenInRussia <hrams205@gmail.com>
;; Version: 0.1
;; Homepage: https://github.com/semeninrussia/emacs.el
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
;; My configuration of `goto-line-preview'.  Preiew line before jump to it
;;; Code:


(leaf goto-line-preview
  :ensure t
  :bind ([remap goto-line] . goto-line-preview))


;;; my-goto-line-preview.el ends here
;;; my-helpful.el --- My config for `helpful'
;; Copyright (C) 2022 Semen Khramtsov
;; Author: Semen Khramtsov <hrams205@gmail.com>
;; Version: 0.1
;; URL: https://github.com/semenInRussia/emacs.el
;; This file is not part of GNU Emacs.
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;; Commentary:
;; My config for `helpful'
;;; Code:


(leaf helpful
  :ensure t
  :bind (("C-h f"   . helpful-callable)
         ("C-h v"   . helpful-variable)
         ("C-h k"   . helpful-key)
         ("C-c C-d" . helpful-at-point)
         ("C-h F"   . helpful-function)
         ("C-h C"   . helpful-command)))


;;; my-helpful.el ends here
;;; my-misc.el --- My some little miscellaneous feautures

;; Copyright (C) 2022 Semen Khramtsov

;; Author: Semen Khramtsov <hrams205@gmail.com>
;; Version: 0.1
;; URL: https://github.com/semenInRussia/emacs.el

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; My some little miscellaneous feautures

;;; Code:


(require 'dash)
(require 'f)

(setq make-backup-files         nil)
(setq auto-save-list-file-name  nil)
(defalias 'yes-or-no-p 'y-or-n-p)
(toggle-truncate-lines t)

;;; russian input method
;;; now I can press `C-\\' and language on which I am now
;; writting will be changed
(setq default-input-method "russian-computer")
(setq default-file-name-coding-system 'utf-8)
(setq default-keyboard-coding-system 'utf-8)
(setq buffer-file-coding-system 'utf-8)

;;; just do PPTX (powerpoint) file that can't be read
(defun my-new-fake-pptx-file ()
  "Make this buffer, fake presentation with format (.pptx)."
  (interactive)
  (->> "~/broken.pptx" (f-read) (insert))
  (text-mode))

(require 'fast-exec)
(fast-exec-bind 'pptx
  (fast-exec-make-some-commands
   ("New Fake PPTX File" 'my-new-fake-pptx-file)))

;;; I try to decrease the Emacs startup time
(defun my-display-startup-time ()
  (message
   "Emacs loaded in %s with %d garbage collections."
   (format
    "%.2f seconds"
    (float-time (time-subtract after-init-time before-init-time)))
   gcs-done))

(add-hook 'emacs-startup-hook #'my-display-startup-time)



;;; my-misc.el ends here
;;; my-open-junk-file.el --- My config to `open-junk-file'

;; Copyright (C) 2022 Semen Khramtsov

;; Author: Semen Khramtsov <hrams205@gmail.com>
;; Version: 0.1
;; URL: https://github.com/semenInRussia/emacs.el

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; My config for open-junk-file

;;; Code:



(leaf open-junk-file :ensure t :bind ("C-t" . 'open-junk-file))



;;; my-open-junk-file.el ends here
;;; my-org-roam.el --- My configuration of `org-roam' -*- lexical-binding: t; -*-

;; Copyright (C) 2022 semenInRussia

;; Author: semenInRussia <hrams205@gmail.com>
;; Version: 0.1
;; Homepage: https://github.com/semeninrussia/emacs.el

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

;; My configuration of `org-roam'.

;;; Code:


(require 'f)

(leaf org-roam
  :ensure t
  :init (f-mkdir "~/org-roam")
  :commands org-roam-ui-mode
  :defun org-roam-db-autosync-mode
  :bind (("C-c z f"   . org-roam-node-find)
         ("C-c z t t" . org-roam-tag-add)
         ("C-c z t d" . org-roam-tag-remove)
         ("C-c z s s" . org-roam-ref-add)
         ("C-c z s d" . org-roam-ref-remove)
         ("C-c z a a" . org-roam-alias-add)
         ("C-c z a d" . org-roam-alias-remove)
         ("C-c z o"   . org-roam-buffer-toggle)
         ("C-c z j"   . org-roam-node-insert))
  :custom (org-roam-mode-sections . '(org-roam-backlinks-section))
  :config                               ;nofmt
  (org-roam-db-autosync-mode t)

  (add-to-list 'Info-directory-list
               (f-full "~/.emacs.d/straight/repos/org-roam/doc"))

  (require 'org-roam-export)
  (require 'org-roam-protocol)

  (leaf org-roam-ui                     ;nofmt
    :ensure t
    :defun org-roam-ui-mode
    :config (org-roam-ui-mode t)))



;;; my-org-roam.el ends here
;;; my-outline.el --- My configuration of `outline' -*- lexical-binding: t; -*-

;; Copyright (C) 2022 semenInRussia

;; Author: semenInRussia <hrams205@gmail.com>
;; Version: 0.1
;; Homepage: https://github.com/semeninrussia/emacs.el

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

;; My configuration of `outline'.

;;; Code:






(leaf outline
  :ensure t
  :bind (:outline-minor-mode-map
         ("S-TAB" . outline-cycle)))



;;; my-outline.el ends here
;;; my-pandoc.el --- My config for the the pandoc

;; Copyright (C) 2022 Semen Khramtsov

;; Author: Semen Khramtsov <hrams205@gmail.com>
;; Version: 0.1
;; URL: https://github.com/semenInRussia/emacs.el

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; My config for the tool pandoc

;;; Code:



(require 'fast-exec)

(require 'dash)
(require 'f)
(require 's)

(defun my-pandoc-tex-to-documents-dir ()
  "Move all .docx files in working dir to directroy documents."
  (f-mkdir "documents")
  (-->
   (file-expand-wildcards "*.tex")
   (-map 'f-base it)
   (--each
       it
     (shell-command
      (s-lex-format
       "pandoc -t docx -f latex -o documents/${it}.docx ${it}.tex")))))

(eval-after-load 'fast-exec
  '(progn
     (fast-exec-bind 'pandoc
       (fast-exec-make-some-commands
        ("Convert Tex Files and Move to Documents Dir"
         'my-pandoc-tex-to-documents-dir)))))



;;; my-pandoc.el ends here
;;; my-pomidor.el --- My configuration for `pomidor': pomodoro techniques

;; Copyright (C) 2022 Semen Khramtsov

;; Author: Semen Khramtsov <hrams205@gmail.com>
;; Version: 0.1
;; URL: https://github.com/semenInRussia/emacs.el

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; My configuration for `pomidor': pomodoro techniques

;;; Code:




(leaf pomidor
  :ensure t
  :hook (emacs-startup-hook . pomidor)
  :bind (("<f12>" . pomidor)
         (:pomidor-mode-map ("Q" . kill-buffer)))
  :custom `((pomidor-sound-tack    . nil)
            (pomidor-seconds       . 1800) ; 30min
            (pomidor-sound-tick    . nil)
            (pomidor-confirm-end-break . nil)))



;;; my-pomidor.el ends here
;;; my-recentf.el --- My configuration of `recentf' -*- lexical-binding: t; -*-

;; Copyright (C) 2022 semenInRussia
;; Author: semenInRussia <hrams205@gmail.com>
;; Version: 0.1
;; Homepage: https://github.com/semeninrussia/emacs.el

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

;; My configuration of `recentf'.

;;; Code:



(leaf recentf
  :ensure t
  :global-minor-mode recentf-mode)



;;; my-recentf.el ends here
;;; my-smartkeys.el --- Some key bindings which depends on context -*- lexical-binding: t; -*-

;; Copyright (C) 2022 semenInRussia

;; Author: semenInRussia <hrams205@gmail.com>
;; Version: 0.0.1
;; Package-Requires: ((dash "2.18.0") (s "1.12.0"))
;; Homepage: https://github.el/semenInRussia/emacs.el

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

;; Some key bindings which depends on context.

;;; Code:



(declare-function sp-kill-whole-line "smartparens.el")
(declare-function sp-splice-sexp "smartparens.el")




(eval-after-load 'smartparens
  '(progn
     (require 'smartparens)
     (defun my-kill-line-or-region ()
       "Call `kill-region' if region is active, otherwise `sp-kill-whole-line'"
       (interactive)
       (if (use-region-p)
           (kill-region (region-beginning) (region-end))
         (sp-kill-whole-line)))

     (defun my-exchange-point-and-mark-or-splice-sexp ()
       "Call `exchange-point-and-mark' if active region else `sp-splice-sexp'."
       (interactive)
       (if (use-region-p) (exchange-point-and-mark) (sp-splice-sexp)))

     (leaf-keys
      ("C-x C-x" . my-exchange-point-and-mark-or-splice-sexp)
      ("C-w" . my-kill-line-or-region))))



;;; my-smartkeys.el ends here
;;; my-speed-type.el --- My configuration of `speed-type' -*- lexical-binding: t; -*-

;; Copyright (C) 2022 semenInRussia

;; Author: semenInRussia <hrams205@gmail.com>
;; Version: 0.1
;; Homepage: https://github.com/semeninrussia/emacs.el

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

;; My configuration of `speed-type'.

;;; Code:


(require 'dash)

(leaf speed-type
  :ensure t
  :fast-exec ("Start a Type Test" 'speed-type-text))



;;; my-speed-type.el ends here
;;; my-supersave.el --- My config for `supersave'

;; Copyright (C) 2022 Semen Khramtsov

;; Author: Semen Khramtsov <hrams205@gmail.com>
;; Version: 0.1
;; URL: https://github.com/semenInRussia/emacs.el

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; My config for `supersave'

;;; Code:



(leaf super-save
  :ensure t
  :defvar super-save-triggers
  :global-minor-mode super-save-mode
  :config (add-to-list 'super-save-triggers 'dired-jump))



;;; my-supersave.el ends here
;;; my-which-key.el --- My config for `which-key'

;; Copyright (C) 2022 Semen Khramtsov

;; Author: Semen Khramtsov <hrams205@gmail.com>
;; Version: 0.1
;; URL: https://github.com/semenInRussia/emacs.el

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; My config for `which-key'

;;; Code:



(leaf which-key
  :ensure t
  :global-minor-mode which-key-mode
  :defun which-key-setup-side-window-bottom
  :config (which-key-setup-side-window-bottom))



;;; my-which-key.el ends here
;;; my-writing-config.el --- My configuration for the writing other configuration -*- lexical-binding: t; -*-

;; Copyright (C) 2022 semenInRussia

;; Author: semenInRussia <hrams205@gmail.com>
;; Version: 0.1
;; Package-Requires: ((dash "2.18.0") (s "1.12.0"))
;; Homepage: https://github.com/semeninrussia/emacs.el

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

;; My configuration for the writing other configuration.

;;; Code:



(require 'fast-exec)

(require 'dash)
(require 's)

(declare-function inspector-inspect "inspector.el")

(defun my-new-config-module (module-name &optional directory)
  "Create a new configuration file named MODULE-NAME in the DIRECTORY.

DIRECTORY defaults to ~/.emacs.d/lisp/"
  (interactive "sName of the configuration module: \nDDirectory: ")
  (setq directory (or directory user-emacs-directory))
  (->>                                  ;nofmt
   module-name
   (s-append ".el")
   (s-prepend "my-")
   (s-prepend directory)
   (find-file))
  (insert
   (s-replace
    "writing-config"
    module-name
    (format
     ";;; my-writing-config.el --- My configuration of `writing-config' -*- lexical-binding: t; -*-

;; Copyright (C) %s semenInRussia

;; Author: semenInRussia <hrams205@gmail.com>
;; Version: 0.1
;; Homepage: https://github.com/semeninrussia/emacs.el

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

;; My configuration of `writing-config'.

;;; Code:

(leaf writing-config)



;;; my-writing-config.el ends here"
     (format-time-string "%Y"))))
  (search-backward "(leaf "))

(fast-exec-bind 'writing-config
  (fast-exec-make-some-commands
   ("New Config Module" 'my-new-config-module)))

(leaf ecukes
  :ensure t
  :bind (:my-feature-local-map :package feature-mode ("e" . ecukes))
  :hook (ecukes-mode-hook . my-feature-mode-hook)
  :config (leaf espuds :ensure t :require t))

(leaf leaf
  :bind ("C-x M-f" . 'leaf-find))

(defun my-bench ()
  "Show bench analysis for Emacs startup."
  (interactive)
  (switch-to-buffer "*Messages*")
  (->>
   (buffer-substring-no-properties (point-min) (point-max))
   (s-lines)
   (--keep
    (-when-let
        ((_ module duration)
         (s-match "‘\\(.*?\\)’ module took \\(.*?\\)sec" it))             ;nofmt
      (cons module (string-to-number duration))))
   (--sort (> (cdr it) (cdr other)))
   (inspector-inspect))
  (rename-buffer "*My Bench*"))

(defun my-do-autoload-for-local-projects-files ()
  "If the opened file is a \"local projects\", make the directory autoloads."
  (interactive)
  (let ((dir (f-full "~/.emacs.d/lisp/local-projects/"))
        (out (f-full "~/.emacs.d/lisp/local-projects/my-autoload.el"))
        flycheck-files)
    (when (string-prefix-p dir (f-full (buffer-file-name)))
      (->>
       dir
       (f-files)
       (--filter (or
                  (s-prefix-p "flycheck_" (f-base it))
                  (s-suffix-p ".elc" it)))
       (setq flycheck-files))
      (loaddefs-generate dir
                         out
                         (cons out flycheck-files)))))

(add-hook 'after-save-hook 'my-do-autoload-for-local-projects-files)




;;; my-writing-config.el ends here
;;; build-config.el --- Join all my config files into one init.el -*- lexical-binding: t; -*-

;; Copyright (C) 2023 semenInRussia

;; Author: semenInRussia <hrams205@gmail.com>
;; Version: 0.0.1

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

;; Join all my config files into one init.el.  I use this script to make one
;; big init.el file which has more fast startup time.

;;; Code:

(require 'cl-lib)

(defvar my-modules-el-file "~/.emacs.d/dist/my-modules.el")
(defvar my-config-modules-prefix "~/.emacs.d/lisp/")

(defvar my-modules-order
  (list
   "package-management/my-straight.el"
   "package-management/my-leaf.el"
   "package-management"
   "my-libs.el"
   "my-lib.el"
   "editing"
   "languages/lisps/my-lisp.el"
   "languages/my-autoformat.el"
   "languages"
   "env"
   "ui")
  "Names of the directories and files that define an order to load.")

(defvar my-modules-files-ignore-regexps
  '("/local-projects/" "/test/" "/features/" ".*-step\\.el" "/site-lisp/")
  "List of the regexps that indicates that a file to load shouldn't be loaded.")

(defun my-build-config ()
  "Build my config."
  (interactive)
  (my-join-modules-into-modules.el)
  (byte-compile-file my-modules-el-file))

(defun my-file-igored-as-module-p (filename)
  "Return non-nil if a module at FILENAME can't be a configuration module."
  (cl-some
   (lambda (regexp) (string-match-p regexp filename))
   my-modules-files-ignore-regexps))

(defmacro my-remove-from (var elt)
  "Remove an ELT from the list at VAR.
The same to
\(setq var (remove elt var))"
  `(setq ,var (remove ,elt ,var)))

(defun my-all-modules-files ()
  "Return list of all modules filenames using `my-modules-order'."
  (interactive "P")
  (let ((order (mapcar
                (lambda (it) (concat my-config-modules-prefix it))
                my-modules-order))
        order-item
        (files (cl-remove-if
                #'my-file-igored-as-module-p
                (directory-files-recursively "~/.emacs.d/lisp" ".el$" nil)))
        f
        sorted)
    (while order
      (setq order-item (car order))
      (setq order (cdr order))
      (cond
       ((file-directory-p order-item)
        (setq sorted
              (append sorted (cl-remove-if
                              (lambda (f) (member f sorted))
                              (directory-files-recursively order-item ".el$")))))
       (t
        (setq sorted (append sorted (list order-item))))))
    (while files
      (setq f (car files))
      (setq files (cdr files))
      (unless (member f sorted)
        (setq sorted (append sorted (list f)))))
    sorted))

(defvar my-config-modules-prefix "~/.emacs.d/lisp/")

(defun my-join-modules-into-modules.el ()
  "Join all configuration modules into one my-modules.el file."
  (my-join-modules my-modules-el-file))

(defun my-join-modules (dest)
  "Join all configuration modules into one file with DEST filename."
  (with-temp-buffer
    (mapc #'insert-file-contents-literally (nreverse (my-all-modules-files)))
    (goto-char (point-max))
    (replace-regexp-in-region "^(provide 'my-[a-zA-Z-]*?)" "\n"
                              (point-min)
                              (point-max))
    (replace-regexp-in-region "^(require 'my-[a-zA-Z-]*?)" "\n"
                              (point-min)
                              (point-max))
    (insert "\n(provide 'my-modules)")
    (delete-file dest)
    (write-region (point-min) (point-max) dest)))

(provide 'build-config)
;;; build-config.el ends here
;;; my-emacsclient.el --- My configuration of `emacsclient' -*- lexical-binding: t; -*-

;; Copyright (C) 2022 semenInRussia

;; Author: semenInRussia <hrams205@gmail.com>
;; Version: 0.1
;; Homepage: https://github.com/semeninrussia/emacs.el

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

;; My configuration of `emacsclient'.

;;; Code:

(add-hook 'emacs-startup-hook 'server-start)



;;; my-emacsclient.el ends here
;;; my-info.el --- Info about me

;; Copyright (C) 2022 Semen Khramtsov

;; Author: Semen Khramtsov <hrams205@gmail.com>
;; Version: 0.1
;; URL: https://github.com/semenInRussia/emacs.el

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Info about me

;;; Code:

(setq user-full-name    "Semen Khramtsov"
      user-mail-address "hrams205@gmail.com")



;;; my-info.el ends here
;;; my-treesit.el --- My configuration of `treesit' -*- lexical-binding: t; -*-

;; Copyright (C) 2022, 2023 semenInRussia

;; Author: semenInRussia <hrams205@gmail.com>
;; Version: 0.1
;; Homepage: https://github.com/semeninrussia/emacs.el

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

;; My configuration of `treesit'.

;;; Code:



(require 'fast-exec)


(require 'dash)

;; (leaf ts-fold
;;   :ensure (ts-fold :type git :host github :repo "emacs-treesit/ts-fold")
;;   :global-minor-mode global-ts-fold-mode
;;   :defun ts-fold-toggle
;;   :bind (:tree-sitter-mode-map
;;          :package treesit
;;          ([remap outline-hide-entry]      . 'ts-fold-close)
;;          ([remap outline-show-entry]      . 'ts-fold-open)
;;          ([remap my-outline-cycle-buffer] . 'ts-fold-close-all)
;;          ([remap my-outline-cycle]        . 'my-ts-fold-toggle))
;;   :config (defun my-ts-fold-toggle
;;               ()
;;             "Like on `ts-fold-toggle', but repeat at last keystrokes."
;;             (interactive)
;;             (ts-fold-toggle)
;;             (repeat-at-last-keystroke)))



;;; my-treesit.el ends here

(provide 'my-modules)