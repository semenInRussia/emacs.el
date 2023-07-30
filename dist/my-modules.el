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

(declare-function straight-use-package "straight.el")


(eval-and-compile
  ;; `eval-and-compile' really install the package in compile time,
  ;; it's important, because `my-leaf' needs in `straight-use-package' to install
  ;; itself and `leaf' needed in the rest config, because `leaf' macro
  (defvar bootstrap-version)
  (setq straight-find-executable "C:\\tools\\find.exe")
  (setq straight-check-for-modifications nil)
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

  (declare-function straight--convert-recipe "straight.el")
  (declare-function straight--add-package-to-load-path "straight.el")
  (declare-function straight--add-package-to-info-path "straight.el")
  (declare-function straight--file "straight.el")
  (declare-function straight--load-package-autoloads "straight.el")
  (declare-function straight--compute-dependencies "straight.el")

  ;; don't build packages, think that they're already installed
  (advice-add 'straight--package-might-be-modified-p :override 'ignore)

  ;; but after init, new packages can be installed, so packages can be
  ;; (re)builded
  (add-hook 'after-init-hook
            (lambda ()
              (advice-remove 'straight--package-might-be-modified-p 'ignore))))



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
                                     (:custom . :pre-setq))))

  (my-leaf-keywords-init))



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

(eval-and-compile
  (add-to-list 'load-path "~/.emacs.d/local-projects")
  (add-to-list 'load-path "~/projects/fast-exec.el")
  (add-to-list 'load-path "~/projects/simple-indention.el")
  (add-to-list 'load-path (locate-user-emacs-file "site-lisp")))



;;; my-projects.el ends here
;;; pam.el --- A fast Emacs package manager which is built over `straight' -*- lexical-binding: t; -*-

;; Copyright (C) 2023 semenInRussia

;; Author: semenInRussia <hrams205@gmail.com>
;; Version: 0.0.1
;; Package-Requires: ((dash "2.18.0") (s "1.12.0") straight)

;;; Commentary:

;; A fast Emacs package manager which is built over `straight'.

;; When use a package instead of `straight-use-package' just add to the load
;; path only ONE directory, load autoloads.  `straight' here add about 190~
;; paths to the load path and load the same amount of autoloads files.  The
;; `pam' version is more simple and lightweight, but the cons is that to install
;; packages (don't activate) it still uses straight, if your packages didn't
;; installed, you should add the --install option when run Emacs in your
;; terminal, it replaces the default behaviour with the default `straight' way.
;; Also if you need change the `pam' behaviour inside of a running Emacs session
;; you can use the `pam-install-everything-mode' minor mode.

;; The main function which you should use in 90% of cases when you use `pam' is
;; `pam-use' it's like `straight-use-package', but don't add any path to the
;; `load-path', don't explore TeXinfo documents, don't check if the package must
;; be byte-compiled.  All these things you can do once when call
;; `pam-load-packages', it have the complexity O(1) (don't depend on the amount
;; of packages) and looks like one call of `straight-use-package'.  Instead
;; `pam-use' just install package if you enabled the respective mode (or used
;; --install flag when call Emacs) or do NOTHING.  In real cases you install the
;; config on other computer, run "emacs --install" and all packages will be
;; installed with `straight' and `pam', so you can use Emacs in other session
;; with just call "emacs" without flags and forgot about `straight' ever.

;; The most the package functions are aliases to their `straight' alternatives,
;; the idea is forgot about the fact that `pam' is built over `straight' and just
;; use pam-<one>, pam-<anotherone> and other package manager functions and don't
;; think where function is define in `straight' or `pam'.  The "real" code in
;; this package placed in little doses.

;;; Code:

;;; autoload `straight' variables and functions.
;;
;; NOTE: here I don't wrote (require \\='straight), because in the most of Emacs
;;   sessions `straight' is extra dependency of `pam', `straight' will be loaded
;;   only when it's really imported.  It saves a little bit of time
(autoload 'straight-use-package "straight")
(autoload 'straight--build-dir "straight")
(autoload 'straight-rebuild-package "straight")
(eval-and-compile
  (defvar straight-use-package-post-build-functions))


(defgroup pam nil
  "My simple package manager which is built over `straight'."
  :group 'emacs)

(defcustom pam-need-to-install-pkgs-p (member "--install" command-line-args)
  "It is a non-nil value if `pam' should install packages."
  :group 'pam
  :type 'hook)

(defcustom pam-build-dir "~/.emacs.d/pam/"
  "Path where all package files (and autoloads) are saved.

Note that if you need in access the autoloads packages file use
`pam--build-dir' function instead of this variable."
  :group 'pam
  :type 'string)

(defcustom pam-autoloads-filename "my-packages-autoloads.el"
  "Name of the file in which located all packages autoloads.

Note that if you need in access the autoloads packages file use
`pam--autoloads-file' function instead of this variable, the function return the
full path, instead of filename.

By default, file with this name is located inside inside `pam-build-dir', but
you can redefine `pam--autoloads-file' with `advice'"
  :group 'pam
  :type 'string)

;; TODO: ability to choose copy or move `straight' files
(defcustom pam-post-build-functions
  '(pam--copy-straight-files
    pam--save-pkg-autoloads)
  "Abnormal hook run after building a package with `pam-use-package'.

Each hook function is called with the name of the package as a
string.  For forward compatibility, it should accept and ignore
additional arguments.

Defaults to 2 functions:
1. that copies `straight' files into the `pam' build directory.
2. that append autoloads into the `pam' autloads file"
  :group 'pam
  :type 'hook)

;;; Macros

;; (macros defined before the public part, because they affect on byte-compiler)

(defmacro pam--with-straight-hooks (&rest body)
  "Evaluate the BODY with changed hooks for `straight'."
  (declare (indent 0))
  `(let ((straight-use-package-post-build-functions
          (append
           straight-use-package-post-build-functions
           ;; add specific functions which will be runned after build.  Here I
           ;; don't use `add-hook' to the `straight' hooks, because the user
           ;; should can to choose `straight-use-package' or `pam-use-package'
           ;; and they should have the different behaviours.
           pam-post-build-functions)))
     ,@body))

;;; Public

(define-minor-mode pam-install-everything-mode
  "Toggle should or shouldn't `pam' install new packages.

With enabled this minor mode, `pam-use-package' will use `straight-use-package'
to install a package while the default behaviour is do nothing because already
all is installed"
  :global t
  :init-value nil
  :variable pam-need-to-install-pkgs-p
  :group 'pam)

(defun pam-use-package (melpa-style-recipe &optional no-clone no-build)
  "Activate the package and MAYBE install it.

The package is defined with MELPA-STYLE-RECIPE (see the `straight'
documentation).  The behaviour depends on the value of the
`pam-need-to-install-pkgs-p' variable.  If packages must be installed then this
function install them using `straight' with passing NO-CLONE and NO-BUILD to
`straight-use-package' (see below details), otherwise (packages already
installed) do nothing, notice that here activation is extra, because in `pam'
you activate all packages only once.

The result value of this function is either nil, that tells package wasn't
installed or activated or non-nil value otherwise.

The paragraphs below tells about arguments which are used if package should be
installed:

First, the package recipe is registered with straight.el.  If NO-CLONE is a
function, then it is called with two arguments: the package name as a string,
and a boolean value indicating whether the local repository for the package is
available.  In that case, the return value of the function is used as the value
of NO-CLONE instead.  In any case, if NO-CLONE is non-nil, then processing stops
here.

Otherwise, the repository is cloned, if it is missing.  If NO-BUILD is a
function, then it is called with one argument: the package name as a string.  In
that case, the return value of the function is used as the value of NO-BUILD
instead.  In any case, if NO-BUILD is non-nil, then processing halts here.
Otherwise, the package is built and activated.  Note that if the package recipe
has a nil `:build' entry, then NO-BUILD is ignored and processing always stops
before building and activation occurs."
  (interactive (read (read-string "Which recipe: ")))
  (if (not pam-need-to-install-pkgs-p)
      t
    (pam--with-straight-hooks
      (straight-use-package melpa-style-recipe no-clone no-build))))

(defun pam-rebuild-package (melpa-style-recipe &optional recursive)
  "Rebuild the package.

The package is defined with MELPA-STYLE-RECIPE (see the `straight'
documentation).  The difference with `straight-rebuild-package' is that after
build files will be copied into the `pam' directory and autoloads file will be
updated.  In other it is the same function MELPA-STYLE-RECIPE and RECURSIVE will
be passed to `straight-rebuild-package'

Notice that while `pam-use-package' check the mode (install or only activate a
package), but `pam-rebuild-package' don't it, because rebuild is a more concrete
command."
  (interactive (read (read-string "Which recipe to rebuild: ")))
  (pam--with-straight-hooks
    (straight-rebuild-package melpa-style-recipe recursive)))

(defun pam-activate ()
  "Activate all installed `pam' packages.

After that you can load any installed package with `require', `M-x' will show
all commands of these packages, TeXinfo will be included in the manual."
  (interactive)
  (add-to-list 'load-path (pam--build-dir))
  (add-to-list 'Info-default-directory-list (pam--build-dir))
  (load (pam--autoloads-file)))

(defun pam-sync-with-straight ()
  "Copy all `straight' packages files into the `pam' dir, make autoloads file.

Notice that it can take a long time."
  (interactive)
  (delete-file (pam--autoloads-file))
  (dolist (pkg (pam--straight-packages))
    (pam--sync-straight-package pkg)))

(defun pam-create-files ()
  "Make the `pam' build directory and touch the autoloads file."
  (make-directory (pam--build-dir)))

;;; Internal

(defun pam--build-dir ()
  "Return the path to the directory where `pam' store all packages files."
  ;; Yes, here I just return the value of a variable.
  ;; It's better than a variable, because `straight' use variables to get paths
  ;; and `pam' must use function `pam--autoloads-file' that return the path,
  ;; user shouldn't guess: Is it function or variable?  Choice already happened:
  ;; everywhere - function
  pam-build-dir)

(defun pam--autoloads-file ()
  "Return the path to the directory where `pam' store all packages files."
  (file-name-concat pam-build-dir
                    pam-autoloads-filename))

(defun pam--straight-packages ()
  "Get the list of the packages names that were installed with `straight'."
  (cddr  ;; here `cddr' skips "." and ".." from the front list
   (directory-files (straight--build-dir))))

(defun pam--sync-straight-package (pkg &rest _ignore)
  "Sync with `pam' PKG which have already installed and built with `straight'.

It can be hook to `straight-use-package-post-build-functions' instead of other
`pam' functions"
  (run-hook-with-args pam-post-build-functions
                      pkg))

(defun pam--copy-straight-files (pkg &optional _ignore)
  "Copy the files of `straight' PKG into the `pam-build-dir'.

Notice that this function expect that PKG was already built, otherwise this
function can do really strange things.

This is a hook for `straight-use-package-post-build-functions', but it still can
be useful in other cases"
  (pam--copy-files
   (straight--build-dir pkg)
   (pam--build-dir)))

(defun pam--copy-files (src dst)
  "Copy all files and directories inside SRC to the DST directory.

The difference with just copy directory, is that it tries to merge conflicted
directories, instead of just replacing one with other"
  (let ((default-directory src))
    (dolist (file (cddr (directory-files src)))
      ;; copy files: SRC -> DST
      (cond
       ((file-directory-p file)
        ;; make a directory in DST and move all SRC files into it
        (unless (file-exists-p (file-name-concat dst file))
          (make-directory (file-name-concat dst file)))
        (pam--copy-files (expand-file-name file)
                         (file-name-concat dst file)))
       (t
        ;; delete a file from DST and copy from SRC
        (ignore-errors
          (delete-file (file-name-concat dst file)))
        (copy-file file (file-name-concat dst file) 'ok-if-already-exists))))))

(defun pam--save-pkg-autoloads (pkg &optional _ignore)
  "Save all autoloads of `straight' built PKG into `pam' autoloads file.

Notice that this function expect that PKG was already built, otherwise this
function can do really strange things.

Also this function doesn't GENERATE autoloads for PKG, expected that `straight'
already done it, here just save them.

This is a hook for `straight-use-package-post-build-functions', but it still can
be useful in other cases."
  (with-temp-buffer
    ;; insert all autoloads files contents
    (dolist (file (cddr (directory-files (straight--build-dir pkg) 'full)))
      (when (string-suffix-p "-autoloads.el" file)
        (insert-file-contents file)))
    ;; write it to the end of autoloads file
    (write-region (point-min) (point-max)
                  (pam--autoloads-file)
                  'append)))

(provide 'pam)
;;; pam.el ends here
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





(eval-and-compile
  ;; `eval-and-compile' installs all libraries in compile-time
  ;; , so "(require \\='dash)" compiles successufelly
  
  (leaf dash
    :ensure (dash :repo "magnars/dash.el" :host github)
    :global-minor-mode global-dash-fontify-mode
    :require t)

  (leaf s
    :ensure t
    :require t)

  (leaf f
    :ensure t
    :require t)

  ;; (straight-use-package '(just :host github :repo "semenInRussia/just.el"))
  (leaf just                              ;nofmt
    :ensure (just :host github :repo "semenInRussia/just.el")
    :require t)

  (leaf queue
    :ensure t)

  (leaf request
    :ensure t)

  (leaf async
    :ensure t)

  (leaf alert
    :ensure t)

  (leaf fringe-helper
    :ensure t)

  (leaf ht
    :ensure t)

  (leaf ov
    :ensure t)

  (leaf indicators
    :ensure t)

  (leaf svg-lib
    :ensure t
    :require t))



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
        ;; stop loop
        (setq plst nil))
      (setq prev-key (car plst))
      (setq prev-val (cadr plst))
      ;; skip both: value and key
      (setq plst (cddr plst)))
    (when (eq prev-key key)
      (setq result prev-val))
    result))

(defun my-alist-p (obj)
  "Return t, when OBJ is `alist'."
  (and
   (listp obj)
   (not (null obj))
   (consp (car obj))))

(defun my-symbol-append (&rest symbols)
  "Get symbol which has `symbol-name' as concatenation of the each of SYMBOLS."
  (->> symbols (-map 'symbol-name) (apply 's-concat) (intern)))

(defun my-major-mode-to-hook (mm)
  "Return hook for major-mode (MM): python-mode => python-mode-hook."
  (my-symbol-append mm '-hook))

(defun my-major-mode-to-map (mm)
  "Return map for major-mode (MM): python-mode => python-mode-map."
  (my-symbol-append mm '-map))

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
   (--remove (assoc (car it) alist2 testfn))
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



(leaf aas
  :ensure t
  :global-minor-mode aas-global-mode)



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





(leaf aggressive-indent
  :ensure t
  :hook emacs-lisp-mode-hook
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
  :ensure (apheleia :repo "radian-software/apheleia"
                    :host github)
  :defvar (apheleia-formatters apheleia-mode-alist)
  :hook (;; I construct a hook list from the `apheleia-mode-alist' variable ;;
         ;; it's better than a `apheleia-global-mode', because it will be loaded after the
         ;; major mode is entered
         ;;
         ;; php-mode has to come before cc-mode
         (php-mode-hook . apheleia-mode)
         ;; json-mode has to come before javascript-mode (aka-hook js-mode)
         (json-mode-hook . apheleia-mode)
         (json-ts-mode-hook . apheleia-mode)
         ;; rest are alphabetical
         (asm-mode-hook . apheleia-mode)
         (awk-mode-hook . apheleia-mode)
         (bash-ts-mode-hook . apheleia-mode)
         (bazel-mode-hook . apheleia-mode)
         (beancount-mode-hook . apheleia-mode)
         (c++-ts-mode-hook . apheleia-mode)
         (caddyfile-mode-hook . apheleia-mode)
         (cc-mode-hook . apheleia-mode)
         (c-mode-hook . apheleia-mode)
         (c-ts-mode-hook . apheleia-mode)
         (c++-mode-hook . apheleia-mode)
         (caml-mode-hook . apheleia-mode)
         (cmake-mode-hook . apheleia-mode)
         (cmake-ts-mode-hook . apheleia-mode)
         (common-lisp-mode-hook . apheleia-mode)
         (crystal-mode-hook . apheleia-mode)
         (css-mode-hook . apheleia-mode)
         (css-ts-mode-hook . apheleia-mode)
         (dart-mode-hook . apheleia-mode)
         (dart-ts-mode-hook . apheleia-mode)
         (elixir-mode-hook . apheleia-mode)
         (elixir-ts-mode-hook . apheleia-mode)
         (elm-mode-hook . apheleia-mode)
         (fish-mode-hook . apheleia-mode)
         (go-mode-hook . apheleia-mode)
         (go-mod-ts-mode-hook . apheleia-mode)
         (go-ts-mode-hook . apheleia-mode)
         (graphql-mode-hook . apheleia-mode)
         (haskell-mode-hook . apheleia-mode)
         (html-mode-hook . apheleia-mode)
         (html-ts-mode-hook . apheleia-mode)
         (java-mode-hook . apheleia-mode)
         (java-ts-mode-hook . apheleia-mode)
         (js3-mode-hook . apheleia-mode)
         (js-mode-hook . apheleia-mode)
         (js-ts-mode-hook . apheleia-mode)
         (kotlin-mode-hook . apheleia-mode)
         (latex-mode-hook . apheleia-mode)
         (LaTeX-mode-hook . apheleia-mode)
         (lua-mode-hook . apheleia-mode)
         (lisp-mode-hook . apheleia-mode)
         (nasm-mode-hook . apheleia-mode)
         (nix-mode-hook . apheleia-mode)
         (perl-mode-hook . apheleia-mode)
         (purescript-mode-hook . apheleia-mode)
         (python-mode-hook . apheleia-mode)
         (python-ts-mode-hook . apheleia-mode)
         (ruby-mode-hook . apheleia-mode)
         (ruby-ts-mode-hook . apheleia-mode)
         (rustic-mode-hook . apheleia-mode)
         (rust-mode-hook . apheleia-mode)
         (rust-ts-mode-hook . apheleia-mode)
         (scss-mode-hook . apheleia-mode)
         (svelte-mode-hook . apheleia-mode)
         (terraform-mode-hook . apheleia-mode)
         (TeX-latex-mode-hook . apheleia-mode)
         (TeX-mode-hook . apheleia-mode)
         (tsx-ts-mode-hook . apheleia-mode)
         (tuareg-mode-hook . apheleia-mode)
         (typescript-mode-hook . apheleia-mode)
         (typescript-ts-mode-hook . apheleia-mode)
         (web-mode-hook . apheleia-mode)
         (yaml-mode-hook . apheleia-mode)
         (yaml-ts-mode-hook . apheleia-mode))
  :setf (;; a formatter for C++
         ((alist-get 'c++-mode apheleia-mode-alist)
          . 'uncrustify)
         ((alist-get 'uncrustify apheleia-formatters)
          . '("uncrustify" "-f" filepath "-c" uncrustify-cfg-file "-o"))
         ;; find the --edition of "rustfmt"
         ((alist-get 'rustfmt apheleia-formatters)
          . '("rustfmt" "--edition" "2021" "--quiet" "--emit" "stdout"))
         ;; use yapf+isort instead of black for python
         ((alist-get 'python-mode apheleia-mode-alist)
          . '(yapf isort))))



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


(leaf avy
  :ensure (avy :repo "abo-abo/avy" :host github))

(leaf ace-window
  :ensure (ace-window :repo "abo-abo/ace-window"
                      :host github)
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
 ("C-TAB" . 'my-visit-last-opened-buffer))



;;; my-buffer-navigation.el ends here
;;; my-citre.el --- Support of citre: the full ide which is built over `ctags' -*- lexical-bindings: t -*-

;; Copyright (C) 2023

;;; Commentary:

;; Support of citre: the full ide which is built over `ctags'.
;;
;; It provides auto-completion, find definition and other features like the
;; modern LSPs

;;; Code:





(leaf citre
  :ensure t
  :hook c++-mode-hook
  :custom (citre-tag-reference-mark . "")
  :bind (;; C-c u is the `citre' prefix
         ("C-c u u" . 'citre-update-this-tags-file)
         ("C-c u n" . 'citre-create-tags-file)
         ;; `citre-peek' is the useful command that show the definition of the
         ;; command/variable in the small popup window below the cursor
         ;;
         ;; bound to "C-c u d"
         ("C-c u d" . 'citre-peek))
  :config (require 'citre-common-tag))

(provide 'my-citre.el)
;;; my-citre.el ends here
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
  :ensure (comment-dwim-2 :repo "remyferre/comment-dwim-2"
                          :host github)
  :bind ("M-;" . comment-dwim-2))

(provide 'my-comment-dwim-2)
;;; my-comment-dwim-2.el ends here
;;; my-corfu.el.el --- My configuration of `corfu' -*- lexical-binding: t; -*-

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

;; My configuration of `corfu'.  I choose `corfu' over `company' because
;; `company' have a big load time (about 9 secs on my computer) while `corfu'
;; 3secs

;;; Code:



(require 'f)   ; for `f-full'


(leaf compat
  :ensure (compat :repo "emacs-straight/compat" :host github))

(leaf corfu
  :ensure (corfu
           :repo "minad/corfu"
           :files ("*.el" "extensions/*"))
  ;; so when auto-completion is provided, load `corfu'
  ;;
  ;; I load `corfu' only when it really needed.  It's awesome idea, until
  ;; `corfu' don't take some seconds before load, it make Emacs a little worth,
  ;; but with `my-use-afk' it's still cool
  :commands corfu--in-region
  :init (setq-default completion-in-region-function 'corfu--in-region)
  ;; make border of auto-completion minibuffer white/black, it looks like nice
  ;; :custom-face ((corfu-border . '((t :background "black"))))
  :custom (;; by default 2 but 1 one is better
           (corfu-auto-prefix . 1)
           ;; by default to run `corfu' you should press `C-M-i'
           (corfu-auto . t)
           ;; I don't like 0sec, because it bad for yasnippets
           (corfu-auto-delay . 0.4))
  :config
  ;; `completion-in-region-function' was already changed, but
  ;; `global-corfu-mode' enable auto complete, if `corfu-auto' is non-nil
  (global-corfu-mode t)

  ;; show documentation of every auto-completion item
  (leaf corfu-popupinfo
    :bind (:corfu-map
           :package corfu
           ("M-i" . 'corfu-popupinfo-toggle)))

  ;; show icons inside auto-completion popup
  (leaf kind-icon
    :ensure (kind-icon :repo "emacs-straight/kind-icon"
                       :host github)
    :after corfu
    :commands kind-icon-margin-formatter
    :defvar corfu-margin-formatters
    :custom ((kind-icon-use-icons . t)
             ;; don't show the extra space between icon and text
             (kind-icon-extra-space . nil)
             ;; show the icons with the white or other theme background color
             (kind-icon-default-face . 'corfu-default)
             (kind-icon-blend-background . nil)
             ;; when an icons isn't known show the completion without icon
             ;;
             ;; (default is to show ??? with the red background)
             (kind-icon--unknown . " ")
             ;; use the same as a symbol size for icons
             (kind-icon-default-style . `(
                                          :padding 0
                                          :stroke 0
                                          :margin 0
                                          :radius 0
                                          :height 0.5
                                          :width 0.1
                                          :scale 1)))
    :init (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter)))

;; misc

(leaf cape
  :ensure (cape :repo "minad/cape" :host github)
  :require t
  ;; I'm using the file from the following GitHub repository
  ;; https://github.com/dwyl/english-words/
  :custom `(cape-dict-file . ,(f-full "~/.emacs.d/dict/english.txt"))
  :defun (cape-symbol
          cape-dabbrev cape-file cape-elisp-block cape-history
          cape-keyword cape-sgml cape-tex cape-abbrev cape-symbol)
  :hook (corfu-mode-hook . my-capf-local-setup)
  :config
  ;; make that `completion-at-point-functions' will be the different for every buffer
  (make-local-variable 'completion-at-point-functions)

  ;; don't use the following
  ;; LaTeX commands, I prefer built-in AuCTex capfs
  ;;   (add-hook 'completion-at-point-functions #'cape-tex)
  ;; I prefer built-in `elisp-completion-at-point'
  ;;   (add-hook 'completion-at-point-functions #'cape-symbol)
  ;; really strange thing, IDK what is it (know but it hard)
  ;;   (add-hook 'completion-at-point-functions #'cape-sgml)
  ;;   (add-hook 'completion-at-point-functions #'cape-rfc1345)
  ;; enrage!!
  ;;   (add-hook 'completion-at-point-functions #'cape-line)

  (defun my-capf-local-setup ()
    "Change the `completion-at-point-functions' for current buffer."
    ;; Here the numbers is the depth of the respective hooks, the default depth is
    ;; 0, so if `major-mode' provides a capf function, then its depth is 0 (in
    ;; 100% cases which I saw)
    ;;
    ;; If i want that one thing will be prefer over another, than set to more
    ;; important one the number with the LESSER number, so if the depth it will
    ;; be the first function which was checked, here I use the depth from 0 to
    ;; the amount of hooks
    (add-hook 'completion-at-point-functions #'cape-history     1 'local)
    (add-hook 'completion-at-point-functions #'cape-elisp-block 2 'local)
    (add-hook 'completion-at-point-functions #'cape-file        3 'local)
    (add-hook 'completion-at-point-functions #'cape-keyword     4 'local)
    (add-hook 'completion-at-point-functions #'cape-dabbrev     5 'local)
    (add-hook 'completion-at-point-functions #'cape-abbrev      6 'local)
    (add-hook 'completion-at-point-functions #'cape-dict        7 'local)))



;;; my-corfu.el ends here
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
  :ensure (rg :repo "dajva/rg.el"
              :host github))

(leaf dumb-jump
  :ensure (dumb-jump :repo "jacktasia/dumb-jump" :host github)
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



(require 'dash)

(declare-function turn-off-flycheck "my-flycheck.el")


(leaf eglot
  :custom `((eglot-send-changes-idle-time . 1) ; in seconds
            (eglot--executable-find . "C:\\tools\\find.exe"))
  :custom-face (eglot-highlight-symbol-face . '((t (:inherit lazy-highlight))))
  :defun eglot-inlay-hints-mode
  :bind (:eglot-mode-map
         ("C-c lr" . 'eglot-rename)
         ("<f6>"   . 'eglot-rename)
         ("C-c la"  . 'eglot-code-actions)
         ("C-c ll"  . 'eglot-code-actions)
         ([remap my-format-expression] . 'eglot-format))
  :fast-exec (("Start a LSP Server for Current Buffer" 'eglot)
              ("Reconnect the LSP Server" 'eglot-reconnect)
              ("Disable the LSP Server" 'eglot-shutdown))
  :config
  ;; `eglot' use `flymake' instead of `flycheck', so i disable `flycheck'
  (add-hook 'eglot-managed-mode-hook #'turn-off-flycheck)

  ;; don't use inlay hints, because they're a bit useless for me
  ;;
  ;; I'm "True Emacs user"!!!
  (add-hook 'eglot-managed-mode-hook #'eglot-inlay-hints-mode)

  (leaf flymake
    :bind (:flymake-mode-map
           ("C-c fd" . 'flymake-show-project-diagnostics)
           ([remap next-error] . 'flymake-goto-next-error)
           ([remap prev-error] . 'flymake-goto-prev-error)))

  ;; set default LSP servers for all supported languages
  (defvar eglot-server-programs)  ; make compiler happier
  ;; python (pyright)
  (setf (alist-get '(python-mode python-ts-mode) eglot-server-programs)
        '("pyright-langserver" "--stdio")))



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




;; use `eldoc' with `flycheck' instead of echo area
(defvar flycheck-mode)
(defvar flycheck-display-errors-function)
(defvar flycheck-help-echo-function)

(declare-function flycheck-error-group "flycheck.el")
(declare-function flycheck-error-id "flycheck.el")
(declare-function flycheck-error-message "flycheck.el")
(declare-function flycheck-error-level "flycheck.el")
(declare-function flycheck-overlay-errors-at "flycheck.el")


(defun my-flycheck-eldoc (callback &rest _ignored)
  "Print flycheck messages at point by calling CALLBACK."
  (when-let ((flycheck-errors (and flycheck-mode (flycheck-overlay-errors-at (point)))))
    (mapc
     (lambda (err)
       (let ((level (flycheck-error-level err)))
         (funcall callback
                  (format "%s:  %s"
                          (propertize
                           (pcase level
                             ('info
                              "I"
                              ;; (nerd-icons-codicon "nf-cod-info")
                              )
                             ('error
                              "E"
                              ;; (nerd-icons-codicon "nf-cod-error")
                              )
                             ('warning
                              "W"
                              ;; (nerd-icons-codicon "nf-cod-warning")
                              )
                             (_ level))
                           'face (pcase level
                                   ('info
                                    'flycheck-error-list-info)
                                   ('error
                                    'flycheck-error-list-error)
                                   ('warning
                                    'flycheck-error-list-warning)
                                   (_ 'font-lock-doc-face)))
                          (flycheck-error-message err))

                  :thing (or (flycheck-error-id err)
                             (flycheck-error-group err)))))
     flycheck-errors)))

(defun my-flycheck-prefer-eldoc ()
  "Prefer `eldoc' over the echo area for `flycheck'."
  (interactive)
  (add-hook 'eldoc-documentation-functions #'my-flycheck-eldoc nil t)
  (setq eldoc-documentation-strategy 'eldoc-documentation-compose-eagerly)
  (setq flycheck-display-errors-function nil)
  (setq flycheck-help-echo-function nil))

(with-eval-after-load 'flycheck
  (add-hook 'flycheck-mode-hook #'my-flycheck-prefer-eldoc)
  (when flycheck-mode
    (my-flycheck-prefer-eldoc)))

;;; use beautifull documentation popup
(eval-and-compile
  (leaf eldoc
    :require t
    :custom ((eldoc-box-clear-with-C-g . t)
             (eldoc-idle-delay . 1.0)))

  (leaf eldoc-box
    :ensure (eldoc-box :repo "casouri/eldoc-box" :host github)
    :require t
    :custom (eldoc-box-fringe-use-same-bg . t)
    :bind ("C-h C-k" . 'eldoc-box-quit-frame)))

(setq eldoc-box-position-function
      #'my-eldoc-box--bottom-corner-position-function)

(defun my-eldoc-box--bottom-corner-position-function (width _)
  "The default function to set childframe position.
Used by `eldoc-box-position-function'.
Position is calculated base on WIDTH and HEIGHT of childframe text window"
  (pcase-let ((`(,_offset-l ,offset-r ,offset-t) eldoc-box-offset))
    (cons (- (frame-outer-width (selected-frame)) width offset-r)
          ;; y position + v-offset
          offset-t)))

(define-global-minor-mode global-eldoc-box-hover-mode
  eldoc-box-hover-mode
  eldoc-box-hover-mode)

(global-eldoc-box-hover-mode)

;;; use `eldoc' with `eglot'



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
  :ensure (embrace :repo "cute-jumper/embrace.el" :host github)
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
  :ensure (expand-region :repo "magnars/expand-region.el" :host github)
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

(leaf fast-exec
  :load-path "~/projects/fast-exec.el/"
  :defun fast-exec-use
  :bind ("M-=" . fast-exec-exec)
  :commands fast-exec-exec
  :config (require 'my-fast-exec-misc))

(eval-when-compile (require 'fast-exec))



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





(leaf flycheck
  :ensure (flycheck :repo "flycheck/flycheck" :host github)
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
  :ensure (goto-last-change :repo "camdez/goto-last-change.el" :host github)
  :bind ("C-_" . 'goto-last-change))



;;; my-goto-last-change.el ends here
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





;; disable tabs, sorry Richard
(setq-default indent-tabs-mode nil)

(leaf-keys
 (prog-mode-map ("RET" . newline-and-indent)))



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
(require 'corfu)


(leaf posframe
  :ensure (posframe :repo "tumashu/posframe" :host github))

(leaf lsp-bridge
  :load-path* "lisp/site-lisp/lsp-bridge"
  :hook ((lsp-bridge-user-multiserver-dir . "~/lsp/multi")
         (lsp-bridge-user-langserver-dir . "~/lsp/single/")
         (lsp-bridge-mode-hook . (lambda () (corfu-mode 0))))
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
         ("C-c la"  . 'lsp-bridge-code-action)
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

;; My configuration of `meow'.  `meow' is a modal editing mode for Emacs.
;; It was inspired by kakoune and has Helix-like key bindings.  I don't love
;; virgin `meow' (without any configs), because every command is "hardcoded" with
;; contributers.  For example the keybindings "o" and "O" is hardcoded with Lisp expresion
;; and jump only around round parentheses, but can also around quotes, double-quotes, symbols,
;; i think that use `forward-sexp', `backward-sexp' and `mark-sexp' is the better choice.  So
;; i try to move on `boon': also modal editing mode for Emacs that was created 9 years
;; ago, while `meow' only 3 and has by 3 times lesser stars on GitHub.
;; I try to fight with it using my own structural state (see `my-structural')

;;; Code:





(leaf meow
  :ensure (meow :repo "meow-edit/meow" :host github)
  :require t
  :require my-meow-structural
  :defvar (meow-cheatsheet-layout
           meow-cheatsheet-layout-qwerty
           meow-replace-state-name-list)
  :defun (my-meow-setup . my-meow)
  :defun (meow-global-mode
          meow-motion-overwrite-define-key
          meow-motion-overwrite-define-key
          meow-leader-define-key
          meow-normal-define-key
          meow-leader-define-keys
          meow-leader-define-state)
  :custom (meow-use-clipboard . t)
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
     '("<" . previous-error)
     '(">" . next-error)
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
     '("O" . meow-to-block)
     '("o" . embark-act)
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
     '("%" . meow-query-replace-regexp)
     '("<escape>" . ignore)))
  (my-meow-setup)
  (meow-global-mode t)

  ;; settings show of `meow' state in modeline
  ;;
  ;; I prefer more short names of states
  ;; , so NORMAL => N
  ;;      BEACON => B
  ;;      and etc
  (setq meow-replace-state-name-list
        '((structural . "S") ;; structural is my own state
          (normal . "N")
          (motion . "M")
          (keypad . "K")
          (insert . "I")
          (beacon . "B"))))



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




(leaf multiple-cursors
  :ensure (multiple-cursors :repo "magnars/multiple-cursors.el" :host github)
  :bind (("M-i"       . 'mc/edit-lines)
         ("C-,"       . 'mc/mark-next-like-this-word)
         ("C-<"       . mc/mark-previous-like-this-word)))



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
  :ensure (ctrlf :repo "radian-software/ctrlf" :host github)
  :bind ("C-s" . ctrlf-forward-default))

(leaf visual-regexp
  :ensure (visual-regexp :repo "benma/visual-regexp.el" :host github)
  :bind ("M-%" . vr/query-replace))

(leaf spinner
  :ensure t)

(leaf deadgrep
  :ensure (deadgrep :repo "Wilfred/deadgrep" :host github)
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
  :ensure (smartparens :repo "Fuco1/smartparens" :host github)
  :global-minor-mode smartparens-global-mode
  :require smartparens-config
  :defun (sp-clone-sexp sp-use-paredit-bindings)
  :bind (:smartparens-mode-map
         ("C-x C-y" . 'my-sp-clone)
         ("C-c DEL" . 'sp-change-enclosing))
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
  :ensure (whitespace-cleanup-mode :repo "purcell/whitespace-cleanup-mode" :host github)
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
  :ensure (yasnippet :repo "joaotavora/yasnippet")
  :defun (yas--table-hash
          yas--filter-templates-by-condition
          yas--namehash-templates-alist)
  :global-minor-mode yas-global-mode
  :custom `((yas-snippet-dirs .
                              ',(list
                                 (locate-user-emacs-file "snippets")))
            (yas-wrap-around-region . t)))



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



(require 'dash)



(declare-function meow-insert "meow-commands.el")


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
  :ensure (paxedit :repo "promethial/paxedit" :host github)
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

(leaf lisp-mode
  :custom (lisp-body-indent . 2))



;;; my-lisp.el ends here
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
  :config
  (add-hook 'emacs-lisp-mode 'paxedit-mode)

  (leaf inspector
    :ensure (inspector :repo "emacs-straight/inspector" :host github)
    :bind (:emacs-lisp-mode-map
           :package elisp-mode
           ("C-c C-i" . inspector-inspect-last-sexp)))

  (leaf paredit
    :ensure (paredit :repo "https://mumble.net/~campbell/git/paredit.git" :host nil)
    :hook emacs-lisp-mode-hook)

  (leaf eros
    :ensure (eros :repo "xiongtx/eros" :host github)
    :hook emacs-lisp-mode-hook)

  (leaf elisp-refs
    :ensure t))

(leaf suggest
  :ensure (suggest :repo "Wilfred/suggest.el" :host github))

(leaf mocker
  :ensure (mocker :repo "sigma/mocker.el" :host github)
  :doc "A library for testing `elisp' with mocks")

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

;; My Configuration for the Lanugage `racket'

;;; Code:





(require 'dash)
(require 'smartparens)

(declare-function my-autoformat-bind-for-major-mode "my-autoformat")


(leaf racket-mode
  :ensure (racket-mode :repo "greghendershott/racket-mode" :host github)
  :defvar (my-racket-meta-return-functions
           racket-xp-mode-hook
           my-racket-meta-return-cond-clauses-expression-names)
  :defun ((my-racket-meta-return-contracted .
                                            (my-racket
                                             my-racket-meta-return-test-case
                                             my-racket-meta-return-let)))
  :bind (:racket-mode-map
         ("M-RET" . 'my-racket-meta-return))
  :hook ((racket-mode-hook . racket-xp-mode)
         ;; `flycheck' is enough slow plus `racket-xp-mode' highlight
         ;; errors too, so i disable `flycheck' for Racket
         (racket-mode-hook . turn-off-flycheck)
         ;; enable structured editing for the `racket-mode'
         (racket-mode-hook . paxedit-mode))
  :custom (racket-xp-mode-hook . nil)
  :config                                      ;nofmt
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

  (add-to-list 'my-racket-meta-return-functions
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
  :ensure (scribble-mode :repo "emacs-pe/scribble-mode" :host github)
  :config
  (add-hook
   'scribble-mode-hook
   (defun my-autoformat-scribble ()
     "Define `my-autoformat' things for `scribble-mode'."
     (require 'my-autoformat)
     (my-autoformat-bind-for-major-mode 'scribble-mode
                                        'my-autoformat-sentence-capitalization))))



;;; my-racket.el ends here
;;; my-bib.el --- My configuration for bibliography management -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2023 semenInRussia

;; Author: semenInRussia <hrams205@gmail.com>

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; My configuration for bibliography management.

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
  (leaf bibtex-utils
    :ensure (bibtex-utils :repo "plantarum/bibtex-utils" :host github)))

;; load `citar', but for `embark'
(leaf citar
  :ensure t
  :hook (org-mode-hook . citar-capf-setup)
  :bind (:org-mode-map
         :package org
         ("C-c C-S-b" . 'org-cite-insert))
  :custom ((org-cite-global-bibliography . '("~/bib.bib"))
           (org-cite-insert-processor   . 'citar)
           (org-cite-follow-processor   . 'citar)
           (org-cite-activate-processor . 'citar)
           (citar-bibliography . org-cite-global-bibliography))
  :defvar (org-cite-global-bibliography
           citar-indicators
           citar-bibliography
           org-roam-directory)
  :config
  ;; `citar' have integration with `all-the-icons',
  ;; but not with `nerd-icons'

  ;; TODO: `nerd-icons' + `citar'

  ;; (defvar my-citar-indicator-notes-nerd-icons
  ;;   (citar-indicator-create
  ;;    :symbol (nerd-icons-codicon "nf-cod-notebook")
  ;;    :function #'citar-has-notes
  ;;    :padding "  "
  ;;    :tag "has:notes"))

  ;; (setq citar-indicators
  ;;       (list
  ;;        ;; plain text
  ;;        citar-indicator-files
  ;;        my-citar-indicator-notes-nerd-icons))

  ;; `citar' + `embark'
  (leaf citar-embark
    :ensure t
    :after (citar embark)
    :global-minor-mode citar-embark-mode)

  ;; `org-roam' + `citar':
  (leaf parsebib :ensure t)

  (leaf citar-org-roam
    :ensure t
    :after org-roam
    :global-minor-mode citar-org-roam-mode
    :config
    ;; `org-roam' has your own the bibliography.bib file
    ;; -- (in my config)
    (with-eval-after-load 'org-roam
      (add-to-list 'org-cite-global-bibliography (f-join org-roam-directory "bibliography.bib"))
      (add-to-list 'citar-bibliography (f-join org-roam-directory "bibliography.bib")))))



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
  :setq-default (;; enable auto insert newline after ";"
                 (c-auto-newline . t)
                 (c-electric-flag . t)
                 (c-hungry-delete-key . t))
  :config (leaf google-c-style
            :ensure (google-c-style :repo "google/styleguide" :host github)
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


(leaf calc
  :defun (calc-yank-internal calc-pack calc-vector-mean)
  :bind ((:calc-mode-map                ;nofmt
          :package calc
          ("v" . nil)
          ("v y" . my-calc-mean-yank))
         (:calc-edit-mode-map
          :package calc-yank
          ([remap save-buffer] . calc-edit-finish)))
  :config
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


(leaf facemenu
  :fast-exec ("Display All Colors" #'list-colors-display))

(leaf css-mode
  :config                               ;nofmt
  (leaf css-eldoc
    :ensure (css-eldoc :repo "zenozeng/css-eldoc" :host github)
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



(leaf dockerfile-mode
  :ensure (dockerfile-mode :repo "spotify/dockerfile-mode" :host github)
  )



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
  :ensure (elm-mode :repo "jcollard/elm-mode" :host github)
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





;; NOTE that here support only `ecukes' now.
;; `ecukes' is cucumber for `emacs-lisp-mode'
(leaf feature-mode
  :ensure (feature-mode :repo "michaelklishin/cucumber.el" :host github)
  :hook (feature-mode-hook . my-feature-mode-hook)
  :bind (:feature-mode-map
         ("RET" . newline-and-indent)
         ("M-RET" . my-feature-add-and-statement))
  :config
  (defun my-feature-mode-hook ()
    "My hook of the `feature-mode'."
    (setq-local outline-regexp "\\( *Feature:\\| *Scenario:\\)"))

  (defun my-feature-add-and-statement ()
    "Add a \"And\" feature statement, like to statement at the current line."
    (interactive)
    (end-of-line)
    (insert "\n" (buffer-substring-no-properties (pos-bol) (pos-eol)))
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
  :ensure (go-mode :repo "dominikh/go-mode.el" :host github)
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
  :ensure (haskell-mode :repo "haskell/haskell-mode" :host github)
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


(defvar my-html-supported-modes
  '(web-mode mhtml-mode)
  "List of `html` major modes."
  ;; :group 'my
  ;; :type '(repeat symbol)
  )

(defun my-html-supported-modes-hooks ()
  "Return list from the hooks for each of `my-html-supported-modes'."
  (-map 'my-major-mode-to-hook my-html-supported-modes))

(defun my-html-supported-modes-maps ()
  "Return list from the maps for each of `my-html-supported-modes'."
  (-map 'my-major-mode-to-map my-html-supported-modes))

(leaf mhtml-mode
  :mode "\\.html$"
  :hook (mhtml-mode-hook . my-lsp-ensure)
  :config                               ;nofmt
  (leaf auto-rename-tag
    :ensure (auto-rename-tag :repo "jcs-elpa/auto-rename-tag" :host github))

  (leaf tagedit
    :ensure (tagedit :repo "magnars/tagedit" :host github)
    ;;    :bind `(,(--map
    ;; `(,it
    ;;   :package ,(my-map-to-major-mode it)
    ;;   ([remap sp-kill-hybrid-sexp] . tagedit-kill)
    ;;   ([remap sp-join-sexp]        . tagedit-join-tags)
    ;;   ([remap sp-raise-sexp]       . tagedit-raise-tag)
    ;;   ([remap sp-splice-sexp]      . tagedit-splice-tag)
    ;;   ([remap sp-change-enclosing]  . tagedit-kill-attribute))
    ;; (my-html-supported-modes-maps))))
    )
  (leaf emmet-mode
    :ensure (emmet-mode :repo "smihica/emmet-mode" :host github)
    :hook mhtml-mode-hook)

  (leaf impatient-mode
    :ensure (impatient-mode :repo "skeeto/impatient-mode" :host github)
    :defun (imp-visit-buffer impatient-mode)
    :bind (:html-mode-map
           :package mhtml-mode
           ("C-c C-e" . my-enable-impatient-mode))
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
  :defvar lsp-bridge-single-lang-server-mode-list
  :hook (js-mode-hook . my-lsp-ensure)
  :defvar lsp-bridge-multi-lang-server-mode-list
  :mode "\\.js$"
  :config                               ;nofmt
  (require 'lsp-bridge)
  (add-to-list 'lsp-bridge-multi-lang-server-mode-list
               '((typescript-mode js-mode)
                 . "typescript_rome"))
  (leaf js-comint
    :ensure (js-comint :repo "redguardtoo/js-comint" :host github)))

(leaf typescript-mode
  :ensure (typescript-mode :repo "emacs-typescript/typescript.el" :host github)
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
  :ensure (json-mode :repo "joshwnj/json-mode" :host github)
  :bind (:json-mode-map
         ([:remap my-format-expression] . json-pretty-print-buffer))
  :hook (json-mode-hook . my-json-fix-indent-funcs)
  :config                               ;nofmt
  (leaf json-snatcher
    :ensure (json-snatcher :repo "Sterlingg/json-snatcher" :host github)
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
(require 'dash)
(require 'smartparens)


(declare-function aas-set-snippets "aas.el")

(declare-function my-embrace-add-paren-of-cdlatex-math "my-latex.el")
(declare-function my-embrace-add-paren-latex-command "my-latex.el")
(declare-function my-embrace-add-paren-latex-style-command "my-latex.el")
(declare-function my-latex-style-command-left-paren-regexp "my-latex.el")
(declare-function my-latex-style-command-left-paren "my-latex.el")

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

(add-hook 'LaTeX-mode-hook 'turn-off-flycheck)

(add-hook 'LaTeX-mode-hook 'my-latex-find-master-file)
(add-hook 'LaTeX-mode-hook 'my-latex-expansion-mode)
(add-hook 'LaTeX-mode-hook 'my-latex-disable-auto-fill)


(leaf auctex
  :ensure (auctex :repo "emacs-straight/auctex" :host github)
  :mode ("\\.tex$" . latex-mode))


(leaf tex-mode
  :defun (((er/mark-LaTeX-math er/mark-LaTeX-inside-environment)
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
  :bind (:latex-mode-map
         :package tex-mode
         ("C-c C-@"  . my-latex-mark-inside-environment-or-math)
         ("C-c C-\\" . my-latex-equation-to-split)
         ("C-c C-w"  . my-latex-kill-section))
  :config                               ;nofmt
  (leaf xenops
    :ensure (xenops :repo "dandavison/xenops")
    :hook LaTeX-mode-hook
    :after tex-mode
    :custom (xenops-math-image-scale-factor . 2))

  (leaf my-latex-insert
    :load-path* "lisp/languages/latex/"
    :hook (LaTeX-mode-hook . my-latex-expansion-mode))

  ;; (leaf yasnippet
  ;;   :bind (:yas-keymap
  ;;          ("<tab>" . yas-next-field-or-cdlatex)
  ;;          ("TAB"   . yas-next-field-or-cdlatex))
  ;;   :disabled t
  ;;   :config                             ;nofmt
  ;;   (require 'calc-lang)
  ;;   (require 'font-latex)

  ;;   (defun cdlatex-in-yas-field ()
  ;;     (when-let* ((_ (overlayp yas--active-field-overlay))
  ;;                 (end (overlay-end yas--active-field-overlay)))
  ;;       (if (>= (point) end)
  ;;           (let ((s (thing-at-point 'sexp)))
  ;;             (unless (and s
  ;;                          (assoc
  ;;                           (substring-no-properties s)
  ;;                           cdlatex-command-alist-comb))
  ;;               (yas-next-field-or-maybe-expand)
  ;;               t))
  ;;         (let (cdlatex-tab-hook minp)
  ;;           (setq minp
  ;;                 (min
  ;;                  (save-excursion (cdlatex-tab) (point))
  ;;                  (overlay-end yas--active-field-overlay)))
  ;;           (goto-char minp)
  ;;           t))))

  ;;   (defun yas-next-field-or-cdlatex nil
  ;;     "Jump to the next Yas field correctly with cdlatex active."
  ;;     (interactive)
  ;;     (if (or
  ;;          (bound-and-true-p cdlatex-mode)
  ;;          (bound-and-true-p org-cdlatex-mode))
  ;;         (cdlatex-tab)
  ;;       (yas-next-field-or-maybe-expand))))

  (defun my-latex-mark-inside-environment-or-math ()
    "If the cursor place inside of the math environment mark that."
    (interactive)
    (if (texmathp)
        (er/mark-LaTeX-math)
      (er/mark-LaTeX-inside-environment)))

  (leaf laas
    :ensure (laas :repo "tecosaur/LaTeX-auto-activating-snippets" :host github)
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
    :ensure (cdlatex :repo "cdominik/cdlatex" :host github)
    :hook (LaTeX-mode-hook  . turn-on-cdlatex)
    :bind (:cdlatex-mode-map
           ("<tab>" . cdlatex-tab)
           (";" . my-latex-dollar))
    :custom (cdlatex-math-modify-alist
             .
             '((?q "\\sqrt" nil t nil nil)
               (?u "\\breve" "\\uline" t nil nil)
               (?v "\\vec" nil t nil nil)))
    :config
    (define-key cdlatex-mode-map "(" nil)
    (define-key cdlatex-mode-map ")" nil)
    (define-key cdlatex-mode-map "{" nil)
    (define-key cdlatex-mode-map "}" nil)
    (define-key cdlatex-mode-map "[" nil)
    (define-key cdlatex-mode-map "]" nil)
    (define-key cdlatex-mode-map "\"" nil)
    (define-key cdlatex-mode-map "\\" nil)

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

  (leaf my-latex-embrace
    :after embrace
    :defun my-embrace-LaTeX-mode-hook
    :defun (embrace-LaTeX-mode-hook . embrace)
    :config
    (add-hook 'LaTeX-mode-hook #'embrace-LaTeX-mode-hook)
    (add-hook 'LaTeX-mode-hook #'my-embrace-LaTeX-mode-hook)
    (when (eq major-mode 'latex-mode)
      (embrace-LaTeX-mode-hook)
      (my-embrace-LaTeX-mode-hook)))

  (leaf smartparens-latex
    :after smartparens
    :require t)

  (leaf latex-extra
    :ensure (latex-extra :repo "Malabarba/latex-extra" :host github)
    :hook ((LaTeX-mode-hook . latex-extra-mode)
           (LaTeX-mode-hook . visual-line-mode))
    :bind (:latex-mode-map
           :package tex-mode
           ("C-c C-c" . latex/compile-commands-until-done)
           ("C-c C-n" . latex/next-section-same-level)
           ("C-c C-p" . latex/previous-section-same-level)))

  (leaf my-latex-math-spaces
    :hook latex-mode)

  (leaf latex-r
    :load-path "~/projects/latex-r"
    :bind (:latex-mode-map
           :package latex
           ("C-c M-n" . 'latex-r-cycle-math-parens)
           ("C-c C-s" . 'latex-r-split-environment)))

  (defun my-latex-disable-auto-fill ()
    "Disable `auto-fill-mode'."
    (interactive)
    (auto-fill-mode 0))

  (leaf my-latex-drag
    :after my-drag
    :defun ((add-up-dragger add-down-dragger) . my-drag)
    :commands (my-latex-try-drag-left-list-item
               my-latex-try-drag-right-list-item)
    ;; eval after `my-drag' is loaded
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
  :config
  (add-hook 'markdown-mode-hook 'visual-line-mode)

  (leaf markdown-toc
    :ensure (markdown-mode :repo "jrblevin/markdown-mode" :host github)
    :bind (:markdown-mode-map
           :package markdown-mode
           ("C-T" . markdown-toc-generate-or-refresh-toc)))

  (leaf edit-indirect
    :ensure (edit-indirect :repo "Fanael/edit-indirect" :host github))

  (require 'my-autoformat)

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


(leaf org
  :ensure t
  :defun ((aas-set-snippets . aas)
          (meow-insert . meow-command))
  :custom ((org-file-apps
            . '(("\\.\\'" . default)
                ("\\.pdf\\'" . "start %s")
                ("\\.png\\'" . "start %s")
                ("\\.jpg\\'" . "start %s")))
           ;; `org-refile'
           (org-refile-use-outline-path . 'file)
           (org-outline-path-complete-in-steps . nil)
           (org-refile-targets . '((nil :maxlevel . 9)))
           ;; `org' startup
           (org-fold-core-style . 'overlays)
           (org-startup-folded . t)
           (org-startup-indented . t)
           (org-startup-with-inline-images . t))
  :bind (;; NOTE: `org-capture' and `org-agenda' in the my-organization.el file
         ;; ("C-c z c" . org-capture)
         (:org-mode-map
          ("C-c tab"   . org-refile)
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
  :config
  (add-hook 'org-mode-hook 'visual-line-mode)
  (add-hook 'org-mode-hook 'aas-activate-for-major-mode)

  (leaf my-org-editing
    :bind (("C-c M-i"   . my-org-insert-image)
           ("C-c M-u"   . my-org-insert-img-at-url)
           ("C-c C-w"   . my-org-cut)
           ("C-c C-M-w" . my-org-clear-subtree)
           ("C-c C-t"   . my-org-todo)))

  ;; format `org-mode' code after every key hit
  (leaf my-org-autoformat
    :hook (org-mode-hook . my-autoformat-mode))

  (leaf consult
    :bind (:org-mode-map
           :package org
           ([remap consult-imenu] . consult-outline)))

  ;; `org-mode' exporter
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
    :config
    (leaf ox-json
      :ensure (ox-json :repo "jlumpe/ox-json" :host github)
      :commands (ox-json-export-to-buffer
                 ox-json-export-to-file
                 ox-json--merge-alists)
      :after ox
      :init
      (org-export-define-backend 'json
        ;; Transcoders
        (ox-json--merge-alists
         '(
           (template . ox-json-transcode-template)
           (plain-text . ox-json-transcode-plain-text)
           (headline . ox-json-transcode-headline)
           (link . ox-json-transcode-link)
           (timestamp . ox-json-transcode-timestamp))
         (cl-loop for type in (append org-element-all-elements org-element-all-objects)
                  collect (cons type #'ox-json-transcode-base)))
        ;; Filters
        :filters-alist '()
        ;; Options
        :options-alist
        '((:json-data-type-property nil "json-data-type-property" "$$data_type")
          (:json-exporters nil nil nil)
          (:json-property-types nil nil nil)
          (:json-strict nil nil nil)
          (:json-include-extra-properties nil nil t))
        ;; Menu
        :menu-entry
        '(?j "Export to JSON" ((?J "As JSON buffer" ox-json-export-to-buffer)
                               (?j "To JSON file" ox-json-export-to-file))))))

  ;; remove some useless things from the current `org-mode' buffer
  (leaf my-org-do-tidy
    :bind (:org-mode-map
           :package org
           ("C-c M-q" . my-org-tidy)))

  ;; transient to change values of #+OPTIONS and other #+<THINGS>
  ;;
  ;; (Info-goto-node "(org)Export Settings")
  (leaf my-org-options
    :bind (:org-mode-map
           :package org
           ("C-c C-." . my-org-options-transient)))

  ;; very beautifull `org'
  ;;
  ;; for example, it show [1/3] like a pie progress. :o
  (leaf org-modern
    :ensure t
    :hook org-mode-hook)

  (leaf org-autolist
    :ensure t
    :hook org-mode-hook)

  (leaf rorg
    :load-path "~/projects/rorg/"
    :bind (:org-mode-map
           :package org
           ("C-c C-x C-x" . rorg-splice-subtree)
           ("C-c C-0" . rorg-wrap-region-or-current-heading)
           ("C-c M-(" . rorg-wrap-region-or-current-heading)
           ("C-c C-{" . rorg-forward-slurp-subtree)
           ("C-c C-}" . rorg-backward-barf-subtree)
           ("C-c {" . rorg-backward-slurp-subtree)
           ("C-c [" . rorg-forward-barf-subtree)))

  (defun doom-docs-org-mode () (interactive)))

(leaf org-download
  :ensure (org-download :repo "abo-abo/org-download" :host github)
  :hook (dired-mode-hook . org-download-enable))



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
    (goto-char end)
    (insert "]")
    (goto-char beg)
    (insert "Optional["))

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




(leaf typst-mode
  :ensure t
  :commands typst-mode)



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



(require 's)
(require 'dash)
(require 'just)
(require 'f)


(defvar dired-filter-map)

;; the most heavy functions placed at the `my-dired-commands'
;; and will be evaluated when really needed (autoloading)
(declare-function my-dired-save-excursion "my-dired-commands.el")
(declare-function embark-open-externally "embark")


(leaf dired
  ;; don't show extra info about files like:
  ;; - owner
  ;; - group
  ;; - last modified time
  :hook (dired-mode-hook . dired-hide-details-mode)
  :defun dired-get-file-for-visit
  :bind (:dired-mode-map
         ;; i'm the user of `meow' with hjkl, where "h" is right, so i press
         ;; right to go the "back" directory
         ("h" . dired-up-directory)
         ("A" . agnifize-dwim))
  :config
  ;; some my commands for `dired'
  (leaf my-dired-commands
    :bind (:dired-mode-map
           :package dired
           ("~" . my-dired-jump-to-home)
           ("C-x h"   . my-dired-mark-all-files)
           ("C-y"     . my-dired-duplicate)
           ("C-o"     . my-dired-new-file)))

  (leaf dired-async
    :ensure async
    :defun dired-async-mode
    :global-minor-mode dired-async-mode)

  (leaf dired-hacks-utils
    :ensure t)

  ;; filter files from the buffre
  (leaf dired-filter
    :ensure (dired-filter
             :repo "Fuco1/dired-hacks"
             :host github)
    :disabled t
    :defvar dired-filter-map
    :bind-keymap (:dired-mode-map
                  :package dired
                  ("." . dired-filter-map)))

  ;; open PDF and other not in Emacs
  (leaf dired-open
    :ensure t
    :bind (:dired-mode-map
           :package dired
           ("RET" . 'dired-open-file))
    :defvar dired-open-functions
    :push ((dired-open-functions . 'my-dired-open-function-pdf))
    :defun (my-dired . (my-pdf-file-p my-try-open-pdf-file))
    :config                             ;nofmt
    (defun my-dired-open-function-pdf ()
      "Open function for `dired-open-functions'."
      (my-try-open-pdf-file (dired-get-file-for-visit)))

    (defun my-try-open-pdf-file (filename)
      "If file at FILENAME is a pdf file, then open as pdf, other return nil."
      (when (my-pdf-file-p filename)
        (embark-open-externally filename)
        t))

    (defun my-pdf-file-p (filename)
      "Return t, when FILENAME is path to a PDF file."
      (s-suffix-p ".pdf" filename)))

  (leaf dired-subtree
    :ensure (dired-subtree :repo "Fuco1/dired-hacks" :host github)
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

  ;; show directories with 1 file
  ;;
  ;; it be like "a/b.txt", instead of just "a"
  (leaf dired-collapse
    :ensure (dired-collapse :repo "Fuco1/dired-hacks" :host github)
    :hook dired-mode-hook)

  ;; icons inside `dired'
  (leaf nerd-icons-dired
    :ensure (nerd-icons-dired :repo "rainstormstudio/nerd-icons-dired" :host github)
    :hook dired-mode-hook)

  ;; Command for printing file
  (with-eval-after-load 'lpr
    (setq lpr-command "PDFToPrinter"))

  ;; ???
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
  :ensure (dwim-shell-command :repo "xenodium/dwim-shell-command" :host github)
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



(require 'dash)


(leaf magit-section :ensure t)
(leaf with-editor :ensure t)
(leaf git-commit :ensure t)

(leaf magit
  :ensure (magit :repo "magit/magit"
                 :host github)
  :bind (:magit-mode-map
         ("D" . magit-file-delete))
  :custom ((magit-refresh-status-buffer . nil)
           (magit-disabled-section-inserters
            . '(magit-insert-push-branch-header
                magit-insert-tags-header
                magit-insert-unpushed-to-upstream-or-recent
                magit-insert-unpulled-from-upstream)))
  :config
  (add-hook 'magit-mode-hook #'hl-line-mode)
  (magit-auto-revert-mode 0))

(leaf git-timemachine
  :ensure (git-timemachine :repo "pidu/git-timemachine" :host gitlab)
  :fast-exec ("Git Timemachine" 'git-timemachine))

(leaf git-modes
  :ensure (git-modes :repo "magit/git-modes" :host github))

(leaf gitignore-templates
  :fast-exec ("Insert Git Ignore" 'gitignore-templates-insert))

(leaf github-clone
  :ensure (github-clone :repo "dgtized/github-clone.el" :host github)
  :custom (github-clone-directory . "~/projects")
  :fast-exec ("Clone a GitHub Project" 'github-clone))

(leaf line-reminder
  :ensure (line-reminder :repo "emacs-vs/line-reminder" :host github)
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



(leaf hl-todo
  :ensure (hl-todo :repo "tarsius/hl-todo" :host github)
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


(leaf org-agenda
  :custom ((org-agenda-files .
                             '("~/agenda.org"
                               "~/tasks-archive/task-archive.org"))
           (org-agenda-span . 14))
  :bind ("C-c a" . org-agenda))

(leaf nano-agenda
  :ensure t
  :after org-agenda
  :bind (:org-agenda-keymap
         :package org-agenda
         ("a" . nano-agenda)))

(leaf org-capture
  :bind ("C-c z c" . org-capture)
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
  :custom ((annotate-use-echo-area . t)
           (annotate-print-annotation-under-cursor . t)
           (annotate-print-annotation-under-cursor-prefix . "[ann] "))
  :bind (:org-mode-map
         :package org
         ("C-c M-a" . 'annotate-annotate)
         ("C-c C-u M-a" . 'annotate-delete-annotation))
  :config (annotate-mode))



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

;; My config for `consult'.

;;; Code:



(require 'dash)


;; some useful things:
;;
;; - `ripgrep' in the project
;; - choose one from `kill-ring' with preview
;; - `imenu' with preview
;; - switch to buffer one of the project buffers, recent opened files and other
(leaf consult
  :ensure t
  :commands (consult-register-format
             consult-register-window
             consult-xref)
  :init (autoload 'consult-xref "consult-xref")
  :defvar (consult-narrow-key consult-project-function)
  :bind (:minibuffer-local-map
         ("M-s" . consult-history) ;; orig. next-matching-history-element
         ("M-r" . consult-history))
  :bind (("C-x C-b" . consult-buffer)
         ("C-c i" . consult-imenu)
         ("C-c n" . consult-imenu-multi))
  :bind ((:project-prefix-map
          ;; instead of built-in `projectile-find-regexp'
          ;; sometimes use command from `projectile-prefix-map' more useful
          ;; , than "C-c s" for example, when you swithch to project and need to find regexp
          ("g" . consult-ripgrep))
         ;; C-c bindings in `mode-specific-map'
         ("C-c s" . consult-ripgrep)
         ("C-c M-x" . consult-mode-command)
         ("C-c k" . consult-kmacro)
         ("C-c m" . consult-man)
         ([remap Info-search] . consult-info)
         ("M-#" . consult-register-load)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ;; M-g bindings in `goto-map'
         ("M-g e" . consult-compile-error)
         ("M-g I" . consult-imenu-multi))
  :custom ((register-preview-delay  . 0.5)
           (register-preview-function . #'consult-register-format))

  ;; i don't know what does the next line
  :hook ((completion-list-mode-hook . consult-preview-at-point-mode))
  :config

  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Use Consult to select xref locations with preview
  (leaf xref
    :defvar (xref-show-xrefs-function
             xref-show-definitions-function)
    :custom ((xref-show-xrefs-function . #'consult-xref)
             (xref-show-definitions-function . #'consult-xref)))

  ;; `embark' like to flexible keymap that changes depending on
  ;; when I call `embark-act'
  ;;
  ;; here only the integration of `embark' with `vertico', the configuration of
  ;; `vertico' inside `my-embark'
  (leaf embark-consult
    :ensure t
    :hook (embark-collect-mode-hook . consult-preview-at-point-mode))

  ;; the following lines fix some things which are wrong in my emacs@29
  (defvar string-width 0)

  (defun compat-call (&rest _)
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
;;; my-embark.el --- My configuration of `embark' -*- lexical-binding: t; -*-

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

;; My configuration of `embark'.

;;; Code:





;; TODO: put it to other more right place
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)


(leaf embark
  :defvar (embark-keymap-alist marginalia-prompt-categories)
  :defun (magit-status-setup-buffer . magit)
  :ensure t
  :bind (("C-." . embark-act)
         ("C-M-." . embark-dwim)
         (:minibuffer-mode-map
          :package minibuffer           ; built-in
          ("C-," . my-embark-act-noexit)
          ("C-<" . my-embark-act-all-noexit)
          ("C->" . embark-act-all)
          ("C-S-m" . embark-export)
          ("C-M-m" . embark-collect))
         (:embark-general-map
          ("G" . my-embark-google-search))
         (:embark-file-map
          ("G" . my-embark-magit-status)))

  ;; eval after `embark' was loaded
  :config

  (defun my-embark-act-noexit ()
    "Do `embark-act' without exit from the minibuffer."
    (interactive)
    (let ((embark-quit-after-action nil))
      (call-interactively #'embark-act)))

  (defun my-embark-act-all-noexit ()
    "Do `embark-act-all' without exit from the minibuffer."
    (interactive)
    (let ((embark-quit-after-action nil))
      (call-interactively #'embark-act-all)))

  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none))))

  ;;; SOME ADDITIONAL ACTIONS

  ;; googling a thing
  ;;
  ;; was grabbed from the offical wiki
  (defun my-embark-google-search (term)
    "Open google.com to search a given TERM."
    (interactive "sSearch Term: ")
    (browse-url
     (format "http://google.com/search?q=%s" term)))

  ;; support of `magit'
  ;;
  ;; was grabbed from the offical wiki
  (defun my-embark-magit-status (file)
    "Run `magit-status` on repo containing the embark target."
    (interactive "GFile: ")
    (magit-status-setup-buffer (locate-dominating-file file ".git")))

  ;; support of `straight'
  ;;
  ;; was grabbed from the offical wiki
  (with-eval-after-load 'straight
    (defvar-keymap embark-straight-map
      :parent embark-general-map
      "u" #'straight-visit-package-website
      "r" #'straight-get-recipe
      "i" #'straight-use-package
      "c" #'straight-check-package
      "F" #'straight-pull-package
      "f" #'straight-fetch-package
      "p" #'straight-push-package
      "n" #'straight-normalize-package
      "m" #'straight-merge-package)

    (add-to-list 'embark-keymap-alist '(straight . embark-straight-map))

    (with-eval-after-load 'marginalia
      (add-to-list 'marginalia-prompt-categories '("recipe\\|package" . straight)))))

;; support of agnifize.el: my small Emacs package
;; to make a regular Python code into bad code (my sister agnia write bad code)
(leaf agnifize
  :bind ((:embark-file-map
          :package embark
          ("Q" . 'agnifize-file))
         (:embark-buffer-map
          :package embark
          ("Q" . 'agnifize-buffer))
         (:embark-region-map
          :package embark
          ("Q" . 'agnifize-region))))



;;; my-embark.el ends here
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
  :bind (:eshell-mode-map
         :package esh-mode
         ([remap beginning-of-line] . 'eshell-begin-on-new-line)
         ([remap beginning-of-line-text] . 'eshell-begin-on-new-line)))



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



(require 'dash)


(leaf go-translate
  :ensure t
  :custom (gts-translate-list . '(("en" "ru")))
  :bind (;; so I can just hit C-. or o with T following to translate a thing
         ;; at point
         (:embark-region-map
          :package embark
          ("T" . gts-do-translate)))
  :defvar gts-default-translator
  :defun (gts-buffer-render gts-translator gts-prompt-picker gts-google-engine)
  :fast-exec ("Translate a String" 'gts-do-translate)
  :defer-config
  ;; I use Google Translate with the output in the separate buffer
  (setq gts-default-translator
        (gts-translator :picker
                        (gts-prompt-picker)
                        :engines
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

;;; just make PPTX (powerpoint) file that can't be read
(defun my-new-fake-pptx-file ()
  "Make this buffer, fake presentation with format (.pptx)."
  (interactive)
  (->> "~/broken.pptx" (f-read) (insert))
  (text-mode))

(with-eval-after-load 'fast-exec
 (fast-exec-bind 'pptx
                 (fast-exec-make-some-commands
                  ("New Fake PPTX File" 'my-new-fake-pptx-file))))

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

;; My config for `open-junk-file'.

;;; Code:





(leaf open-junk-file
  :ensure t
  :bind ("C-c t" . 'open-junk-file))



;;; my-open-junk-file.el ends here
;;; my-orderless.el --- Settings of `orderless': the match the completion with only some symbols -*- lexical-binding: t -*-
;;; Copyright (c) 2023 semenInRussia

;;; Commentary:
;; Settings of `orderless': the match the completion with only some symbols.

;;; Code:





(leaf orderless
  :ensure (orderless :host github
                     :repo "minad/orderless"
                     :files ("*.el" "extensions/*.el"))
  :commands orderless
  :init (setq completion-styles '(orderless)))



;;; my-orderless.el ends here
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

(leaf emacsql
  :ensure t)

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

  (with-eval-after-load 'Info
    (add-to-list 'Info-directory-list
                 (f-full "~/.emacs.d/straight/repos/org-roam/doc")))

  ;; (require 'org-roam-export)
  ;; (require 'org-roam-protocol)

  (leaf simple-httpd
    :ensure t)

  (leaf org-roam-ui
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



(require 'dash)


(leaf pomidor
  :ensure t
  :bind (("<f12>" . pomidor)
         (:pomidor-mode-map
          ("Q" . kill-buffer)))
  :custom ((pomidor-sound-tack    . nil)
           (pomidor-seconds       . 1800) ; 30min
           (pomidor-sound-tick    . nil)
           (pomidor-confirm-end-break . nil)))

(declare-function pomidor--break-duration "pomidor.el")
(declare-function pomidor--current-state "pomidor.el")
(declare-function pomidor--overwork-duration "pomidor.el")
(declare-function pomidor--work-duration "pomidor.el")
(declare-function pomidor-overwork-p "pomidor.el")

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
;;; my-afk.el --- Load some heavy packages after some seconds of AFK -*- lexical-binding: t; -*-

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

;; When you in afk, load some frequently used things to don't load them after
;; (idea grabbed from the Doomemacs, but I didn't find the implementation, so
;; create my own)

;;; Code:

(defcustom my-use-afk-modules
  '(;; `consult' command `consult-buffer', really frequently useful by me,
    ;; I use it in 99% of Emacs sessions, load it when in AFK
    consult
    ;; auto-completion (`corfu')
    corfu
    ;; my configuration really tied with `embark'.
    ;; I use it for: change reGisTeR of the region, kill the sexp at point,
    ;; do things on the minibuffer items, browse the URL at the cursor
    ffap  ; a dependency
    embark
    ;; I frequently use `dired', but it have a bad start up time, load it in AFK
    dired
    ;; I use `ace-window' (alternative to C-x o) in 100% of cases.
    ;; I don't need to wait 1-2secs before it.
    avy        ; dep
    ace-window
    ;; lsp server (`eglot')
    imenu
    ert
    flymake
    external-completion
    jsonrpc
    xref
    eglot
    ;; `magit' is really big, so I load it in AFK
    magit-section
    with-editor
    transient
    magit-base
    magit-git
    add-log
    pcvs-util
    gmm-utils
    mail-utils
    mm-util
    mailabbrev
    mail-parse
    mm-util
    mm-bodies
    mml
    puny
    rmc
    yank-media
    mailcap
    sendmail
    message
    log-edit
    git-commit
    ;; org-mode
    org-compat
    calendar
    find-func
    format-spec
    org-keys
    ol
    oc
    org-table
    org-fold
    org-cycle
    ;; fast-exec
    fast-exec
    ;; my translator
    gts-core
    gts-implements
    gts-engine-bing
    go-translate
    ;; run-command
    run-command)
  "This is the list of modules which should be loaded after some seconds of AFK.

Emacs will load them when I am not doing anything, so I won't wait loading of
them when they really needed"
  :group 'my
  :type '(repeat (repeat symbol)))

(defcustom my-use-afk-timeout 2
  "Secs in AFK to load next heavy thind from the `my-use-afk-modules'."
  :group 'my
  :type 'number)

(defcustom my-use-afk-timeout-between-loads
  1
  "Secs between loadidng heavy modules."
  :group 'my
  :type 'number)

(defvar my-use-afk-counter 0
  "The counter that tell about the progress of loading heavy things in an AFK time.

Don't set it manually, `my-use-afk-next' will load the heavy next thing and
change the value of this variable")

(defvar my-use-afk-timer nil
  "Timer for load heavy things in AFK.")

(defun my-use-afk-next ()
  "Setup timer to load the rest heavy things after some secs of AFK.

It load the respective Emacs package from the `my-use-afk-modules' list after
`my-use-afk-timeout' seconds depends on `my-use-afk-counter', change the value
of this counter and run timer to load itself after the seconds of AFK"
  (interactive)
  (let ((module (nth my-use-afk-counter my-use-afk-modules)))
    (cond
     ((null module)                     ; all things already loaded -> stop
      (message "All heavy things have already loaded")
      (my-use-afk-stop)
      t)
     ((featurep module)      ; the current thing already loaded -> load the next
      (message "The module have already loaded: %s" module)
      (cl-incf my-use-afk-counter)
      (my-use-afk-next))
     (t                                 ; just load the current module
      (message "Load the module, because u in AFK: %s..." module)
      (unless (ignore-errors (require module))  ; can't load module
        (message "Can't load the module in afk: %s" module))
      (cl-incf my-use-afk-counter)
      (run-with-timer
       my-use-afk-timeout-between-loads
       nil
       (lambda ()
         ;; this code will runned after `my-use-afk-timeout-between-loads'
         ;; seconds after loading a heavy thing.
         ;;
         ;; here check that all time before load a heavy thing, the user wasn't
         ;; doing anything, in this case users still in AFK, so load the next
         ;; heavy thing
         (when (time-less-p
                (time-add
                 my-use-afk-timeout
                 my-use-afk-timeout-between-loads)
                (current-idle-time))
           (my-use-afk-next))))))))

(defun my-use-afk-stop ()
  "Don't using AFK time to load heavy things anymore."
  (interactive)
  (cancel-timer my-use-afk-timer))

(defun my-use-afk-first-start ()
  "Load all heavy things in AFK starting from the first one.

It useful when an error in AFK loading was occured and you need to try fix it."
  (interactive)
  (setq my-use-afk-counter 0)
  (my-use-afk-setup-timer))

(defun my-use-afk-setup-timer ()
  "Load the next heavy thing after some seconds in AFK."
  (setq my-use-afk-timer
        (run-with-idle-timer my-use-afk-timeout t 'my-use-afk-next)))

(define-minor-mode my-use-afk-mode
  "Use time when user do nothing (AFK time) to load heavy things.

It's useful, because when this mode is enabled and user doing nothing Emacs do a
hard work to don't do it when user type a text."
  :global t
  :group 'misc
  :init-value nil
  (if my-use-afk-mode
      (my-use-afk-first-start)
    (my-use-afk-stop)))

;; when Emacs is started start using AFK
(add-hook 'after-init-hook 'my-use-afk-mode)



;;; my-use-afk.el ends here
;;; my-vertico.el --- Settings of `vertico': the modern completion -*- lexical-binding: t -*-

;;; Copyright (c) 2023

;;; Commentary:

;; Settings of `vertico': the modern completion.

;;; Code:





(leaf vertico
  :ensure (vertico :host github
                   :repo "minad/vertico"
                   :files ("*.el" "extensions/*.el"))
  :commands vertico--advice
  ;; it's part of `vertico-mode'
  :init
  (advice-add 'completing-read-default :around #'vertico--advice)
  (advice-add 'completing-read-multiple :around #'vertico--advice)
  :config (vertico-mode t)
  :config
  ;; I press `M-delete' to go the up directory inside of `vertico'
  ;; and TAB to enter into the directory.
  (leaf vertico-directory
    :bind (:vertico-map
           :package vertico
           ;; instead I press TAB
           ;; ("RET" . vertico-directory-enter)
           ("DEL" . vertico-directory-delete-char)
           ("M-DEL" . vertico-directory-delete-word)))

  ;; beautifull icons inside `vertico'
  (leaf nerd-icons
    :ensure t)

  (leaf nerd-icons-completion
    :ensure t
    :commands nerd-icons-completion-mode
    ;; `marginalia' and this both use the same way to display info inside `vertico',
    ;; if i load it before `marginalia', then it wasn't working
    :hook marginalia-mode-hook)

  ;; show a bit of additional info inside the `vertico' `minibuffer'
  (leaf marginalia
    :ensure t
    :global-minor-mode marginalia-mode))



;;; my-vertico.el ends here
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

(with-eval-after-load 'fast-exec
  (fast-exec-bind
   'writing-config
   (fast-exec-make-some-commands
    ("New Config Module" 'my-new-config-module))))

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

(eval-when-compile
  (require 'dash)
  (require 's)
  (require 'f))

(defun my-move-all-straight-packages-files-into-dir (dest)
  "Move all files of all installed and packages built with `straight' into DEST."
  (interactive (list "~/.emacs.d/telpa"))
  (f-mkdir-full-path dest)
  (my-copy-files
   (file-expand-wildcards (concat "~/.emacs.d/straight/build/" "*/*"))
   dest)
  (my-create-package-autoloads dest))

(defun my-create-package-autoloads (dest)
  "Create one file of package autoloads in the DEST from other autoloads files."
  (let ((default-directory dest))
    (with-temp-buffer
      (->>
       dest
       directory-files
       (--filter (s-suffix-p "-autoloads.el" it))
       (mapc #'insert-file-contents))
      (write-region (point-min) (point-max) (f-join dest "my-package-autoloads.el")))))

(defun my-copy-files (files dest)
  "Copy all FILES into the directory DEST."
  (--each files
    (if (f-dir-p it)
        (ignore-errors
          (copy-directory it
                          (f-join dest (f-filename it))))
      (copy-file it
                 (f-join dest (f-filename it))
                 'ok-if-exists))))



;;; my-writing-config.el ends here
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

;; you can install this font, from the GitHub repo `nerd-fonts'
(defcustom my-fonts-main
  "JetBrains Mono"
  "Name of the main font to display all."
  :group 'my
  :type 'string)

(defcustom my-font-size
  20
  "Name of the main font to display all."
  :group 'my
  :type 'number)

(set-face-attribute 'default nil
                    :height (* my-font-size 10)
                    :family my-fonts-main)

(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-language-environment 'utf-8)
(set-selection-coding-system 'utf-8)



;;; my-fonts.el ends here
;;; my-layout.el --- My settings to layout: paddings -*- lexical-binding: t; -*-

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

;; My settings to layout: paddings

;;; Code:

(require 'disp-table)

;; (setq default-frame-alist
;;       (append (list
;;                '(min-height . 1)
;;                '(height     . 45)
;;                '(min-width  . 1)
;;                '(width      . 81)
;;                '(vertical-scroll-bars . nil)
;;                '(internal-border-width . 24)
;;                '(left-fringe    . 1)
;;                '(right-fringe   . 1)
;;                '(tool-bar-lines . 0)
;;                '(menu-bar-lines . 0))))

;; on OSX, type the line below (in terminal) to get a 1 pixel border
;; defaults write com.apple.universalaccess increaseContrast -bool YES

;; To control anti-aliasing on OSX:
;; defaults write org.gnu.Emacs AppleFontSmoothing -int 0 (none)
;; defaults write org.gnu.Emacs AppleFontSmoothing -int 1 (light)
;; defaults write org.gnu.Emacs AppleFontSmoothing -int 2 (medium)
;; defaults write org.gnu.Emacs AppleFontSmoothing -int 3 (strong)

;; Fall back font for glyph missing in Roboto
(defface fallback '((t :family "Fira Code"
                       :inherit 'nano-face-faded)) "Fallback")
(set-display-table-slot standard-display-table 'truncation
                        (make-glyph-code ?… 'fallback))
(set-display-table-slot standard-display-table 'wrap
                        (make-glyph-code ?? 'fallback))

;; (set-fontset-font t nil "Fira Code" nil 'append)

;; Fix bug on OSX in term mode & zsh (spurious % after each command)
(add-hook 'term-mode-hook
          (lambda () (setq buffer-display-table (make-display-table))))

(setq inhibit-startup-screen t
      inhibit-startup-message t
      inhibit-startup-echo-area-message t
      initial-scratch-message nil)

;; (global-hl-line-mode 0)
(setq x-underline-at-descent-line t)

;; Vertical window divider
(setq window-divider-default-right-width 24)
(setq window-divider-default-places 'right-only)
(window-divider-mode 1)

;; No ugly button for checkboxes
(setq widget-image-enable nil)

;; Hide org markup for README
(setq org-hide-emphasis-markers t)

;; paddings
(leaf spacious-padding
  :ensure t
  :global-minor-mode spacious-padding-mode)

;;; my-layout.el ends here
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
;; - `doom-1337'.  The best one, I think
;; - `gruber-darker'.  Cool, but `org-mode' and `vertico' are bad
;; - `doom-monokai-classic'.  Cool
;; - `solarized'
;; - `flatland-theme'




(setq font-lock-maximum-decoration t
      truncate-partial-width-windows nil)

(leaf doom-themes
  :ensure t)

(leaf gruber-darker-theme
  :ensure t)

(leaf monokai-theme
  :ensure t)

(leaf modus-themes
  :custom ((modus-themes-bold-constructs . t)
           (modus-themes-italic-constructs . t))
  :config
  (load-theme 'modus-operandi t)
  (global-hl-line-mode))

(leaf ef-themes
  :require t
  :config
  (load-theme 'ef-cyprus t)
  (global-hl-line-mode))

;; (load-theme 'doom-1337 t)

;; (custom-set-faces
;;  `(region
;;    ((t (:background "white")))))

(setq line-spacing 0.2)



;;; my-load-theme.el ends here
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

;; My configuration for modeline

;;; Code:





(leaf nerd-icons :ensure t)
(leaf shrink-path :ensure t)

(leaf doom-modeline
  :ensure t
  :ensure t
  :custom (;; it looks like more nice
           (doom-modeline-height . 48)
           ;; enconding not useful I think.
           (doom-modeline-buffer-encoding . nil)
           ;; don't show directory names in `doom-modeline'
           (doom-modeline-project-detection . 'project)
           (doom-modeline-buffer-file-name-style . 'buffer-name))
  :hook after-init-hook
  :config
  ;; I use Emacs in fullscreen mode, so I don't see time that provided
  ;; by OS, so I need time in modeline.  EMACS IS MY OS!!!
  ;; I need only to time (not date) in 24hour format
  (defvar display-time-format) ;; make compile happy
  (setq display-time-format "%H:%M")
  (display-time-mode 1)

  ;; disable show line and column numbers in modeline, because it only
  ;; take off extra place
  (column-number-mode 0)
  (line-number-mode 0)

  ;; show size of the file.  My Emacs don't show line numbers, but know about
  ;; amount of text in the file is important
  (size-indication-mode t))

(define-minor-mode my-modeline-at-top-mode
  "Place mode-line at the top of the screen."
  :value nil
  (if my-modeline-at-top-mode
      (progn
        (setq-default header-line-format mode-line-format)
        (setq-default mode-line-format nil))
    (setq-default mode-line-format header-line-format)
    (setq header-line-format nil)))



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

(declare-function server-running-p "server")

(add-hook
 'emacs-startup-hook
 (defun my-maybe-server-start ()
   "Run the Emacs server if servers haven't been started."
   (require 'server)
   (unless (server-running-p)
     (server-start))))



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

(provide 'my-modules)