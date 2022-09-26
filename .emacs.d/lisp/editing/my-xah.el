;;; my-xah.el --- My config of the `xah-fly-keys'

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

;; My config of the `xah-fly-keys'

;;; Code:
(use-package xah-fly-keys
    :load-path "site-lisp"
    :init (defvar xah-fly-insert-state-p t)
    :config                             ;nofmt
    (xah-fly-keys-set-layout "qwerty")
    (xah-fly-keys 1)
    :bind ((:map xah-fly-command-map)
           ("SPC l"   . nil)
           ("SPC j"   . nil)
           ("SPC SPC" . nil)))

(defvar my-local-major-mode-map nil
  "My map for current `major-mode'.")

(defun my-local-major-mode-map-run ()
  "Run `my-local-major-mode-map'."
  (interactive)
  (set-transient-map my-local-major-mode-map))

(bind-keys
 :map xah-fly-command-map
 ("SPC l" . my-local-major-mode-map-run))

(add-to-list 'use-package-keywords :major-mode-map)

(defun use-package-normalize/:major-mode-map (pkg-name keyword args)
  "Normalizer of :major-mode-map for `use-package'.

Should return `plist' with followed keywords

:keymap-name
Name of the keymap without special prefixes and suffixes.

:major-modes
List of the major-modes in which should work keymap

:parent
Parent of the `major-mode' keymap

PKG-NAME, KEYWORD and ARGS"
  (let* ((args-plist (car args))
         keymap-name
         parent
         major-modes)
    (cond
      ((eq args-plist t)
       (setq keymap-name pkg-name)
       (setq major-modes (list pkg-name)))
      ((symbolp args-plist)
       (setq keymap-name args-plist)
       (setq major-modes (list pkg-name)))
      (t
       (setq keymap-name
             (or (-first 'symbolp args-plist) pkg-name))
       (setq major-modes
             (or (-first 'listp args-plist) (list pkg-name)))
       (setq parent (plist-get args-plist :parent))))
    (list
     :keymap-name keymap-name
     :major-modes major-modes
     :parent parent)))

(defcustom my-major-mode-map-prefix "my-"
  "Prefix for the local `major-mode' keymap variable name."
  :type 'string)

(defcustom my-major-mode-map-suffix "-local-map"
  "Suffix for the local `major-mode' keymap variable name."
  :type 'string)

(defvar my-major-mode-local-maps
  (make-hash-table :test 'eql)
  "Hashtable of the `major-mode's and respective keymaps.")

(defun use-package-handler/:major-mode-map
    (pkg-name keyword keymap-info rest state)
  (let ((body (use-package-process-keywords pkg-name rest state))
        (keymap-name (plist-get keymap-info :keymap-name))
        (major-modes (plist-get keymap-info :major-modes))
        (parent (plist-get keymap-info :parent)))
    (cons
     `(my-define-local-major-mode-map ',keymap-name ',major-modes ,parent)
     body)))

(defun my-define-local-major-mode-map (keymap-name major-modes &optional parent)
  "Define local keymap for the MAJOR-MODES.

Name of the local keymap will be KEYMAP-NAME + some suffixes and
prefixes (KEYMAP-NAME is a symbol).

PARENT is the parent keymap for the local map."
  (let ((keymap-symbol
         (define-prefix-command-for-major-mode keymap-name)))
    (set-keymap-parent (eval keymap-symbol) parent)
    (--each major-modes
      (puthash it keymap-symbol my-major-mode-local-maps))))

(defun define-prefix-command-for-major-mode (keymap-name)
  "Define prefix command for the `major-mode' with the KEYMAP-NAME.

KEYMAP-NAME has type symbol.

Name of the keymap variable will has the followed form:
my-{keymap-name}-local-map"
  (->
   keymap-name
   (symbol-name)
   (s-prepend my-major-mode-map-suffix)
   (s-append my-major-mode-map-prefix)
   (intern)
   (define-prefix-command)))

(defun my-change-local-major-mode-map (&optional mode)
  "Change local major mode keymap for the MODE.

MODE defaults to the current `major-mode'.  See `my-local-major-mode-map-run'"
  (interactive)
  (or mode (setq mode major-mode))
  (setq-local my-local-major-mode-map
              (gethash mode my-major-mode-local-maps)))

(add-hook 'after-change-major-mode-hook 'my-change-local-major-mode-map)

(provide 'my-xah)
;;; my-xah.el ends here
