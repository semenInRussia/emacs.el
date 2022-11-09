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

(defvar my-local-major-mode-map nil
  "My map for current `major-mode'.")

(defun my-local-major-mode-map-run ()
  "Run `my-local-major-mode-map'."
  (interactive)
  (set-transient-map (eval my-local-major-mode-map)))

(defcustom my-major-mode-map-prefix "my-"
  "Prefix for the local `major-mode' keymap variable name."
  :type 'string
  :group 'my)

(defcustom my-major-mode-map-suffix "-local-map"
  "Suffix for the local `major-mode' keymap variable name."
  :type 'string
  :group 'my)

(defvar my-major-mode-local-maps
  (make-hash-table :test 'eql)
  "Hashtable of the `major-mode's and respective keymaps.")

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
my-{keymap-name}-local-map.

If keymap have defined already, then do nothing."
  (let ((prefix-command
         (->
          keymap-name
          (symbol-name)
          (s-prepend my-major-mode-map-suffix)
          (s-append my-major-mode-map-prefix)
          (intern))))
    (unless (boundp prefix-command)
      (define-prefix-command prefix-command))
    prefix-command))

(defun my-change-local-major-mode-map (&optional mode)
  "Change local major mode keymap for the MODE.

MODE defaults to the current `major-mode'.  See `my-local-major-mode-map-run'"
  (interactive)
  (or mode (setq mode major-mode))
  (setq-local my-local-major-mode-map
              (gethash mode my-major-mode-local-maps)))

(add-hook 'after-change-major-mode-hook
          'my-change-local-major-mode-map)

(leaf xah-fly-keys
  :ensure (xah-fly-keys :host github :repo "xahlee/xah-fly-keys")
  :commands xah-fly-insert-mode-activate
  :require t
  :bind (("M-SPC" . xah-fly-command-mode-activate)
         (:xah-fly-command-map ("SPC l" . my-local-major-mode-map)))
  :config                               ;nofmt
  (xah-fly-keys-set-layout "qwerty")
  (xah-fly-keys 1)
  (defun my-format-expression ()
    "Format the current expression, depends on major modes.

Not each `major-mode' support the formatting, for add formatting to your
`major-mode' you should use the following Elisp code:


(define-key major-mode-map [remap my-format-expression] 'your-function)"
    (interactive)
    ;; default implementation
    (if (use-region-p)
        (format-all-region (region-beginning) (region-end))
      (user-error
       "for this major mode reformat support only with region")))

  (leaf-keys
   (:xah-fly-command-map
    ("SPC l"     . my-local-major-mode-map-run)
    ("SPC SPC"   . nil)
    ("SPC SPC q" . my-format-expression)
    ("SPC z"     . nil))))

(provide 'my-xah)
;;; my-xah.el ends here
