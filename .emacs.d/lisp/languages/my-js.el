;;; my-js.el --- My configuration for `js'

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

;; My configuration for `js'

;;; Code:

(defun my-enable-flycheck () (flycheck-mode 1))

(leaf js2-mode
  :ensure t
  :mode "\\.js$"
  :custom ((js2-allow-rhino-new-expr-initializer . nil)
           (js2-auto-indent-p . nil)
           (js2-enter-indents-newline . nil)
           (js2-global-externs .
                               '("module"
                                 "require"
                                 "buster"
                                 "sinon"
                                 "assert"
                                 "refute"
                                 "setTimeout"
                                 "clearTimeout"
                                 "setInterval"
                                 "clearInterval"
                                 "location"
                                 "__dirname"
                                 "console"
                                 "JSON"))
           (js2-idle-timer-delay . 0.1)
           (js2-indent-on-enter-key . t)
           (js2-mirror-mode . nil)
           (js2-strict-inconsistent-return-warning . nil)
           (js2-auto-indent-p . t)
           (js2-include-rhino-externs . nil)
           (js2-include-gears-externs . nil)
           (js2-concat-multiline-strings . 'eol)
           (js2-rebind-eol-bol-keys . nil)
           (js2-show-parse-errors . nil)
           (js2-strict-missing-semi-warning . nil)
           (js2-strict-trailing-comma-warning . t)
           (js-indent-level . 2))
  :hook (js2-mode . my-enable-flycheck)
  :config                               ;nofmt
  (leaf web-mode :ensure t)
  (leaf js-comint :ensure t))

(provide 'my-js)
;;; my-js.el ends here
