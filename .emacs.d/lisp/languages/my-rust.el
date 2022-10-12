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
(leaf rust-mode
  :ensure t
  :config                               ;nofmt
  (leaf racer                           ;nofmt
    :ensure t
    :hook ((rust-mode-mode  . racer-mode)
           ;; (racer-mode-mode . eldoc-mode)
           ))

  (leaf flycheck-rust
    :ensure t
    :require t
    :config (flycheck-rust-setup)))

(provide 'my-rust)
;;; my-rust.el ends here
