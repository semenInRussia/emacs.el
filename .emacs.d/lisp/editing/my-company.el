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

(leaf company
  :after (yasnippet)
  :config                             ;nofmt
  (defvar company-mode/enable-yas t
    "Enable yasnippet for all backends.")

  (defun company-mode/backend-with-yas (backend)
    (if (or
         (not company-mode/enable-yas)
         (and (listp backend) (member 'company-yasnippet backend)))
        backend
      (append
       (if (consp backend) backend (list backend))
       '(:with company-yasnippet))))

  (setq company-backends
        (mapcar #'company-mode/backend-with-yas company-backends)))

(provide 'my-company)
;;; my-company.el ends here
