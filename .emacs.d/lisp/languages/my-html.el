;;; my-html.el --- my-html

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
(defcustom html-modes
  '(web-mode html-mode mhtml-mode)
  "List of `html` major modes.")

(use-package web-mode
    :ensure t
    :config (my-define-local-major-mode-map 'html
                                            '(html-mode
                                              web-mode
                                              web-mode-prog-mode
                                              mhtml-mode))
    :hook (web-mode . yas-minor-mode-off)
    :custom (web-mode-script-padding 1)
    (web-mode-block-padding 0))

(use-package auto-rename-tag
    :ensure t
    :config :init
    (--each html-modes
      (add-hook
       (intern (s-append "-hook" (symbol-name it)))
       (lambda () (auto-rename-tag-mode 38)))))

(use-package tagedit
    :ensure t
    :init (--each html-modes
            (let ((map-symbol (intern (s-append "-map" (symbol-name it))))
                  map)
              (when (boundp map-symbol)
                (setq map (eval map-symbol))
                (define-key map [remap sp-kill-hybrid-sexp] 'tagedit-kill)
                (define-key map [remap sp-join-sexp] 'tagedit-join-tags)
                (define-key map [remap sp-raise-sexp] 'tagedit-raise-tag)
                (define-key map [remap sp-splice-sexp] 'tagedit-splice-tag)
                (define-key
                    map
                    [remap sp-change-enclosing]
                  'tagedit-kill-attribute)))))

(use-package company-web
    :ensure t
    :init (add-hook 'web-mode-hook
                    (lambda ()
                      (set
                       (make-local-variable 'company-backends)
                       '(company-web-html))
                      (company-mode t))))

(use-package impatient-mode
    :ensure t
    :bind ((:map my-html-local-map)
           ("e" . 'my-enable-impatient-mode)))

(defun my-enable-impatient-mode ()
  "Enable `impatient-mode' and open the page of current browser in web browser."
  (interactive)
  (impatient-mode +1)
  (->>
   (buffer-name)
   (s-prepend "http://localhost:8080/imp/live/")
   (browse-url)))

(provide 'my-html)
;;; my-html.el ends here
