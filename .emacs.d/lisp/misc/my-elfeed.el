;;; my-elfeed.el --- My configuration of `elfeed' -*- lexical-binding: t; -*-

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

;; My configuration of `elfeed'.

;;; Code:

(leaf elfeed
  :ensure t
  :bind (:xah-fly-command-map
         :package xah-fly-keys
         ("SPC i i" . 'elfeed))
  :config                               ;nofmt
  (leaf elfeed-org                      ;nofmt
    :ensure t
    :require t
    :defvar (rmh-elfeed-org-files my-elfeed-main-opml)
    :custom ((rmh-elfeed-org-files . '("~/opmls/main.org"))
             (my-elfeed-main-opml  . "~/opmls/main.opml"))
    :config                             ;nofmt
    (defun my-elfeed-org-export ()
      "Export each of the `rmh-elfeed-org-files' to opml file at special dir.

Special directory configured in `my-elfeed-main-opml' variable"
      (interactive)
      (let ((opml-body
             (->>
              rmh-elfeed-org-files
              (--map
               (rmh-elfeed-org-convert-org-to-opml
                (find-file-noselect it)))
              (apply 'concat))))
        (with-temp-buffer
          (insert "<?xml version=\"1.0\"?>\n")
          (insert "<opml version=\"1.0\">\n")
          (insert "  <head>\n")
          (insert "    <title>Elfeed-Org Export</title>\n")
          (insert "  </head>\n")
          (insert "  <body>\n")
          (insert opml-body)
          (insert "  </body>\n")
          (insert "</opml>\n")
          (f-touch my-elfeed-main-opml)
          (f-write (buffer-string) 'utf-8 my-elfeed-main-opml))))

    (my-elfeed-org-export)
    (elfeed-load-opml my-elfeed-main-opml))

  (leaf elfeed-goodies
    :ensure t
    :require t
    :commands elfeed-goodies/setup
    :config (elfeed-goodies/setup))

  (leaf elfeed-tube :ensure t :require t))

(provide 'my-elfeed)
;;; my-elfeed.el ends here
