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
(require 'my-leaf)

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

(provide 'my-pandoc)
;;; my-pandoc.el ends here
