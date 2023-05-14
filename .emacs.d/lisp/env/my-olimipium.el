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

(require 'my-lib)

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

(with-eval-after-load 'fast-exec
  (fast-exec-bind 'olimpium
    (fast-exec-make-some-commands
     ("New Olimpium Task" 'my-olimpium-new-solution))))

(provide 'my-olimipium)
;;; my-olimipium.el ends here
