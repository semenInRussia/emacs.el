;;; my-org-test.el --- Tests for my configuration of `org-mode'

;; Copyright (C) 2022 Semen Khramtsov

;; Author: Semen Khramtsov <hrams205@gmail.com>

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

;; Tests for my configuration of `org-mode'

;;; Code:

(require 'ert)

(ert-deftest my-org-check-list-item-p
    ()
  (with-temp-buffer
    (insert "- jdejde")
    (should (my-org-list-item-p))))

(ert-deftest my-org-properties-end-p
    ()
  (with-temp-buffer
    (insert "    :END:")
    (should (my-org-properties-end-p))
    (newline)
    (should-not (my-org-properties-end-p))))
(provide 'my-org-test)
;;; my-org-test.el ends here
