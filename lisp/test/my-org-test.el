;;; my-org-test.el --- Tests for my configuration of `org-mode'

;; Copyright (C) 2022-2025 semenInRussia

;; Author: semenInRussia <hrams205@gmail.com>

;;; Commentary:

;; Tests for my configuration of `org-mode'

;;; Code:

(require 'ert)

(ert-deftest my-org-check-list-item-p
    ()
  (with-temp-buffer
    (insert "- jdejde")
    (should (my-org-list-item-p))
    (newline)
    (insert "  + jdeijaewojdi")
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
