;;; my-lib-test.el --- Tests for `my-lib'

;; Copyright (C) 2022-2023 Semen Khramtsov

;; Author: Semen Khramtsov <hrams205@gmail.com>

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Tests for `my-lib'

;;; Code:

(require 'ert)

(ert-deftest my-lib-check-goto-lisp-sexp-begin
    ()
  (with-temp-buffer
    (insert "(with-temp-buffer (message) (print))")
    (my-goto-lisp-sexp-begin "print")
    (insert "|")
    (should
     (equal
      (buffer-string)
      "(with-temp-buffer (message) (|print))"))
    (my-goto-lisp-sexp-begin "message")
    (insert "|")
    (should
     (equal
      (buffer-string)
      "(with-temp-buffer (|message) (|print))"))
    (my-goto-lisp-sexp-begin "with-temp-buffer")
    (insert "|")
    (should
     (equal
      (buffer-string)
      "(|with-temp-buffer (|message) (|print))"))))

(ert-deftest my-lib-check-in-lisp-sexp-p
    ()
  (with-temp-buffer
    (insert
     " (message \"OK!\")
(eval-after-load 'fast-exec
 '(progn
    (defun add (x y))
  (+ x y)))")
    (search-backward "add")
    (should (my-in-lisp-sexp-p "progn"))
    (should (my-in-lisp-sexp-p "eval-after-load"))
    (should (my-in-lisp-sexp-p "defun"))
    (should-not (my-in-lisp-sexp-p "+"))
    (should-not (my-in-lisp-sexp-p "message"))))

(ert-deftest my-lib-check-alist-p
    ()
  (should
   (my-alist-p '((a . 3) (a . 2))))
  (should-not (my-alist-p nil))
  (should-not (my-alist-p '(a (a . 2)))))

(ert-deftest my-lib-check-uri-of-url
    ()
  (should
   (string-equal (my-uri-of-url "https://google.com/test") "test"))
  (should (string-equal (my-uri-of-url "https://google.com") ""))
  (should (string-equal (my-uri-of-url "https://google.com/") ""))
  (should
   (string-equal
    (my-uri-of-url "https://google.com/test/u.jpg")
    "u.jpg")))

(ert-deftest my-lib-check-uri-of-url
    ()
  (should (my-url-p "https://google.com/test"))
  (should (my-url-p "ftp://google.com"))
  (should-not (my-url-p "Not Url!")))

(ert-deftest my-lib-check-alist-union
    ()
  (should
   (equal
    (my-alist-union
     '((a . 1)
       (b . 2))
     '((a . 2)
       (c . 3)))
    '((a . 2)
      (c . 3)
      (b . 2)))))

(ert-deftest my-lib-check-inc-filename
    ()
  (should (equal (my-inc-filename "a/b/1.txt") "a/b/2.txt")))

(provide 'my-lib-test)
;;; my-lib-test.el ends here
