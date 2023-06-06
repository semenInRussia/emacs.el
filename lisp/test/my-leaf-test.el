;;; my-leaf-test.el --- Tests for `my-leaf'

;; Copyright (C) 2022 Semen Khramtsov

;; Author: Semen Khramtsov <hrams205@gmail.com>
;; Version: 0.1

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

;; Tests for `my-leaf'

;;; Code:

(require 'ert)

(defmacro should-leaf-macroexpand (form expected)
  "Ensure that macroexpanded FORM is equal to EXPECTED."
  `(let ((leaf-expand-minimally t)
         (leaf-expand-leaf-protect nil)
         (leaf-expand-leaf-defun nil)
         (leaf-expand-leaf-defvar nil)
         (leaf-expand-leaf-path nil))
     (should
      (equal (macroexpand-1 ',form) ',expected))))

(ert-deftest my-leaf-check-aas-keyword
    ()
  (should-leaf-macroexpand
   (leaf leaf :aas ("pr" "print"))
   (prog1 'leaf
     (eval-after-load 'aas '(aas-set-snippets 'leaf "pr" "print")))))

(ert-deftest my-leaf-check-aas-keyword-with-special-keymap
    ()
  (should-leaf-macroexpand
   (leaf leaf :aas (org "pr" "print"))
   (prog1 'leaf
     (eval-after-load 'aas '(aas-set-snippets 'org "pr" "print")))))

(ert-deftest my-leaf-check-aas-keyword-with-some-bindings
    ()
  (should-leaf-macroexpand
   (leaf leaf
     :aas ("pr" "print"
           "mes" "message"
           "lg" "log"
           "lst" "list"
           "beg" "begin"))
   (prog1 'leaf
     (eval-after-load 'aas
       '(aas-set-snippets 'leaf
          "pr" "print"
          "mes" "message"
          "lg" "log"
          "lst" "list"
          "beg" "begin")))))

(ert-deftest my-leaf-check-aas-keyword-with-cond
    ()
  (should-leaf-macroexpand
   (leaf leaf
     :aas (:cond 'texmathp
                 "pr" "print"
                 "mes" "message"
                 "lg" "log"
                 "lst" "list"
                 "beg" "begin"))
   (prog1 'leaf
     (eval-after-load 'aas
       '(aas-set-snippets 'leaf
          :cond 'texmathp
          "pr" "print"
          "mes" "message"
          "lg" "log"
          "lst" "list"
          "beg" "begin")))))

(ert-deftest my-leaf-check-fast-exec-keyword
    ()
  (should-leaf-macroexpand
   (leaf leaf :fast-exec ("Pretty Print Current Buffer" 'pp-buffer))
   (prog1 'leaf
     (unless (fboundp 'quote) (autoload #'quote "leaf" nil t))
     (unless (fboundp 'pp-buffer)
       (autoload #'pp-buffer "leaf" nil t))
     (eval-after-load 'fast-exec
       '(fast-exec-bind 'leaf
          (fast-exec-make-some-commands
           ("Pretty Print Current Buffer" 'pp-buffer)))))))

(ert-deftest my-leaf-check-fast-exec-keyword-some-bindings
    ()
  (should-leaf-macroexpand
   (leaf leaf
     :fast-exec (("Pretty Macro Expand Last Expression"
                  'pp-macroexpand-last-sexp)
                 ("Pretty Print Current Buffer" 'pp-buffer)))
   (prog1 'leaf
     (unless (fboundp 'quote) (autoload #'quote "leaf" nil t))
     (unless (fboundp 'pp-macroexpand-last-sexp)
       (autoload #'pp-macroexpand-last-sexp "leaf" nil t))
     (unless (fboundp 'pp-buffer)
       (autoload #'pp-buffer "leaf" nil t))
     (eval-after-load 'fast-exec
       '(fast-exec-bind 'leaf
          (fast-exec-make-some-commands
           ("Pretty Macro Expand Last Expression" 'pp-macroexpand-last-sexp)
           ("Pretty Print Current Buffer" 'pp-buffer)))))))

(ert-deftest my-leaf-check-major-mode-map-keyword
    ()
  (should-leaf-macroexpand
   (leaf leaf                           ;nofmt
     :major-mode-map (leaf
                       :parent parent-map
                       :modes (leaf-mode leaf-interactive-mode)))
   (prog1 'leaf
     (eval-after-load 'xah-fly-keys
       '(my-define-local-major-mode-map 'leaf
                                        '(leaf-mode leaf-interactive-mode)
                                        'parent-map)))))

(ert-deftest my-leaf-check-major-mode-map-keyword-without-modes
    ()
  (should-leaf-macroexpand
   (leaf leaf                           ;nofmt
     :major-mode-map (leaf :parent parent-map))
   (prog1 'leaf
     (eval-after-load 'xah-fly-keys
       '(my-define-local-major-mode-map 'leaf '(leaf) 'parent-map)))))

(ert-deftest my-leaf-check-major-mode-map-keyword-without-keywords
    ()
  (should-leaf-macroexpand
   (leaf leaf                           ;nofmt
     :major-mode-map (leaf (leaf-mode leaf-interactive-mode)))
   (prog1 'leaf
     (eval-after-load 'xah-fly-keys
       '(my-define-local-major-mode-map 'leaf
                                        '(leaf-mode leaf-interactive-mode)
                                        'nil)))))

(ert-deftest my-leaf-check-major-mode-with-only-name
    ()
  (should-leaf-macroexpand
   (leaf markdown-mode :major-mode-map markdown)
   (prog1 'markdown-mode
     (eval-after-load 'xah-fly-keys
       '(my-define-local-major-mode-map 'markdown
                                        '(markdown-mode)
                                        'nil)))))

(ert-deftest my-leaf-check-major-mode-with-default-values
    ()
  (should-leaf-macroexpand
   (leaf markdown-mode :major-mode-map t)
   (prog1 'markdown-mode
     (eval-after-load 'xah-fly-keys
       '(my-define-local-major-mode-map 'markdown-mode
                                        '(markdown-mode)
                                        'nil)))))

(ert-deftest my-leaf-check-major-mode-with-special-order
    ()
  (should-leaf-macroexpand
   (leaf markdown-mode
     :major-mode-map t
     :leaf-autoload nil
     :bind ("M-RET" . markdown-insert-list-item))
   (prog1 'markdown-mode
     (eval-after-load 'xah-fly-keys
       '(my-define-local-major-mode-map 'markdown-mode
                                        '(markdown-mode)
                                        'nil))
     (leaf-keys (("M-RET" . markdown-insert-list-item))))))

(provide 'my-leaf-test)
;;; my-leaf-test.el ends here
