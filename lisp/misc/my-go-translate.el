;;; my-go-translate.el --- My config `google-translate'

;; Copyright (C) 2022-2024 semenInRussia
;; Author: semenInRussia <hrams205@gmail.com>
;; Version: 0.1
;; URL: https://github.com/semenInRussia/emacs.el
;; This file is not part of GNU Emacs.

;;; Commentary:

;; My config `go-translate'

;;; Code:

(require 'my-leaf)
(require 'dash)

(declare-function google-translate-translate "google-translate")
(defun my-google-translate (text)
  "Translate the TEXT which is one of `my-translate-languages' to other."
  (if (string-match-p "[абвгдежзийклмнопрстуфхцшщьъыэюя]" text)  ;; russian text
      (google-translate-translate "ru" "en" text)
    (google-translate-translate "en" "ru" text)))

(leaf google-translate
  :ensure t
  :bind ((:embark-general-map
          :package embark
          ("T" . my-google-translate))))

(provide 'my-go-translate)
;;; my-go-translate.el ends here
