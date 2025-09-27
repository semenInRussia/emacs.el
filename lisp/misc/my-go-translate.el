;;; my-go-translate.el --- My config of Google translate

;; Copyright (C) 2022-2025 semenInRussia

;;; Commentary:

;; My config of Google translate

;;; Code:

(require 'my-leaf)
(require 'dash)

(declare-function google-translate-translate "google-translate")
(defun my-google-translate (text)
  "Translate the TEXT which is one of `my-translate-languages' to other."
  (interactive "sA text: ")
  (if (string-match-p "[абвгдежзийклмнопрстуфхцшщьъыэюя]" text) ;; russian text
      (google-translate-translate "ru" "en" text)
    (google-translate-translate "en" "ru" text)))

(leaf google-translate
  :ensure t
  :commands google-translate-translate
  :bind ((:embark-general-map
          :package embark
          ("T" . my-google-translate))
         ("C-x T" . my-google-translate)))

(provide 'my-go-translate)
;;; my-go-translate.el ends here
