;;; my-pandoc.el --- My config for the the pandoc
;; Copyright (C) 2022-2024 Semen Khramtsov

;;; Commentary:

;; My config for the tool pandoc

;;; Code:

(require 'my-leaf)
(require 'dash)
(require 'f)
(require 's)


(defun my-pandoc-tex-to-documents-dir ()
  "Move all .docx files in working dir to directroy documents."
  (f-mkdir "documents")
  (->>
   (file-expand-wildcards "*.tex")
   (-map 'f-base)
   (--map
    (s-lex-format
     "pandoc -t docx -f latex -o documents/${it}.docx ${it}.tex"))
   (mapc #'shell-command)))

(provide 'my-pandoc)
;;; my-pandoc.el ends here
