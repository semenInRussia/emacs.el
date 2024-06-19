;;; my-yas-capf.el --- Completion at point function for `yasnippet' -*- lexical-binding: t -*-
;;; Commentary:
;; Introduce completion of existing `yasnippet' snippets keys.

;;; Code:
(require 'dash)
(require 'yasnippet)

(autoload 'cape--properties-table "cape")
(autoload 'cape--bounds "cape")
(autoload 'cape-interactive "cape")


(defvar my-yas--capf-properties
  (list :annotation-function (lambda (_) " Snippet ")
        :company-kind (lambda (_) 'snippet)
        :exit-function 'my-yas-capf--exit
        :company-docsig 'my-yas-capf--docsig
        :exclusive 'no)
  "Completion extra properties for `my-yas-capf'.")

;;;###autoload
(defun my-yas-capf (&optional interactive)
  "Completion at point for `yasnippet'.

If INTERACTIVE is true, show the completion where suggested only snippets."
  (interactive (list t))
  (if interactive
      (cape-interactive #'my-yas-capf)
    (when-let (snippets (yas-active-keys))
      (let ((bounds (cape--bounds 'symbol)))
        `(,(car bounds) ,(cdr bounds)
          ,(cape--properties-table snippets :category 'snippet)
          ,@my-yas--capf-properties)))))

(defun my-yas-capf--docsig (key)
  "Snippet content for `corfu' which show it in the echo area.

It takes the KEY (trigger) of snippet, because user type it and
`corfu' manipulate with it to show helpful things"
  (->
   ;; the first snippets table
   (yas--get-snippet-tables major-mode)
   car
   ;; fetch snippets with a given key
   (yas--fetch key)
   ;; choose the first
   car cdr
   ;; get its content
   yas--template-content))

(defun my-yas-capf--exit (name status)
  "Exit from `my-yas-capf'.

Use NAME and STATUS."
  (and name
       (eq status 'finished)
       (yas-expand)))

(provide 'my-yas-capf)
;;; my-yas-capf.el ends here
