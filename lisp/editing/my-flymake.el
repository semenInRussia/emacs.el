;;; my-flymake.el --- my `flymake' settings -*- lexical-binding: t -*-
;;; Commentary:

;; My settings for `flymake'.  `flymake' is built-in solution to
;; highlight errors and warnings (diagnostics) in the buffer

;; `flymake' is choice of `eglot', but now `flycheck' is more popular

;;; Code:

(require 'my-leaf)


(defun my-flymake-embark-eglot-code-actions (diag)
  "A wrapper over `eglot-code-actions' for `embark-flymake-map'.

Use object DIAG which is captured with one of `embark-target-finders'"
  (ignore diag)
  (call-interactively #'eglot-code-actions))

(leaf flymake
  :bind ((:embark-flymake-map
          :package embark
          ("f" . my-flymake-embark-eglot-code-actions))
         (:flymake-mode-map
          ([remap next-error] . 'flymake-goto-next-error)
          ([remap previous-error] . 'flymake-goto-prev-error))))

;; change the default behaviour of `embark' with `flymake'.
;;
;; `embark' when see a flyamake diagnostic, find a text which is
;; highlighted, I prefer when the error message is found text
;;
;; see `my-flycheck' for simple showcases
(leaf embark
  :after flymake
  :config
  (advice-add
   'embark-target-flymake-at-point
   :around
   (defun my-embark-flymake-target (&rest args)
     "Advice over `embark' which change the default behavior and pass args."
     (let* ((res (apply args))
            (beg (car (last res)))
            (end (cdr (last res))))
       (when res
         (setf (nth 1 res)
               (seq-mapcat
                #'flymake--diag-text
                (flymake-diagnostics beg end)
                'string)))
       res))))

(provide 'my-flymake)
;;; my-flymake.el ends here
