;;; my-bench.el --- My a small package to provide a cool config benchmarks -*- lexical-binding: t; -*-

;; Copyright (C) 2024 semenInRussia

;;; Commentary:

;; My a small package to provide a cool config benchmarks.  Try to
;; call `my-require-times' it's print a table with load times of every
;; package

;;; Code:

(defvar my-require-times nil
  "A list of (FEATURE LOAD-START-TIME LOAD-DURATION).
LOAD-DURATION is the time taken in milliseconds to load FEATURE.")


(defun my-require-times-wrapper (orig feature &rest args)
  "Note in `my-require-times' the time taken to require each feature.

Pass FEATURE with ARGS to `require'.  ORIG is the original `require' function"
  (let* ((already-loaded (memq feature features))
         (require-start-time (and (not already-loaded) (current-time))))
    (prog1 (apply orig feature args)
      (when (and (not already-loaded) (memq feature features))
        (let ((time
               (* 1000.0 (float-time (time-subtract (current-time)
                                                    require-start-time)))))
          (push (list (intern (symbol-name feature)) require-start-time time)
                my-require-times))))))

(advice-add 'require :around 'my-require-times-wrapper)

(provide 'my-bench)
;;; my-bench.el ends here
