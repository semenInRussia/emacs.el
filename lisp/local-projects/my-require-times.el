;;; my-require-times.el --- A mode to see time which every Emacs module took to load -*- lexical-binding: t -*-
;; semenInRussia 2024-2025

;;; Commentary:
;; A mode to see time which every Emacs module took to load

;;; Code:
(require 'cl-lib)

(defvar my-require-times nil
  "A list of (FEATURE LOAD-START-TIME LOAD-DURATION).
LOAD-DURATION is the time taken in milliseconds to load FEATURE.")

;;;###autoload
(defun my-require-times ()
  "Show a tabular view of how long various libraries took to load."
  (interactive)
  (with-current-buffer (get-buffer-create "*Require Times*")
    (my-require-times-mode)
    (tabulated-list-revert)
    (display-buffer (current-buffer))))

(defun my-time-subtract-millis (b a)
  "Subtract two time structures: A and B and return milliseconds."
  (* 1000.0 (float-time (time-subtract b a))))

;;;###autoload
(define-derived-mode my-require-times-mode tabulated-list-mode "Require-Times"
  "Show times taken to `require' packages."
  (setq tabulated-list-format
        [("Start time (ms)" 20 my-require-times-sort-by-start-time-pred)
         ("Feature" 30 t)
         ("Time (ms)" 12 my-require-times-sort-by-load-time-pred)])
  (setq tabulated-list-sort-key (cons "Start time (ms)" nil))
  (setq tabulated-list-padding 2)
  (setq tabulated-list-entries #'my-require-times-tabulated-list-entries)
  ;; it enable M-. to jump to Emacs packages
  (setq-local xref-backend-functions '(elisp--xref-backend t))
  (tabulated-list-init-header)
  (when (fboundp 'tablist-minor-mode)
    (tablist-minor-mode)))

(defun my-require-times-sort-by-start-time-pred (entry1 entry2)
  (< (string-to-number (elt (nth 1 entry1) 0))
     (string-to-number (elt (nth 1 entry2) 0))))

(defun my-require-times-sort-by-load-time-pred (entry1 entry2)
  (> (string-to-number (elt (nth 1 entry1) 2))
     (string-to-number (elt (nth 1 entry2) 2))))

(defun my-require-times-tabulated-list-entries ()
  (cl-loop for (feature start-time millis) in my-require-times
           with order = 0
           do (cl-incf order)
           collect (list order
                         (vector
                          (format "%.3f" (my-time-subtract-millis start-time before-init-time))
                          (symbol-name feature)
                          (format "%.3f" millis)))))

(provide 'my-require-times)
;;; my-require-times.el ends here
