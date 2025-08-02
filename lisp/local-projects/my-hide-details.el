;;; my-hide-details.el --- Functions to hide extra details in `compilation-mode' -*- lexical-binding: t -*-
;;; semenInRussia 2024-2025

;;; Commentary:
;; Functions to hide extra details in `compilation-mode'.  Hide
;; complation start time, status code, duration and etc.

;;; Code:
;;; hide details inside `compile' buffer

(defvar-local my-compilation-hide-details-p nil
  "Variable is non-nil if details of `compile' buffer was hiden.")

;;;###autoload
(defun my-compilation-toggle-hide-details ()
  "Hide or show details inside of `compile' buffer."
  (interactive)
  (if my-compilation-hide-details-p
      (my-compilation-show-details)
    (my-compilation-hide-details))
  (setq-local my-compilation-hide-details-p (not my-compilation-hide-details-p)))

(defun my-compilation-hide-details ()
  "Hide details like time at compile start inside `compile' buffer."
  (interactive)
  (narrow-to-region (progn
                      (goto-char (point-min))
                      (forward-line 4)
                      (point))
                    (progn
                      (goto-char (point-max))
                      (forward-line -2)
                      (point)))
  (goto-char (point-min)))

(defalias 'my-compilation-show-details #'widen
  "Show details like compile command start inside `compile' buffer.")

(provide 'my-hide-details)
;;; my-hide-details.el ends here
