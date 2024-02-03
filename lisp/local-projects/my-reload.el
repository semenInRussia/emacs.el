;;; my-reload.el --- Load Emacs config when edit it and save -*- lexical-binding: t -*-

;; Copyright (C) 2024 by semenInRussia

;;; Commentary:

;; Load Emacs config when edit it and save.  NOTE: that if load config
;; here is mean to run `eval-buffer'.

;; NOTE: that not all config files need in it, so you can disable it
;; locally changing buffer local variable `my-dont-reload-on-save' to
;; non-nil value

;;; Code:

(defcustom my-reload-config-directory
  "~/.config/emacs/"
  "Path to the dir where located config files on which it will work."
  :group 'my
  :type 'string)

(defvar-local my-dont-reload-on-save nil
  "Non-nil if you don't need in `my-reload' feature for this file.")

;;;###autoload
(define-minor-mode my-reload-mode
  "Mode which will eval/load the current config file after save."
  :init-value t
  (if (and my-reload-mode (my-reload--config-file-p)
           (not my-dont-reload-on-save))
      (add-hook 'after-save-hook #'eval-buffer
                nil :local)
    (remove-hook 'after-save-hook #'eval-buffer
                 :local)))

(defun my-reload--config-file-p ()
  "Return non-nil when the current opened file is a config file."
  (and
   (buffer-file-name)
   (string-prefix-p (expand-file-name my-reload-config-directory)
                    (expand-file-name default-directory))))

(provide 'my-reload)
;;; my-reload.el ends here
