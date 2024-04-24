;;; my-buffer-navigation.el --- My config for navigation beetween buffers

;; Copyright (C) 2022 semenInRussia

;; Author: semenInRussia <hrams205@gmail.com>
;; Version: 0.1
;; URL: https://github.com/semenInRussia/emacs.el

;;; Commentary:

;; My config for navigation between buffers

;;; Code:
(require 'my-leaf)

(require 'dash)
(require 's)


(leaf avy
  :ensure (avy :repo "abo-abo/avy" :host github))

(leaf ace-window
  :ensure (ace-window :repo "abo-abo/ace-window"
                      :host github)
  :bind ("M-o" . ace-window))

(defun my-visit-last-opened-buffer ()
  "Visit buffer which was opened recently."
  (interactive)
  (switch-to-buffer (my-last-opened-buffer)))

(defun my-last-opened-buffer ()
  "Get buffer which was visited most recently."
  (--find
   (not (my--visit-last-opened-buffer-ignore-p it))
   (cdr (buffer-list))))

(defun my--visit-last-opened-buffer-ignore-p (buffer)
  "Take object of BUFFER and return nil when don't need visit its."
  (->> buffer (buffer-name) (s-trim) (s-prefix-p "*Minibuf")))

(leaf-keys
 ("C-<tab>" . 'my-visit-last-opened-buffer))

(provide 'my-buffer-navigation)
;;; my-buffer-navigation.el ends here
