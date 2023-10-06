;;; my-fonts.el --- My configuration for fonts

;; Copyright (C) 2022, 2023 Semen Khramtsov

;; Author: Semen Khramtsov <hrams205@gmail.com>
;; Version: 0.1
;; URL: https://github.com/semenInRussia/emacs.el

;;; Commentary:

;; My configuration for fonts

;;; Code:

(require 'my-leaf)
(require 'dash)

;; you can install this font, from the GitHub repo `nerd-fonts'
(defcustom my-fonts-main
  "JetBrainsMonoNerdFont"
  "Name of the main font to display all."
  :group 'my
  :type 'string)

(defcustom my-font-size
  20
  "Name of the main font to display all."
  :group 'my
  :type 'number)

(set-face-attribute 'default nil
                    :height (* my-font-size 10)
                    :family my-fonts-main)


(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-language-environment 'utf-8)
(set-selection-coding-system 'utf-8)

(provide 'my-fonts)
;;; my-fonts.el ends here
