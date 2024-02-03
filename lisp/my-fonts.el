;;; my-fonts.el --- My configuration for fonts

;; Copyright (C) 2022, 2023 Semen Khramtsov

;; Author: Semen Khramtsov <hrams205@gmail.com>
;; Version: 0.1
;; URL: https://github.com/semenInRussia/emacs.el

;;; Commentary:

;; My configuration for fonts

;;; Code:

;; you can install this font, from the GitHub repo `nerd-fonts'
(defcustom my-fonts-main
  "JetBrainsMonoNerdFont"
  "Name of the main font to display all."
  :group 'my
  :type 'string)

(defcustom my-fonts-size
  20
  "Size of font in editor."
  :group 'my
  :type 'number)

(setq line-spacing 0.2)

(push (cons 'font
            (format "%s-%s"
                    my-fonts-main
                    my-fonts-size))
      default-frame-alist)

(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-language-environment 'utf-8)
(set-selection-coding-system 'utf-8)

(provide 'my-fonts)
;;; my-fonts.el ends here
