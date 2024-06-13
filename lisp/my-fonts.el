;;; my-fonts.el --- My configuration for fonts

;; Copyright (C) 2022, 2023 semenInRussia

;; Author: semenInRussia <hrams205@gmail.com>
;; Version: 0.1
;; URL: https://github.com/semenInRussia/emacs.el

;;; Commentary:

;; My configuration for fonts

;;; Code:

(require 'my-leaf)

(defcustom my-fonts-main
  '("JetBrainsMono Nerd Font"
    "JetBrainsMono"
    "FiraCode Nerd Font"
    "FiraCode"
    "Cascadia Code Nerd Font"
    "Cascadia Code"
    "Cascadia Code NF")
  "Name of the main font to display all."
  :group 'my
  :type 'string)

(defcustom my-fonts-size 20
  "Size of font in editor."
  :group 'my
  :type 'number)

(defun font-installed-p (name)
  "Return non-nil if the font with the name NAME is exist."
  (find-font (font-spec :name name)))

(unless (assoc 'font default-frame-alist)
  (let ((fonts my-fonts-main))
    (while fonts
      (when (font-installed-p (car fonts))
        (push (cons 'font (format "%s-%s" (car fonts) my-fonts-size))
              default-frame-alist)
        (setq fonts nil))
      (setq fonts (cdr fonts)))))

(leaf nerd-icons
  :ensure t
  :require t
  :config
  (when (and (display-graphic-p)
             (not (font-installed-p nerd-icons-font-family)))
    (nerd-icons-install-fonts t)))

(setq-default line-spacing 0.30)

;;; UTF-8
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-language-environment 'utf-8)
(set-selection-coding-system 'utf-8)

(leaf ligature
  :ensure t
  :require t
  :commands ligature-set-ligatures global-ligature-mode
  :config
  (ligature-set-ligatures 'prog-mode '("--" "---" "==" "===" "!="
                                       "!==" "=!=" "=:=" "=/=" "<="
                                       ">=" "&&" "&&&" "&=" "++" "+++"
                                       "***" ";;" "!!"  "??" "???"
                                       "?:" "?." "?=" "<:" ":<" ":>"
                                       ">:" "<:<" "<>" "<<<" ">>>"
                                       "<<" ">>" "||" "-|" "_|_" "|-"
                                       "||-" "|=" "||=" "##" "###"
                                       "####" "#{" "#[" "]#" "#(" "#?"
                                       "#_" "#_(" "#:" "#!" "#=" "^="
                                       "<$>" "<$" "$>" "<+>" "<+" "+>"
                                       "<*>" "<*" "*>" "</" "</>" "/>"
                                       "<!--" "<#--" "-->" "->" "->>"
                                       "<<-" "<-" "<=<" "=<<" "<<="
                                       "<==" "<=>" "<==>" "==>" "=>"
                                       "=>>" ">=>" ">>=" ">>-" ">-"
                                       "-<" "-<<" ">->" "<-<" "<-|"
                                       "<=|" "|=>" "|->" "<->" "<~~"
                                       "<~" "<~>" "~~" "~~>" "~>" "~-"
                                       "-~" "~@" "[||]" "|]" "[|" "|}"
                                       "{|" "[<" ">]" "|>" "<|" "||>"
                                       "<||" "|||>" "<|||" "<|>" "..."
                                       ".." ".=" "..<" ".?" "::" ":::"
                                       ":=" "::=" ":?" ":?>" "//"
                                       "///" "/*" "*/" "/=" "//="
                                       "/==" "@_" "__" "???"  "<:<"
                                       ";;;"))
  (global-ligature-mode t))

(provide 'my-fonts)
;;; my-fonts.el ends here
