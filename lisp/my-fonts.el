;;; my-fonts.el --- My configuration for fonts

;; Copyright (C) 2022-2024 semenInRussia

;; Author: semenInRussia <hrams205@gmail.com>
;; Version: 0.1
;; URL: https://github.com/semenInRussia/emacs.el

;;; Commentary:

;; My configuration for fonts

;;; Code:

(require 'my-leaf)
(require 'cl-lib)

(defcustom my-fonts-main
  '("JetBrainsMono"
    "JetBrainsMono Nerd Font"
    "FiraCode"
    "FiraCode Nerd Font"
    "Cascadia Code"
    "Cascadia Code Nerd Font"
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
  (let ((font (cl-find-if #'font-installed-p my-fonts-main)))
    (setf (alist-get 'font default-frame-alist)
          (format "%s-%s" font my-fonts-size))))

(leaf nerd-icons
  :ensure t
  :require t
  :config
  (when (and (display-graphic-p)
             (not (font-installed-p nerd-icons-font-family)))
    (nerd-icons-install-fonts t)))

(setq-default line-spacing 0.3)

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
