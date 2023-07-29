;;; my-corfu.el.el --- My configuration of `corfu' -*- lexical-binding: t; -*-

;; Copyright (C) 2023 semenInRussia

;; Author: semenInRussia <hrams205@gmail.com>
;; Version: 0.1
;; Homepage: https://github.com/semeninrussia/emacs.el

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; My configuration of `corfu'.  I choose `corfu' over `company' because
;; `company' have a big load time (about 9 secs on my computer) while `corfu'
;; 3secs

;;; Code:

(require 'my-leaf)
(require 'f)   ; for `f-full'


(leaf compat
  :ensure (compat :repo "emacs-straight/compat" :host github))

(leaf corfu
  :ensure (corfu
           :repo "minad/corfu"
           :files ("*.el" "extensions/*"))
  ;; so when auto-completion is provided, load `corfu'
  ;;
  ;; I load `corfu' only when it really needed.  It's awesome idea, until
  ;; `corfu' don't take some seconds before load, it make Emacs a little worth,
  ;; but with `my-use-afk' it's still cool
  :commands corfu--in-region
  :init (setq-default completion-in-region-function 'corfu--in-region)
  ;; make border of auto-completion minibuffer white/black, it looks like nice
  ;; :custom-face ((corfu-border . '((t :background "black"))))
  :custom (;; by default 2 but 1 one is better
           (corfu-auto-prefix . 1)
           ;; by default to run `corfu' you should press `C-M-i'
           (corfu-auto . t)
           ;; I don't like 0sec, because it bad for yasnippets
           (corfu-auto-delay . 0.4))
  :config
  ;; `completion-in-region-function' was already changed, but
  ;; `global-corfu-mode' enable auto complete, if `corfu-auto' is non-nil
  (global-corfu-mode t)

  ;; show documentation of every auto-completion item
  (leaf corfu-popupinfo
    :bind (:corfu-map
           :package corfu
           ("M-i" . 'corfu-popupinfo-toggle)))

  ;; show icons inside auto-completion popup
  (leaf kind-icon
    :ensure (kind-icon :repo "emacs-straight/kind-icon"
                       :host github)
    :after corfu
    :commands kind-icon-margin-formatter
    :defvar corfu-margin-formatters
    :custom ((kind-icon-use-icons . t)
             ;; show the icons with the white or other theme background color
             (kind-icon-default-face . 'corfu-default)
             (kind-icon-blend-background . nil)
             ;; when an icons isn't known show the completion without icon
             ;;
             ;; (default is to show ??? with the red background)
             (kind-icon--unknown . " ")
             ;; use the same as a symbol size for icons
             (kind-icon-default-style . `(
                                          :padding 0
                                          :stroke 0
                                          :margin 0
                                          :radius 0
                                          :height 0.5
                                          :scale 1)))
    :init (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter)))

(leaf cape
  :ensure (cape :repo "minad/cape" :host github)
  :require t
  ;; I'm using the file from the following GitHub repository
  ;; https://github.com/dwyl/english-words/
  :custom `(cape-dict-file . ,(f-full "~/.emacs.d/dict/english.txt"))
  :defun (cape-symbol
          cape-dabbrev cape-file cape-elisp-block cape-history
          cape-keyword cape-sgml cape-tex cape-abbrev cape-symbol)
  :hook (corfu-mode-hook . my-capf-local-setup)
  :config
  ;; make that `completion-at-point-functions' will be the different for every buffer
  (make-local-variable 'completion-at-point-functions)

  ;; don't use the following
  ;; LaTeX commands, I prefer built-in AuCTex capfs
  ;;   (add-hook 'completion-at-point-functions #'cape-tex)
  ;; I prefer built-in `elisp-completion-at-point'
  ;;   (add-hook 'completion-at-point-functions #'cape-symbol)
  ;; really strange thing, IDK what is it (know but it hard)
  ;;   (add-hook 'completion-at-point-functions #'cape-sgml)
  ;;   (add-hook 'completion-at-point-functions #'cape-rfc1345)
  ;; enrage!!
  ;;   (add-hook 'completion-at-point-functions #'cape-line)

  (defun my-capf-local-setup ()
    "Change the `completion-at-point-functions' for current buffer."
    ;; Here the numbers is the depth of the respective hooks, the default depth is
    ;; 0, so if `major-mode' provides a capf function, then its depth is 0 (in
    ;; 100% cases which I saw)
    ;;
    ;; If i want that one thing will be prefer over another, than set to more
    ;; important one the number with the LESSER number, so if the depth it will
    ;; be the first function which was checked, here I use the depth from 0 to
    ;; the amount of hooks
    (add-hook 'completion-at-point-functions #'cape-history     1 'local)
    (add-hook 'completion-at-point-functions #'cape-elisp-block 2 'local)
    (add-hook 'completion-at-point-functions #'cape-file        3 'local)
    (add-hook 'completion-at-point-functions #'cape-keyword     4 'local)
    (add-hook 'completion-at-point-functions #'cape-dabbrev     5 'local)
    (add-hook 'completion-at-point-functions #'cape-abbrev      6 'local)
    (add-hook 'completion-at-point-functions #'cape-dict        7 'local)))

(provide 'my-corfu)
;;; my-corfu.el ends here
