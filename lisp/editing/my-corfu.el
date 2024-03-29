;;; my-corfu.el.el --- My configuration of `corfu' -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2024 semenInRussia

;; Author: semenInRussia <hrams205@gmail.com>
;; Version: 0.1
;; Homepage: https://github.com/semeninrussia/emacs.el

;;; Commentary:

;; My configuration of `corfu'.  I choose `corfu' over `company'
;; because `company' have a big load time (about 9 secs on my old
;; windows computer) while `corfu' some milliseconds

;;; Code:

(require 'my-leaf)
(require 'f)   ; for `f-full'

(leaf corfu
  :ensure (corfu
           :repo "minad/corfu"
           :files ("*.el" "extensions/*"))
  ;; so when auto-completion is provided, load `corfu'
  ;;
  ;; I load `corfu' only when it really needed.  It's awesome idea,
  ;; until `corfu' don't take some seconds before load, it make Emacs
  ;; a little worth, but with `my-use-afk' it's still cool
  :commands corfu--in-region global-corfu-mode
  :init
  (--each '(prog-mode-hook text-mode-hook)
    (add-hook it
              (defun my-corfu-set ()
                "Set some vars for corfu."
                (interactive)
                (setq-local completion-in-region-function 'corfu--in-region))))
  ;; `completion-in-region-function' was already changed, but
  ;; `global-corfu-mode' enable auto complete, if `corfu-auto' is
  ;; non-nil, call `corfu-mode' to enable corfu locall in this buffer
  :config (global-corfu-mode t) (corfu-mode t)
  :custom (;; by default 2 but 1 one is better
           (corfu-auto-prefix . 1)
           ;; by default to run `corfu' you should press `C-M-i'
           (corfu-auto . t)
           ;; I don't like 0sec, because it bad for yasnippets
           (corfu-auto-delay . 0.3)
           ;; when `line-spacing' changed, the default `corfu-count' (10) is bad
           (corfu-count . 7))
  :config
  ;; show documentation of every auto-completion item
  (leaf corfu-popupinfo
    :global-minor-mode t
    :bind (:corfu-map
           :package corfu
           ("C-h" . 'corfu-popupinfo-toggle)))

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
             (kind-icon--unknown . "  ")
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
  ;; I'm using the file from the following GitHub repository:
  ;; https://github.com/dwyl/english-words/
  ;;
  ;; wget https://raw.githubusercontent.com/dwyl/english-words/master/words.txt
  :custom `(cape-dict-file .
                           ,(if (eq system-type 'windows-nt)
                                (f-full (locate-user-emacs-file "dict/english.txt"))
                              "/usr/share/dict/words.txt"))
  :commands corfu-mode
  :defun (;; some capfs provided by `cape'
          cape-symbol
          cape-dabbrev cape-file cape-elisp-block cape-history cape-dict
          cape-keyword cape-sgml cape-tex cape-abbrev cape-symbol
          ;; some `cape' internals
          cape-capf-silent cape-capf-super)
  :hook (corfu-mode-hook . my-capf-local-setup)
  :config
  ;; make that `completion-at-point-functions' will be the different for every buffer
  (make-local-variable 'completion-at-point-functions)

  ;; don't use the following
  ;; I prefer built-in `elisp-completion-at-point'
  ;;   (add-hook 'completion-at-point-functions #'cape-symbol)
  ;; really strange things, IDK what is it (know but it hard)
  ;;   (add-hook 'completion-at-point-functions #'cape-sgml)
  ;;   (add-hook 'completion-at-point-functions #'cape-rfc1345)
  ;;   (add-hook 'completion-at-point-functions #'cape-tex)
  ;; enrage!!
  ;;   (add-hook 'completion-at-point-functions #'cape-line)

  (defun my-capf-local-setup ()
    "Change the `completion-at-point-functions' for current buffer."
    ;; Here the numbers is the depth of the respective hooks, the
    ;; default depth is 0, so if `major-mode' provides a capf
    ;; function, then its depth is 0 (in 100% cases which I saw).
    ;;
    ;; For `cape' hooks I define the depth 20, because I need that the
    ;; capf provided by major-mode (or lsp client) will be prefered
    ;; over `cape' capfs.
    ;;
    ;; Notice that here order is important, first will checks the
    ;; first added hooks, so it starts from `cape-history', then
    ;; `cape-elisp-block' and the last is `cape-dict'.
    ;;
    ;; In `yasnippet' capf config (defined in `my-yas' file) I add
    ;; capf for yasnippet I add to it depth 10, I prefer yasnippets
    ;; over `cape'
    (add-hook 'completion-at-point-functions #'cape-history     20 'local)
    (add-hook 'completion-at-point-functions #'cape-elisp-block 20 'local)
    (add-hook 'completion-at-point-functions #'cape-file        20 'local)
    (add-hook 'completion-at-point-functions #'cape-keyword     20 'local)
    ;; `my-capf-word' = `cape-dict' + `cape-dabbrev'
    (add-hook 'completion-at-point-functions 'my-capf-word     20 'local))

  (defalias 'my-capf-word (cape-capf-silent
                           (cape-capf-super
                            #'cape-dict
                            #'cape-dabbrev))
    "Complete word from dictionary + some words in the opened buffers.

It compose two functions from `cape': `cape-dict' and `cape-dabbrev', but here I
try to do complete without saving buffer and other extra messages.  Also it more
useful, because usually if you need in a dictionary completion than you also can
try `dabbrev' before.

If you call it interactively or INTERACTIVE is non-nil value, then it show
auto-completion popup with this capf")

  ;; Emacs have capf provided by `ispell' which is looks like `cape-dict', I
  ;; prefer a `cape' one, so disable `ispell'
  (advice-add 'ispell-complete-word :override 'ignore))

(provide 'my-corfu)
;;; my-corfu.el ends here
