;;; my-window-navigation.el --- My config for navigation beetween buffers
;; Copyright (C) 2022-2024 semenInRussia
;; Author: semenInRussia <hrams205@gmail.com>

;;; Commentary:
;; My config for navigation between buffers.
;;
;; check this article of @karthinks
;; https://karthinks.com/software/emacs-window-management-almanac/
;;
;; Tutorial how to navigate windows
;;
;; Termins
;; - Buffer is a thing which have the text, name and almost usually
;;   have a file path
;; - Frame is separated floating window, you can move it, using your
;;   OS things
;; - Window is part of current frame which is shown now at the top.
;;   if you see only one file, then you see one window.
;;
;; How to use:
;;
;; 1. Change the current(active) window with M-o
;; 2. Close the current(active) window M-0(zero)
;; 3. Split horizontally with hit C-x 3 and choose needed buffer
;;    when I choose `consult-buffer' will be called, if I need other
;;    command (like `find-file'), I can hit > to which is
;;    `embark-become'
;; 4. Split vertically with ... C-x 2 ...
;; 5. Make a window frame (for fun), with C-x f
;; 6. Also you can change the buffer of current window with C-x C-n
;;    and C-x C-p
;; 7. If you are `meow' user, try q to either kill window, change
;;    buffer to other
;; 8. If I need to open anything like documentation
;;   (`describe-variable') I press "C-x O", after choose command "C-h
;;   v", after choose the place where this documentation buffer will
;;   be opened

;;; Code:
(require 'my-leaf)
(require 'dash)
(require 's)

(leaf ace-window
  :ensure t
  :custom (aw-scope . 'frame)
  :bind ("M-o" . ace-window))

(declare-function my-split-below "my-window-funcs")
(declare-function my-split-right "my-window-funcs")

(with-eval-after-load 'embark
  (defvar embark-become-file+buffer-map)
  (keymap-set embark-become-file+buffer-map "2" #'my-split-below)
  (keymap-set embark-become-file+buffer-map "3" #'my-split-right))

(defvar-keymap my-prev-next-buf-map
  :repeat (:enter (next-buffer previous-buffer))
  "n" #'next-buffer
  "p" #'previous-buffer)

(leaf-keys
 (("C-<tab>" . 'my-visit-last-opened-buffer)
  ;; Fast select buffers
  ("C-x C-p" . previous-buffer)
  ("C-x C-n" . next-buffer)

  ;; split
  ([remap split-window-right] . my-split-right) ;; C-x 3
  ([remap split-window-below] . my-split-below) ;; C-x 2

  ;; Close window
  ("M-0" . my-delete-window-frame)

  ;; Make window separately frame
  ("C-x f" . my-buffer-to-frame-floating)

  ("C-;" . ace-window-one-command)
  ("C-x O" . ace-window-prefix)))

;;; Rules to show dome types of windows

(defvar my/occur-grep-modes-list '(occur-mode
                                   grep-mode
                                   xref--xref-buffer-mode
                                   locate-mode
                                   flymake-diagnostics-buffer-mode
                                   rg-mode)
  "List of major-modes used in occur-type buffers.")

;; This does not work at buffer creation since the major-mode for
;; REPLs is not yet set when `display-buffer' is called, but is
;; useful afterwards
(defvar my/repl-modes-list '(matlab-shell-mode
                             eshell-mode
                             geiser-repl-mode
                             shell-mode
                             eat-mode
                             ;; vterm-mode
                             inferior-python-mode
                             cider-repl-mode
                             fennel-repl-mode
                             jupyter-repl-mode
                             inferior-ess-julia-mode)
  "List of major-modes used in REPL buffers.")

(defvar my/repl-names-list
  '("^\\*\\(?:.*?-\\)\\{0,1\\}e*shell[^z-a]*\\(?:\\*\\|<[[:digit:]]+>\\)$"
    "\\*.*REPL.*\\*"
    "\\*Python\\*"
    "^\\*jupyter-repl.*?\\(\\*\\|<[[:digit:]]>\\)$"
    "\\*Inferior .*\\*$"
    "^\\*cider-repl.*\\*$"
    "\\*ielm\\*"
    "\\*edebug\\*")
  "List of buffer names used in REPL buffers.")

(defvar my/help-modes-list '(helpful-mode
                             help-mode
                             pydoc-mode
                             eldoc-mode
                             TeX-special-mode)
  "List of major-modes used in documentation buffers.")

(defvar my/man-modes-list '(Man-mode woman-mode)
  "List of major-modes used in Man-type buffers.")

(defvar my/message-modes-list '(compilation-mode
                                edebug-eval-mode)
  "List of major-modes used in message buffers.")

(defun buffer-mode (&optional buffer-or-name)
  "Returns the major mode associated with a buffer.
If buffer-or-name is nil return current buffer's mode."
  (buffer-local-value 'major-mode
                      (if buffer-or-name
                          (get-buffer buffer-or-name)
                        (current-buffer))))

(setq display-buffer-alist
      '(

        ("^\\*[Ee]shell [Ee]xport: .*\\*$"
         (display-buffer-reuse-window display-buffer-use-some-window))

        ;; ----------------------------------------------------------------
        ;; Windows on top
        ;; ----------------------------------------------------------------

        ("\\*\\(?:Org Select\\|Agenda Commands\\)\\*"
         (display-buffer-below-selected
          display-buffer-in-side-window)
         (body-function . select-window)
         (window-height . (lambda (win) (fit-window-to-buffer win nil 12)))
         (side . top)
         (slot . -2)
         (preserve-size . (nil . t))
         (window-parameters . ((mode-line-format . nil))))

        ("\\*Buffer List\\*" (display-buffer-in-side-window)
         (side . top)
         (slot . 0)
         (window-height . shrink-window-if-larger-than-buffer))

        ((lambda (buf act) (member (buffer-mode buf) my/occur-grep-modes-list))
         (display-buffer-reuse-mode-window
          display-buffer-in-direction
          display-buffer-in-side-window)
         (side . top)
         (slot . 5)
         (window-height . (lambda (win) (fit-window-to-buffer win 20 10)))
         (direction . above)
         (body-function . select-window))

        ("\\*\\(Flycheck\\|Package-Lint\\).*"
         (display-buffer-in-direction display-buffer-in-side-window)
         (direction . above)
         (window-height . shrink-window-if-larger-than-buffer)
         ;; (window-height . 0.16)
         (side . top)
         (slot . 1)
         (window-parameters . (;; (mode-line-format . (:eval (my/helper-window-mode-line-format)))
                               (no-other-window . t))))

        ;; ----------------------------------------------------------------
        ;; Windows on the side
        ;; ----------------------------------------------------------------

        ;; ((lambda (buf act) (member (buffer-mode buf) my/man-modes-list))
        ;;  ;; "^\\*\\(?:Wo\\)?Man"
        ;;  (display-buffer-in-side-window)
        ;;  (body-function . select-window)
        ;;  (window-width . 76)       ; See the :hook
        ;;  (side . left)
        ;;  (slot . 9))

        ("\\*Faces\\*" (display-buffer-in-side-window)
         (window-width . 0.25)
         (side . right)
         (slot . -2)
         (window-parameters . ((no-other-window . t)
                               ;; (mode-line-format . (:eval (my/helper-window-mode-line-format)))
                               )))

        ;; ((lambda (buf act) (or (equal (buffer-mode buf) 'Custom-mode)
        ;;                   (string-match-p "^\\*Customize" (buffer-name))))
        ;;  (display-buffer-in-side-window)
        ;;  (body-function . select-window)
        ;;  (window-width . 74)
        ;;  (side . right)
        ;;  (slot . 5))

        ("\\*undo-tree\\*" ;; (lambda (buf act) (equal (buffer-mode buf) 'undo-tree-visualizer-mode))
         (display-buffer-in-direction)
         (window-width . 35) ;; (lambda (win) (fit-window-to-buffer win nil nil 65 40 t)))
         (direction . right)
         (side . right)
         (slot . -5))

        ;; ----------------------------------------------------------------
        ;; Windows at the bottom
        ;; ----------------------------------------------------------------

        ("\\*Backtrace\\*" (display-buffer-in-side-window)
         (window-height . 0.20)
         (side . bottom)
         (slot . -9)
         ;; (preserve-size . (nil . t))
         ;; (window-parameters . (;; (mode-line-format . (:eval (my/helper-window-mode-line-format)))
         ;;                       ))
         )

        ("\\*RefTex" (display-buffer-in-side-window)
         (window-height . 0.25)
         (side . bottom)
         (slot . -9)
         ;; (preserve-size . (nil . t))
         ;; (window-parameters . (;; (mode-line-format . (:eval (my/helper-window-mode-line-format)))
         ;;                       ))
         )

        ;; ("\\*scratch\\*"
        ;;  display-buffer-in-side-window
        ;;  (body-function . select-window)
        ;;  ;; (window-width 35)
        ;;  (window-height . (lambda (win) (fit-window-to-buffer win 20 nil 85)))
        ;;  (side . bottom)
        ;;  (slot . -8))

        ((lambda (buf act) (member (buffer-mode buf) my/message-modes-list))
         (display-buffer-at-bottom display-buffer-in-side-window)
         (window-height . 0.25)
         (side . bottom)
         (slot . -6)
         ;; (preserve-size . (nil . t))
         ;; (window-parameters . ((no-other-window . #'ignore)
         ;;                       ;; (mode-line-format . (:eval (my/helper-window-mode-line-format)))
         ;;                       ))
         )

        ("\\*\\(?:Warnings\\|Compile-Log\\|Messages\\)\\*" ;\\|Tex Help\\|TeX errors
         (display-buffer-at-bottom display-buffer-in-side-window display-buffer-in-direction)
         (window-height . (lambda (win) (fit-window-to-buffer
                                         win
                                         (floor (frame-height) 5))))
         (side . bottom)
         (direction . below)
         (slot . -5)
         ;; (preserve-size . (nil . t))
         (window-parameters . ((split-window . #'ignore)
                               ;; (no-other-window . t)
                               ;; (mode-line-format . (:eval (my/helper-window-mode-line-format)))
                               )))

        ("[Oo]utput\\*" display-buffer-in-side-window
         (window-height . (lambda (win)
                            (fit-window-to-buffer win (floor (frame-height) 2.5))))
         (side . bottom)
         (slot . -4)
         ;; (preserve-size . (nil . t))
         ;; (window-parameters . ((no-other-window . t)
         ;;                       ;; (mode-line-format . (:eval (my/helper-window-mode-line-format)))
         ;;                       ))
         )

        ("\\*Async Shell Command\\*" display-buffer-in-side-window
         (window-height . 0.20)
         (side . bottom)
         (slot . -4)
         ;; (preserve-size . (nil . t))
         (window-parameters . ((no-other-window . t)
                               ;; (mode-line-format . (:eval (my/helper-window-mode-line-format)))
                               )))

        ("\\*\\(Register Preview\\).*" (display-buffer-in-side-window)
         (window-height . 0.20)       ; See the :hook
         (side . bottom)
         (slot . -3)
         (window-parameters . ((no-other-window . t)
                               ;; (mode-line-format . (:eval (my/helper-window-mode-line-format)))
                               )))

        ("\\*Completions\\*" (display-buffer-in-side-window)
         (window-height . 0.20)
         (side . bottom)
         (slot . -2)
         ;; (window-parameters . ((no-other-window . t)
         ;;                       ;; (mode-line-format . (:eval (my/helper-window-mode-line-format)))
         ;;                       ))
         )

        ("\\*Apropos\\*" (display-buffer-in-side-window)
         ;; (window-height . 0.40)
         (window-width . 65)
         (side . right)
         (slot . -2)
         (window-parameters . (;; (no-other-window . t)
                               ;; (mode-line-format . (:eval (my/helper-window-mode-line-format)))
                               )))


        ((lambda (buf act) (or (seq-some (lambda (regex) (string-match-p regex buf))
                                    my/repl-names-list)
                          (seq-some (lambda (mode)
                                      (equal
                                       (buffer-mode buf)
                                       mode))
                                    my/repl-modes-list)))
         (display-buffer-reuse-window
          display-buffer-in-direction
          display-buffer-in-side-window)
         (body-function . select-window)
         ;; display-buffer-at-bottom
         (window-height . .35)
         (window-width .  .40)
         ;; (preserve-size . (nil . t))
         (direction . below)
         (side . bottom)
         (slot . 1))

        ((lambda (buf act) (member (buffer-mode buf) my/help-modes-list))
         (display-buffer-reuse-window
          display-buffer-in-direction
          display-buffer-in-side-window)
         (body-function . select-window)
         ;; (direction . bottom)
         ;; (window-height . (lambda (win) (fit-window-to-buffer win 25 14)))
         (window-width . 77 ;; (lambda (win) (fit-window-to-buffer win nil nil 75 65))
                       )
         (direction . below)
         (side . right)
         (slot . 2)
         (window-parameters . ((split-window . #'ignore)
                               ;; (no-other-window . t)
                               ;; (mode-line-format . (:eval (my/helper-window-mode-line-format)))
                               )))

        (;; (lambda (buf act) (equal (buffer-mode buf) 'matlab-shell-help-mode))
         "\\*Matlab Help\\*"
         (display-buffer-reuse-window
          display-buffer-in-side-window
          display-buffer-in-direction)
         (body-function . select-window)
         ;; (direction . bottom)
         ;; (window-height . (lambda (win) (fit-window-to-buffer win 25 14)))
         (window-width . 86 ;; (lambda (win) (fit-window-to-buffer win nil nil 75 65))
                       )
         (direction . right)
         (side . right)
         (slot . 2)
         (window-parameters . ((split-window . #'ignore)
                               ;; (no-other-window . t)
                               ;; (mode-line-format . (:eval (my/helper-window-mode-line-format)))
                               )))

        ("^\\*eldoc.*\\*$"
         (display-buffer-reuse-window
          display-buffer-in-direction
          display-buffer-in-side-window)
         ;; (body-function . select-window)
         ;; (direction . bottom)
         ;; (window-height . (lambda (win) (fit-window-to-buffer win 25 14)))
         (window-width . 82 ;; (lambda (win) (fit-window-to-buffer win nil nil 75 65))
                       )
         (direction . below)
         (side . below)
         (slot . 2)
         (window-parameters . ((dedicated . t)
                               (split-window . #'ignore)
                               (no-other-window . t)
                               (mode-line-format . none))))

        ((lambda (buf act) (member (buffer-mode buf) '(ibuffer-mode bookmark-bmenu-mode)))
         (;; display-buffer-reuse-window
          ;; display-buffer-in-side-window
          ;;display-buffer-at-bottom
          display-buffer-below-selected)
         (body-function . select-window)
         (direction . below)
         (window-height . (lambda (win) (fit-window-to-buffer win 30 7)))
         ;; (dedicated . t)
         ;; (window-width . (lambda (win) (fit-window-to-buffer win nil nil 85 55)))
         ;; (direction . right)
         (side . bottom)
         (slot . 2))


        ;; ((lambda (buf act) (with-current-buffer buf view-mode))
        ;;  (display-buffer-in-side-window)
        ;;  (window-height . (/ (frame-height) 3))
        ;;  (side . bottom)
        ;;  (slot . 10)
        ;;  ;; (window-parameters . (;; (no-other-window . t)
        ;;  ;;                       ;; (mode-line-format . (:eval (my/helper-window-mode-line-format)))
        ;;  ;;                       ))
        ;;  )


        ;; ("\\*elfeed-entry\\*" (lambda (buf act) (let ((parent-win (get-buffer-window)))
        ;;                                      (display-buffer-in-direction buf act)
        ;;                                      (select-window parent-win)
        ;;                                      ))
        ;;  (direction . below)
        ;;  (window-height . 0.5)
        ;;  )
        ))



(provide 'my-window-navigation)
;;; my-window-navigation.el ends here
