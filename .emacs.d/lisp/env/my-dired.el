;;; my-dired.el --- My configuration of the `dired'

;; Copyright (C) 2022 Semen Khramtsov

;; Author: Semen Khramtsov <hrams205@gmail.com>
;; Version: 0.1
;; URL: https://github.com/semenInRussia/emacs.el

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; My configuration of the `dired'

;;; Code:

(require 's)
(require 'dash)
(require 'just)
(require 'f)

(leaf dired
  :hook ((dired-mode-hook . xah-fly-insert-mode-activate)
         (dired-mode-hook . dired-hide-details-mode))
  :defun ((dired-mark
           dired-do-rename
           dired-current-directory
           dired-jump
           dired-get-file-for-visit)
          (helm-open-file-with-default-tool . helm-utils))
  :bind (:dired-mode-map
         ("SPC"     . nil)                ; make command at space empty prefix

         ;; Navigation
         ("k"       . next-line)
         ("i"       . previous-line)
         ("n"       . dired-avy)
         ("SPC h"   . beginning-of-buffer)
         ("SPC n"   . end-of-buffer)
         ("'"       . dired-isearch-filenames)

         ;; Open file
         ("o"       . dired-find-file-other-window)
         ;; some other open files I define in the section "Dired Hacks: Open"

         ;; Mark files
         ("t"       . dired-mark)
         ("SPC u"   . dired-unmark-all-marks)

         ;; Misc.
         ("y"       . dired-undo)
         ("A"       . agnifize-dwim)
         ("~"       . my-dired-jump-to-home)

         ;; Key bindings which not change your commands
         ("a"       . helm-M-x)
         (","       . xah-next-window-or-frame))

  :config                               ;nofmt
  (leaf async                           ;nofmt
    :config (dired-async-mode 1))

  (leaf my-dired-commands
    :bind (:dired-mode-map
           :package dired
           ;; Mark anything
           ("SPC a"   . my-dired-mark-all-files)
           ;; Manipulation with file(s)
           ;; copy/move/paste also defines in the section "Dired Hacks: Ranger"
           ("SPC g"   . my-dired-delete)
           ("SPC x"   . my-dired-delete-all-files)
           ("SPC y"   . my-dired-duplicate)
           ("f"       . my-dired-rename)
           ("SPC TAB" . my-dired-move)
           ("s"       . my-dired-new-file)
           ("j"       . my-dired-goto-parent-dir)))

  (leaf dired-filter
    :ensure t
    :bind-keymap (:dired-mode-map       ;nofmt
                  :package dired
                  ("." . dired-filter-map)))

  (leaf dired-open
    :ensure t
    :bind (:dired-mode-map
           :package dired
           ("l"   . 'dired-open-file)
           ("RET" . 'dired-open-file))
    :push ((dired-open-functions . 'my-dired-open-function-pdf))
    :defvar dired-open-functions
    :defun (my-dired . (my-pdf-file my-try-open-pdf-file))
    :config                             ;nofmt
    (defun my-dired-open-function-pdf ()
      "Open function for `dired-open-functions'."
      (my-try-open-pdf-file (dired-get-file-for-visit)))

    (defun my-try-open-pdf-file (filename)
      "If file at FILENAME is a pdf file, then open as pdf, other return nil."
      (when (my-pdf-file filename)
        (helm-open-file-with-default-tool filename)
        t))

    (defun my-pdf-file (filename)
      "Return t, when FILENAME is path to a PDF file."
      (s-suffix-p ".pdf" filename)))

  (leaf dired-rainbow
    :ensure t
    :require t
    :defun (dired-rainbow-define-chmod dired-rainbow-define)
    :config                             ;nofmt
    (dired-rainbow-define-chmod directory "#6cb2eb" "d.*")
    (dired-rainbow-define html 'yellow2
                          ("css"
                           "less"
                           "sass"
                           "scss"
                           "htm"
                           "html"
                           "jhtm"
                           "mht"
                           "eml"
                           "mustache"
                           "xhtml"))
    (dired-rainbow-define xml "#f2d024"
                          ("xml"
                           "xsd"
                           "xsl"
                           "xslt"
                           "wsdl"
                           "bib"
                           "json"
                           "msg"
                           "pgn"
                           "rss"
                           "yaml"
                           "yml"
                           "rdata"))
    (dired-rainbow-define document "#9561e2"
                          ("docm"
                           "doc"
                           "docx"
                           "odb"
                           "odt"
                           "pdb"
                           "pdf"
                           "ps"
                           "rtf"
                           "djvu"
                           "epub"
                           "odp"
                           "ppt"
                           "pptx"))
    (dired-rainbow-define markdown "yellow"
                          ("org"
                           "etx"
                           "info"
                           "markdown"
                           "md"
                           "mkd"
                           "nfo"
                           "pod"
                           "rst"
                           "tex"
                           "textfile"
                           "txt"))
    (dired-rainbow-define database "#6574cd"
                          ("xlsx"
                           "xls"
                           "csv"
                           "accdb"
                           "db"
                           "mdb"
                           "sqlite"
                           "nc"))
    (dired-rainbow-define media "#de751f"
                          ("mp3"
                           "mp4"
                           "MP3"
                           "MP4"
                           "avi"
                           "mpeg"
                           "mpg"
                           "flv"
                           "ogg"
                           "mov"
                           "mid"
                           "midi"
                           "wav"
                           "aiff"
                           "flac"))
    (dired-rainbow-define image "#f66d9b"
                          ("tiff"
                           "tif"
                           "cdr"
                           "gif"
                           "ico"
                           "jpeg"
                           "jpg"
                           "png"
                           "psd"
                           "eps"
                           "svg"))
    (dired-rainbow-define log "#c17d11" ("log"))
    (dired-rainbow-define shell "#f6993f"
                          ("awk" "bash" "bat" "sed" "sh" "zsh" "vim"))
    (dired-rainbow-define interpreted "#38c172"
                          ("py"
                           "ipynb"
                           "rb"
                           "pl"
                           "t"
                           "msql"
                           "mysql"
                           "pgsql"
                           "sql"
                           "r"
                           "clj"
                           "cljs"
                           "scala"
                           "js"))
    (dired-rainbow-define compiled "#4dc0b5"
                          ("asm"
                           "cl"
                           "lisp"
                           "el"
                           "c"
                           "h"
                           "c++"
                           "h++"
                           "hpp"
                           "hxx"
                           "m"
                           "cc"
                           "cs"
                           "cp"
                           "cpp"
                           "go"
                           "f"
                           "for"
                           "ftn"
                           "f90"
                           "f95"
                           "f03"
                           "f08"
                           "s"
                           "rs"
                           "hi"
                           "hs"
                           "pyc"
                           ".java"))
    (dired-rainbow-define executable "#8cc4ff" ("exe" "msi"))
    (dired-rainbow-define compressed "#51d88a"
                          ("7z"
                           "zip"
                           "bz2"
                           "tgz"
                           "txz"
                           "gz"
                           "xz"
                           "z"
                           "Z"
                           "jar"
                           "war"
                           "ear"
                           "rar"
                           "sar"
                           "xpi"
                           "apk"
                           "xz"
                           "tar"))
    (dired-rainbow-define packaged "#faad63"
                          ("deb"
                           "rpm"
                           "apk"
                           "jad"
                           "jar"
                           "cab"
                           "pak"
                           "pk3"
                           "vdf"
                           "vpk"
                           "bsp"))
    (dired-rainbow-define encrypted "#ffed4a"
                          ("gpg"
                           "pgp"
                           "asc"
                           "bfe"
                           "enc"
                           "signature"
                           "sig"
                           "p12"
                           "pem"))
    (dired-rainbow-define fonts "#6cb2eb"
                          ("afm" "fon" "fnt" "pfb" "pfm" "ttf" "otf"))
    (dired-rainbow-define
     partition
     "#e3342f"
     ("dmg" "iso" "bin" "nrg" "qcow" "toast" "vcd" "vmdk" "bak"))
    (dired-rainbow-define vc "#0074d9"
                          ("git" "gitignore" "gitattributes" "gitmodules")))

  (leaf dired-ranger
    :ensure t
    :bind (:dired-mode-map
           :package dired
           ("m" . 'dired-ranger-move)
           ("v" . 'dired-ranger-paste)
           ("c" . 'dired-ranger-copy)))

  (leaf dired-subtree
    :ensure t
    :bind (:dired-mode-map
           :package dired
           ("TAB" . 'dired-subtree-cycle)
           ("/"   . 'my-dired-subtree-in-special-buffer))
    :config                           ;nofmt
    (defun my-dired-subtree-in-special-buffer ()
      "Open current `dired-subtree' in the separate `dired' buffer."
      (interactive)
      (my-dired-save-excursion
        (dired-subtree-beginning)
        (dired-previous-line 1)
        (dired (dired-x-guess-file-name-at-point)))))

  (leaf dired-collapse
    :ensure t
    :hook (dired-mode-hook . dired-collapse-mode))

  (defcustom my-dired-commands-using-minibuffer
    '(dired-filter-by-file
      dired-filter-by-extension
      my-dired-new-file
      dired-byte-compile
      dired-do-delete
      dired-create-directory
      dired-isearch-filenames)
    "List of the `dired' commands using the minubuffer."
    :type '(repeat symbol)
    :group 'my)

  (advice-add 'dired-jump
              :after (lambda (&rest _ignore) (xah-fly-insert-mode-activate))
              '((name . "xah-fly-insert-mode-activate")))

  (--each my-dired-commands-using-minibuffer
    (advice-add it :after
                (lambda (&rest _) (xah-fly-insert-mode-activate))
                '((name . xah-fly-insert-mode-activate))))

  ;; Command for printing file
  (with-eval-after-load 'lpr (setq lpr-command "PDFToPrinter"))

  (remove-hook 'dired-mode-hook 'dired-mode))

(provide 'my-dired)
;;; my-dired.el ends here
