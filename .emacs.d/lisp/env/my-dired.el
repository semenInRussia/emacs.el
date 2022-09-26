;;; my-dired.el --- my-dired

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

;;; Code:
(add-hook 'dired-mode-hook (lambda () (dired-hide-details-mode 1)))

(use-package dired
    :bind
  ((:map dired-mode-map)
   ("SPC"     . nil)                ; make command at space empty prefix

   ;; Navigation
   ("k"       . 'next-line)
   ("i"       . 'previous-line)
   ("n"       . 'dired-avy)
   ("SPC h"   . 'beginning-of-buffer)
   ("SPC n"   . 'end-of-buffer)
   ("'"       . 'dired-isearch-filenames)

   ;; Open file
   ("o"       . 'dired-find-file-other-window)
   ("j"       . 'my-dired-goto-parent-dir)
   ;; some other open files I define in the section "Dired Hacks: Open"

   ;; Manipulation with file(s)
   ("SPC g"   . 'my-dired-delete)
   ("SPC x"   . 'my-dired-delete-all-files)
   ("SPC y"   . 'my-dired-duplicate)
   ("f"       . 'my-dired-rename)
   ("SPC TAB" . 'my-dired-move)
   ("s"       . 'my-dired-new-file)
   ;; copy/move/paste also defines in the section "Dired Hacks: Ranger"

   ;; Mark files
   ("t"       . 'dired-mark)
   ("SPC u"   . 'dired-unmark-all-marks)
   ("SPC a"   . 'my-dired-mark-all-files)

   ;; Misc.
   ("y"       . 'dired-undo)
   ("~"       . 'my-dired-jump-to-home)

   ;; Key bindings which not change your commands
   ("a"       . 'xah-fly-M-x)
   (","       . 'xah-next-window-or-frame))
  :custom ((lpr-command "PDFToPrinter")) ; Command for printing file
  :config (add-hook 'dired-mode-hook 'xah-fly-insert-mode-activate))

(defun my-dired-mark-all-files ()
  "Mark all file in `dired'."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (dired-mark 1)))

(defmacro my-define-dired-command-taking-file (name args docstring &rest body)
  "Define the command from function which take a 1 argument: filename."
  (declare (indent 2))
  `(defun ,name ()
     (interactive)
     ,docstring
     (funcall
      (lambda ,args ,@body)
      (dired-get-filename))
     (revert-buffer)))

(defun my-dired-name-of-file-at-line ()
  "Get name of file at dired object at current point."
  (-last-item (s-split " " (just-text-at-line))))

(my-define-dired-command-taking-file my-dired-rename
    (from)
  "Rename file at point from FROM to other name readed from the minibuffer."
  (let ((to (my-rename-file from)))
    (revert-buffer)
    (my-dired-goto-file to)))

(defun my-dired-goto-file (file)
  "Go to line of `dired' buffer describing FILE."
  (goto-char (point-min))
  (search-forward (f-base to)))

(defun my-rename-file (file)
  "Change name of FILE to new readed from the minibuffer name.

Return new name of FILE"
  (let* ((new-name-of-file
          (read-string "New name, please: " (f-filename from)))
         (to (f-join (f-dirname from) new-name-of-file)))
    (f-move from to)
    to))

(defun my-dired-move ()
  "Move file of current directory of `dired' at the point."
  (interactive)
  (dired-do-rename))

(my-define-dired-command-taking-file my-dired-delete
    (file)
  "Delete file at dired object at current position of the cursor."
  (f-delete file t))

(defun my-dired-goto-parent-dir ()
  "Navigate to parent directory of current dired directory."
  (interactive)
  (let ((parent (f-parent (dired-current-directory))))
    (kill-buffer)
    (dired parent)))

(defun my-dired-new-file (filename)
  "Create file with FILENAME in the directory which opened in the dired buffer."
  (interactive "sName of new file, please: ")
  (f-touch (f-join (dired-current-directory) filename)))

(defun my-dired-delete-all-files ()
  "Delete all files from the directory of the `dired' buffer."
  (interactive)
  (--each (f-entries (dired-current-directory))
    (f-delete it t))
  (revert-buffer))

(defun dired-avy ()
  "Version of `avy' for the `dired'."
  (interactive)
  (avy-goto-line))

(my-define-dired-command-taking-file my-dired-duplicate (filename)
  "Make copy of the file with FILENAME in the same directory."
  (f-touch
   (f-join
    (dired-current-directory)
    (read-string "Name of the filename, please: " (f-filename filename)))))

(defun my-dired-jump-to-home ()
  "Open a `dired' buffer of the home directory."
  (interactive)
  (dired-jump nil "~/"))

(use-package dired-filter
    :ensure t
    :init
    (define-key dired-mode-map (kbd ".") dired-filter-map))

(use-package dired-open
    :ensure t
    :bind ((:map dired-mode-map)
           ("l"   . 'dired-open-file)
           ("RET" . 'dired-open-file))
    :config
    (add-to-list 'dired-open-functions 'my-dired-open-function-pdf))

(defun my-dired-open-function-pdf ()
  "Open function for `dired-open-functions'."
  (my-try-open-pdf-file (dired-get-file-for-visit)))

(defun my-try-open-pdf-file (filename)
  "If file with FILENAME is a pdf file, then open it as pdf, other return nil."
  (when (my-pdf-file filename)
    (helm-open-file-with-default-tool filename)
    t))

(defun my-pdf-file (filename)
  "Return t, when FILENAME is path to a PDF file."
  (s-suffix-p ".pdf" filename))

(use-package dired-rainbow
    :ensure t
    :config
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
    (dired-rainbow-define xml "#f2d024" ("xml"
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
    (dired-rainbow-define document "#9561e2" ("docm"
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
    (dired-rainbow-define markdown "yellow" ("org"
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
    (dired-rainbow-define database "#6574cd" ("xlsx"
                                              "xls"
                                              "csv"
                                              "accdb"
                                              "db"
                                              "mdb"
                                              "sqlite"
                                              "nc"))
    (dired-rainbow-define media "#de751f" ("mp3"
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
    (dired-rainbow-define image "#f66d9b" ("tiff"
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
    (dired-rainbow-define shell "#f6993f" ("awk"
                                           "bash"
                                           "bat"
                                           "sed"
                                           "sh"
                                           "zsh"
                                           "vim"))
    (dired-rainbow-define interpreted "#38c172" ("py"
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
    (dired-rainbow-define compiled "#4dc0b5" ("asm"
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
    (dired-rainbow-define compressed "#51d88a" ("7z"
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
    (dired-rainbow-define packaged "#faad63" ("deb"
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
    (dired-rainbow-define encrypted "#ffed4a" ("gpg"
                                               "pgp"
                                               "asc"
                                               "bfe"
                                               "enc"
                                               "signature"
                                               "sig"
                                               "p12"
                                               "pem"))
    (dired-rainbow-define fonts "#6cb2eb" ("afm"
                                           "fon"
                                           "fnt"
                                           "pfb"
                                           "pfm"
                                           "ttf"
                                           "otf"))
    (dired-rainbow-define
     partition
     "#e3342f"
     ("dmg"
      "iso"
      "bin"
      "nrg"
      "qcow"
      "toast"
      "vcd"
      "vmdk"
      "bak"))
    (dired-rainbow-define vc "#0074d9"
                          ("git" "gitignore" "gitattributes" "gitmodules")))

(use-package dired-ranger
    :ensure t
    :bind ((:map dired-mode-map)
           ("m" . 'dired-ranger-move)
           ("v" . 'dired-ranger-paste)
           ("c" . 'dired-ranger-copy)))

(use-package dired-collapse
    :ensure t
    :hook (dired-mode . dired-collapse-mode))

(provide 'my-dired)
;;; my-dired.el ends here