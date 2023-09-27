;;; my-misc.el --- My some little miscellaneous feautures

;; Copyright (C) 2022 Semen Khramtsov

;; Author: Semen Khramtsov <hrams205@gmail.com>
;; Version: 0.1
;; URL: https://github.com/semenInRussia/emacs.el

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; My some little miscellaneous feautures

;;; Code:
(require 'my-leaf)
(require 'dash)
(require 'f)

(setq make-backup-files         nil)
(setq auto-save-list-file-name  nil)
(defalias 'yes-or-no-p 'y-or-n-p)
(toggle-truncate-lines t)

;;; russian input method
;;; now I can press `C-\\' and language on which I am now
;; writting will be changed
(with-eval-after-load 'my-modules
  (setq-default default-input-method "russian-computer")
  (setq default-file-name-coding-system 'utf-8)
  (setq default-keyboard-coding-system 'utf-8)
  (setq buffer-file-coding-system 'utf-8))

;;; just make PPTX (powerpoint) file that can't be read
(defun my-new-fake-pptx-file ()
  "Make this buffer, fake presentation with format (.pptx)."
  (interactive)
  (->> "~/broken.pptx" (f-read) (insert))
  (text-mode))

(with-eval-after-load 'fast-exec
  (eval
   '(fast-exec-bind 'pptx
      (fast-exec-make-some-commands
       ("New Fake PPTX File" 'my-new-fake-pptx-file)))))

;;; I try to decrease the Emacs startup time
(defun my-display-startup-time ()
  (message
   "Emacs loaded in %s with %d garbage collections."
   (format
    "%.2f seconds"
    (float-time (time-subtract after-init-time before-init-time)))
   gcs-done))

(add-hook 'emacs-startup-hook #'my-display-startup-time)

;;; Don't display native-comp warnings
(defvar native-comp-async-report-warnings-errors)
(setq native-comp-async-report-warnings-errors nil)

(provide 'my-misc)
;;; my-misc.el ends here
