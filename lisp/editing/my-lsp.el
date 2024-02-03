;;; my-lsp.el --- My choose between LSP clieents -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2024 semenInRussia

;; Author: semenInRussia <hrams205@gmail.com>
;; Version: 0.0.1

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; My choose between LSP clieents.

;;; Code:

(declare-function eglot-ensure "eglot.el")
(declare-function lsp-bridge-mode "lsp-bridge.el")


(defun my-lsp-ensure ()
  "Run choosen lsp client for the current buffer*.

* or add a command to run lsp client to the future calls stacks."
  (interactive)
  ;; run it with idle timer.  It's useful, because in this case Emacs
  ;; don't need to load `eglot' instantly after somebody open a file,
  ;; so file will be opened more quickly, and lsp will be activated
  ;; after some time
  (run-with-idle-timer 1 nil #'eglot-ensure))

(provide 'my-lsp)
;;; my-lsp.el ends here
