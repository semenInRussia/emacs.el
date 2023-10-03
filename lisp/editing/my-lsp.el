;;; my-lsp.el --- My choose between LSP clieents -*- lexical-binding: t; -*-

;; Copyright (C) 2023 semenInRussia

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
  "Run choosen lsp client for the current buffer."
  (interactive)
  (eglot-ensure))

(provide 'my-lsp)
;;; my-lsp.el ends here
