;;; my-flycheck.el --- My configuration of the `flycheck'

;; Copyright (C) 2022-2024 semenInRussia

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; My configuration of the `flycheck'

;;; Code:

(require 'my-leaf)


(leaf flycheck
  :ensure (flycheck :repo "flycheck/flycheck" :host github)
  :bind (:flycheck-mode-map
         ([remap next-error] . 'flycheck-next-error)
         ([remap previous-error] . 'flycheck-previous-error))
  :defun flycheck-mode
  :global-minor-mode global-flycheck-mode
  :config                             ;nofmt
  (defun turn-off-flycheck (&rest _)
    "Disable `flycheck-mode' locally for current buffer."
    (interactive)
    (flycheck-mode 0)))

(provide 'my-flycheck)
;;; my-flycheck.el ends here
