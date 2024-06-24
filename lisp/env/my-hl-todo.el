;;; my-hl-todo.el --- My config source code for highlight todo commentaries

;; Copyright (C) 2022-2024 semenInRussia

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; My config source code for highlight todo commentaries

;;; Code:
(require 'my-leaf)

;; (leaf hl-todo
;;   :ensure (hl-todo :repo "tarsius/hl-todo" :host github)
;;   :global-minor-mode global-hl-todo-mode)

(defface my-hl-todo-face  '((t (:bold nil :italic t :foreground "#cc9393")))
  "A face to highlight things like TODO and NOTE.")

(define-minor-mode my-hl-todo-mode
  "Highlight things like TODO and NOTE."
  :init-value t
  (when (and my-hl-todo-mode
             (not (eq major-mode 'org-mode)))
    (font-lock-add-keywords
     nil
     '(("\\(TODO\\|NOTE\\|DONE\\|PERF\\|HACK\\)" . 'my-hl-todo-face))
     t)))

(add-hook 'prog-mode-hook #'my-hl-todo-mode)
(add-hook 'text-mode-hook #'my-hl-todo-mode)

(provide 'my-hl-todo)
;;; my-hl-todo.el ends here
