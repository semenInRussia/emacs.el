;;; my-hl-todo.el --- My config source code for highlight todo commentaries

;; Copyright (C) 2022-2024 Semen Khramtsov

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; My config source code for highlight todo commentaries

;;; Code:
(require 'my-leaf)

(leaf hl-todo
  :ensure (hl-todo :repo "tarsius/hl-todo" :host github)
  :global-minor-mode global-hl-todo-mode)

(provide 'my-hl-todo)
;;; my-hl-todo.el ends here
