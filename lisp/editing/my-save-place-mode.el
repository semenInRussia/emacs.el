;;; my-save-place-mode.el --- My configuration of `save-place-mode' -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024 semenInRussia

;; Author: semenInRussia <hrams205@gmail.com>
;; Version: 0.1
;; Homepage: https://github.com/semeninrussia/emacs.el

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; My configuration of `save-place-mode'.  Just jump to saved position
;; when file is opened.  NOTE that it's a builtin power of Emacs

;;; Code:

(require 'my-leaf)

(leaf save-place-mode
  :global-minor-mode save-place-mode)

(provide 'my-save-place-mode)
;;; my-save-place-mode.el ends here
