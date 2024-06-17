;;; my-libs.el --- Some libraries

;; Copyright (C) 2022-2023 semenInRussia

;; Author: semenInRussia <hrams205@gmail.com>
;; Version: 0.1
;; URL: https://github.com/semenInRussia/emacs.el

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Some libraries

;;; Code:

(require 'my-leaf)


(eval-and-compile
  ;; `eval-and-compile' installs all libraries in compile-time
  ;; , so "(require \\='dash)" compiles successufelly
  (leaf seq
    :ensure (seq :type built-in))

  (leaf dash
    :ensure (dash :repo "magnars/dash.el" :host github)
    :require t)

  (leaf s
    :ensure t
    :require t)

  (leaf f
    :ensure t
    :require t)

  (leaf just
    :ensure (just :host github :repo "semenInRussia/just.el")
    :require t)

  (leaf indicators :ensure t)
  (leaf svg-lib :ensure t))

(provide 'my-libs)
;;; my-libs.el ends here
