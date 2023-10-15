;;; my-libs.el --- Some libraries

;; Copyright (C) 2022-2023 Semen Khramtsov

;; Author: Semen Khramtsov <hrams205@gmail.com>
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
  
  (leaf dash
    :ensure (dash :repo "magnars/dash.el" :host github)
    :global-minor-mode global-dash-fontify-mode
    :require t)

  (leaf s
    :ensure t
    :require t)

  (leaf f
    :ensure t
    :require t)

  ;; (straight-use-package '(just :host github :repo "semenInRussia/just.el"))
  (leaf just
    :ensure (just :host github :repo "semenInRussia/just.el")
    :require t)

  (leaf queue
    :ensure t)

  (leaf request
    :ensure t)

  (leaf async
    :ensure t)

  (leaf alert
    :ensure t)

  (leaf fringe-helper
    :ensure t)

  (leaf ht
    :ensure t)

  (leaf ov
    :ensure t)

  (leaf indicators
    :ensure t)

  (leaf svg-lib
    :ensure t
    :require t))

(provide 'my-libs)
;;; my-libs.el ends here
