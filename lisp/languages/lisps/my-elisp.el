;;; my-elisp.el --- My configuration of the elisp

;; Copyright (C) 2022-2023 semenInRussia

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; My configuration of the `emacs-lisp-mode'

;;; Code:

(require 'my-leaf)

(require 's)
(require 'dash)

(leaf elisp-mode
  :config
  (add-hook 'emacs-lisp-mode 'paxedit-mode)

  (leaf inspector
    :ensure (inspector :repo "emacs-straight/inspector" :host github)
    :bind (:emacs-lisp-mode-map
           :package elisp-mode
           ("C-c C-i" . inspector-inspect-last-sexp)))

  (leaf paredit
    :ensure (paredit :repo "https://mumble.net/~campbell/git/paredit.git" :host nil)
    :hook emacs-lisp-mode-hook)

  (leaf eros
    :ensure (eros :repo "xiongtx/eros" :host github)
    :hook emacs-lisp-mode-hook)

  (leaf elisp-refs
    :ensure t))

(leaf suggest
  :ensure (suggest :repo "Wilfred/suggest.el" :host github))

(leaf mocker
  :ensure (mocker :repo "sigma/mocker.el" :host github)
  :doc "A library for testing `elisp' with mocks")

(leaf my-elisp-embrace
  :hook (emacs-lisp-mode-hook . my-embrace-emacs-lisp-mode-hook))

(provide 'my-elisp)
;;; my-elisp.el ends here
