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
  :hook prog-mode-hook text-mode-hook
  :config
  (defun turn-off-flycheck (&rest _)
    "Disable `flycheck-mode' locally for current buffer."
    (interactive)
    (flycheck-mode -1)))


;;; Use `embark' with `flycheck'
;;
;; In `embark-general-map' I have a bound with . command to googling
;; things.  If a `flycheck' diagnostic at point is exist, I can pres
;; "C-. ." to google the error
(leaf embark
  :after flycheck
  :defun ((flycheck-copy-errors-as-kill
           flycheck-overlays-at
           flycheck-error-message
           flycheck-overlay-errors-at)
          . flycheck)
  :defvar (flycheck-mode
           embark-general-map
           embark-keymap-alist
           embark-target-finders)
  :defer-config
  (eval-and-compile
    (defmacro my-embark-action (cmd)
      "Define a command which can be an `emabark' action.

The CMD ignoring embarks args.  CMD must be a symbol"
      (let ((name (string-trim (format "%s" cmd) "'")))
        `(defun ,(intern (concat "my-embark-" name)) (x)
           ,(format "My wrapper over `%s' to be an embark action." name)
           (and x
                (call-interactively ',(intern name)))))))

  (defvar-keymap my-embark-flycheck-map
    :doc "Keymap for Embark actions on `flycheck' diagnostics."
    :parent embark-general-map
    "RET" (my-embark-action 'flycheck-list-errors)
    "n" (my-embark-action 'flycheck-next-error)
    "p" (my-embark-action 'flycheck-previous-error)
    "!" (my-embark-action 'flycheck-compile)
    "e" (my-embark-action 'flycheck-explain-error-at-point))

  (add-to-list 'embark-keymap-alist
               '(flycheck my-embark-flycheck-map))

  (eval-and-compile
    (defun my-embark-target-flycheck-at-point ()
      "Target for `embark' `flycheck' at point."
      (let ((o (car (flycheck-overlays-at (point)))))
        (when (and o flycheck-mode)
          (cons 'flycheck
                (cons
                 (seq-mapcat #'flycheck-error-message
                             (flycheck-overlay-errors-at (point))
                             'string)
                 (cons
                  (overlay-start o)
                  (overlay-end o))))))))

  (add-to-list 'embark-target-finders
               #'my-embark-target-flycheck-at-point
               t))

(provide 'my-flycheck)
;;; my-flycheck.el ends here
