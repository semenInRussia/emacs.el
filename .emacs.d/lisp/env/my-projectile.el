;;; my-projectile.el --- My configration of `projectile'

;; Copyright (C) 2022-2023 Semen Khramtsov

;; Author: Semen Khramtsov <hrams205@gmail.com>
;; Version: 0.1
;; URL: https://github.com/semenInRussia/emacs.el

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; My configration of `projectile'

;;; Code:

(require 'dash)
(require 'f)
(require 's)

(leaf projectile
  :ensure t
  :defun (projectile-acquire-root
          projectile-project-p
          projectile-project-root
          projectile-completing-read
          projectile-project-files
          projectile-maybe-invalidate-cache)
  :defvar (projectile-known-projects projectile-project-root)
  :custom ((projectile-switch-project-action . 'helm-projectile-find-file)
           (projectile-project-search-path . '("~/projects/"))
           (projectile-completion-system . 'helm)
           (projectile-project-root-functions .
                                              '(projectile-root-local
                                                my-project-root))
           ((projectile-enable-caching . nil)
            (projectile-auto-discover . nil)))
  :global-minor-mode projectile-mode
  :config                               ;nofmt
  (leaf helm-projectile
    :ensure t
    :require t
    :after helm
    :defvar (helm-projectile-file-actions
             helm-pattern
             helm-source-projectile-files-list)
    :defun ((helm-ff-prefix-filename  . helm-files)
            (with-helm-current-buffer . helm-lib)
            (helm-build-sync-source   . helm-source)
            (helm-update              . helm-core)
            (helm-projectile--move-to-real
             helm-projectile--remove-move-to-real
             helm-projectile-file-persistent-action))
    :commands (helm-projectile-find-file)
    :bind ((:xah-fly-command-map
            :package xah-fly-keys
            ("SPC j"  . helm-projectile-find-file))
           (:helm-projectile-find-file-map
            ("M-<f5>" . my-helm-projectile-find-file-update)))
    :config                             ;nofmt
    (defun my-helm-projectile-find-file-update ()
      "Update function for `helm-projectile-find-file'."
      (interactive)
      (projectile-project-files-clear-cache
       (projectile-acquire-root))
      (helm-update))

    (defun projectile--find-file (invalidate-cache &optional ff-variant)
      "Jump to a project's file using completion.

With INVALIDATE-CACHE invalidates the cache first.  With FF-VARIANT set to a
defun, use that instead of `find-file'.   A typical example of such a defun
would be `find-file-other-window' or `find-file-other-frame'"
      (interactive "P")
      (projectile-maybe-invalidate-cache invalidate-cache)
      (let* ((project-root (projectile-acquire-root))
             (file
              (projectile-completing-read
               "Find file: "
               (projectile-project-files project-root t)))
             (ff (or ff-variant #'find-file)))
        (when file
          (funcall ff (expand-file-name file project-root))
          (run-hooks 'projectile-find-file-hook))))

    (defun helm-projectile-file-list-filtered-candidate-transformer (files
                                                                     ;; nofmt
                                                                     _source)
      "File Candiadate Transformer for `helm-source-projectile-files-list'.

Change _SOURCE with FILES of the current project"
      (lambda ()
        (with-helm-current-buffer
          (let* ((root (projectile-project-root))
                 (file-at-root
                  (file-relative-name
                   (expand-file-name helm-pattern root))))
            (if (or
                 (string-empty-p helm-pattern)
                 (assoc helm-pattern files))
                files
              (if (equal helm-pattern file-at-root)
                  (cl-acons
                   (helm-ff-prefix-filename helm-pattern nil t)
                   (expand-file-name helm-pattern)
                   files)
                (cl-pairlis
                 (list
                  (helm-ff-prefix-filename helm-pattern nil t)
                  (helm-ff-prefix-filename file-at-root nil t))
                 (list
                  (expand-file-name helm-pattern)
                  (expand-file-name helm-pattern root))
                 files)))))))

    (defun helm-source-projectile-files-list-before-init ()
      "Function which will be runned before `helm-projectile-files-list'."
      (add-hook 'helm-after-update-hook #'helm-projectile--move-to-real)
      (add-hook 'helm-cleanup-hook #'helm-projectile--remove-move-to-real))

    (defvar helm-source-projectile-files-list-before-init-hook nil
      "Hook which will be runned befor `helm-projectile-files-list'.")

    (add-hook 'helm-source-projectile-files-list-before-init-hook
              'helm-source-projectile-files-list-before-init)

    (setq helm-source-projectile-files-list
          (helm-build-sync-source "Projectile files"
            :before-init-hook 'helm-source-projectile-files-list-before-init-hook
            :candidates (lambda
                          ()
                          (when (projectile-project-p)
                            (with-helm-current-buffer
                              (cl-loop with root =
                                       (projectile-project-root)
                                       for display in
                                       (projectile-current-project-files)
                                       collect
                                       (cons
                                        (s-chop-prefix
                                         (f-full root)
                                         (f-full display))
                                        (expand-file-name display root))))))
            :filtered-candidate-transformer (lambda
                                              (files _source)
                                              (with-helm-current-buffer
                                                (let* ((root (projectile-project-root))
                                                       (file-at-root
                                                        (file-relative-name
                                                         (expand-file-name helm-pattern root))))
                                                  (if (or
                                                       (string-empty-p helm-pattern)
                                                       (assoc helm-pattern files))
                                                      files
                                                    (if (equal helm-pattern file-at-root)
                                                        (cl-acons
                                                         (helm-ff-prefix-filename helm-pattern nil t)
                                                         (expand-file-name helm-pattern)
                                                         files)
                                                      (cl-pairlis
                                                       (list
                                                        (helm-ff-prefix-filename helm-pattern nil t)
                                                        (helm-ff-prefix-filename file-at-root nil t))
                                                       (list
                                                        (expand-file-name helm-pattern)
                                                        (expand-file-name helm-pattern root))
                                                       files))))))
            :fuzzy-match helm-projectile-fuzzy-match
            :keymap helm-projectile-find-file-map
            :help-message 'helm-ff-help-message
            :mode-line helm-read-file-name-mode-line-string
            :action helm-projectile-file-actions
            :persistent-action #'helm-projectile-file-persistent-action
            :persistent-help "Preview file"))))

(defalias 'projectile-project-files 'my-projectile-project-files)
(defalias 'projectile-project-root 'my-project-root)
(defalias 'projectile-root-local 'my-projectile-root-local)
(defalias 'projectile-files-with-string 'my-projectile-files-with-string)

(provide 'my-projectile)
;;; my-projectile.el ends here
