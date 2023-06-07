;;; my-project.el --- Definitions of some functions from `projectile' -*- lexical-binding: t; -*-

;; Copyright (C) 2023 semenInRussia

;; Author: semenInRussia <hrams205@gmail.com>
;; Version: 0.0.1

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Definitions of some functions from `projectile' like
;; `projectile-acquire-root' or `projectile-files'

;;; Code:
(require 's)
(require 'f)
(require 'dash)
(require 'projectile)

(defvar my-project-files-hash (make-hash-table :test 'equal))

(defcustom my-project-gitignore-default-patterns
  '(".git/" "*.exe")
  "Patterns in .gitignore sytax style which should be ignore by default."
  :type '(repeat string)
  :group 'my)

;;;###autoload
(defun my-project-root (&optional dir)
  "Root of the project at DIR."
  (or dir (setq dir default-directory))
  (-some->>
      projectile-known-projects
    (--filter (s-starts-with? (f-full it) (f-full dir)))
    (-non-nil)
    (--max-by (> (f-depth it) (f-depth other)))))

(defun my-projectile-root-local (dir)
  "Simple wrapper around `projectile-project-root'.

Return value of `projectile-project-root' when DIR is nil, otherwise return nil"
  (unless dir projectile-project-root))

;;;###autoload
(defun my-projectile-project-files (root)
  "Return filenames list of the project at ROOT, with caching."
  (let ((root (f-full root)))
    (unless (gethash root my-project-files-hash)
      (puthash root
               (my-project-files-no-cache root)
               my-project-files-hash))
    (--map (s-chop-prefix root (f-full it))
           (gethash root my-project-files-hash))))

(defun my-project-files-no-cache (root)
  "Return filenames list of the project at ROOT, without caching."
  (if (f-exists-p (f-join root ".git"))
      (let ((default-directory root))
        (->>
         (shell-command-to-string "git ls-files")
         (s-trim)
         (s-lines)))
    (my-files-of-root-not-match-with-regexps
     root
     (my-project-gitignore-regexps root))))

;;;###autoload
(defun projectile-project-files-clear-cache ()
  "Function `projectile-project-files' is cached, clear this cache for ROOT."
  (interactive)
  (setq my-project-files-hash (make-hash-table :test 'equal)))

(defun my-files-of-root-not-match-with-regexps (root regexps)
  "Return files list of ROOT each of it don't match with one of REGEXPS."
  (--reduce-from
   (cond
    ((my-matches-with-one-of-p (f-full it) regexps)
     acc)
    ((f-directory-p it)
     (append acc
             (my-files-of-root-not-match-with-regexps it regexps)))
    (t (cons it acc)))
   nil
   (f-entries root)))

(defun my-matches-with-one-of-p (str regexps)
  "Return non-nil, when one of REGEXPS has match with STR."
  (--find (s-matches-p it str) regexps))

(defun my-project-gitignore-patterns (root)
  "Get patterns list with syntax of .gitignore files for the project at ROOT."
  (append
   (my-project-specific-gitignore-patterns root)
   my-project-gitignore-default-patterns))

(defun my-project-specific-gitignore-patterns (root)
  "Parse .gitignore file of project at ROOT into list of ignored patterns."
  (--when-let
      (my-project-gitignore root)
    (->>
     it
     (f-read)
     (s-lines)
     (--remove
      (or (string-equal "" it) (my-gitignore-comment-line-p it))))))

(defun my-project-gitignore (root)
  "Return path to .gitignore file of the project at ROOT.

If the project not contains .gitignore file, then return nil"
  (let ((path (f-join root ".gitignore")))
    (when (f-exists-p path) path)))

(defun my-regexp-from-gitignore-pattern (regexp gitignore-root)
  "From REGEXP of .gitignore file to real Elisp regular expression.

Notice that a gitignore pattern like \"target/\" converted from this function
won't be matched with paths like \"target/debug\" only strict \"target\"

GITIGNORE-ROOT directory is directory which contains .gitginore file."
  ;; TODO Don't ignore files in project, which has same name with ignored
  ;; directory
  (let ((gitignore-root (f-full gitignore-root)))
    (-->
     regexp
     (my-gitignore-rx-to-el it)
     (if (s-starts-with-p "/" it)
         (f-join gitignore-root (s-chop-prefix "/" it))
       (concat ".*/" it)))))

(defun my-gitignore-rx-to-el (regexp)
  "Transform REGEXP with regexp syntax as in .gitignore file to Elisp regexp."
  (->>
   regexp
   (s-replace "*" "[^/]*")
   (s-replace "\\" "")
   (s-replace "." "\\.")))

(defun my-gitignore-comment-line-p (line)
  "Return non-nil, if a LINE of .gitignore file is commented."
  (s-prefix-p "#" (s-trim line)))

(defun my-project-gitignore-regexps (root)
  "Return list of regexp from .gitignore file of project at ROOT."
  (--map
   (my-regexp-from-gitignore-pattern it root)
   (my-project-gitignore-patterns root)))

;;;###autoload
(defun my-projectile-files-with-string (string directory &optional _file-ext)
  "Return a list of all files containing STRING in DIRECTORY."
  (->>
   directory
   (projectile-project-files)
   (--filter (s-contains-p string (f-read it)))))

(provide 'my-project)
;;; my-project.el ends here
