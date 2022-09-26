;;; my-zms.el --- my-zms

;; Copyright (C) 2022 Semen Khramtsov

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

;;; Code:
(defcustom my-zms-compile-command
  (s-concat
   "pdflatex "
   "-interaction nonstopmode -file-line-error "
   "--output-directory=\"{output-directory}\" "
   "\"{solution.tex}\"")
  "Command which compile Solution.tex file of the ZMS section.

{solution.tex} will be replaced with path to Solution.tex file of the ZMS
section"
  :type 'string)

(defcustom my-zms-directory "~/zms"
  "Path to the directory in which will be saved files of the ZMS."
  :type 'string)

(defcustom my-zms-view-solution-latex "\\inputsolution{./solutions/%s.tex}"
  "LaTeX source code which should view solution of the ZMS task."
  :type 'string)

(defcustom my-zms-section-template-directory "~/zms/_template/"
  "Path to the directory which will be temlate for the section of the ZMS."
  :type 'string)

(defcustom my-zms-section-solutions-relatieve-path "solutions/"
  "Path relatieve with the section subdirectory to the task solutions files."
  :type 'string)

(defclass my-zms-section ()
  ((name :initarg :name :accessor my-zms-section-name)
   (number :initarg :number :accessor my-zms-section-number))
  "Section of the zms.")

(defun my-zms-new-section (section-name)
  "New section called SECTION-NAME of the ZMS tasks, solutions and other."
  (interactive "sName of the ZMS section, please: ")
  (my-zms-section-save (my-zms-next-section-called section-name)))

(defun my-zms-delete-section (section)
  "Delete SECTION of the ZMS tasks, solutions and other."
  (interactive (list (my-zms-read-section)))
  (->
   section
   (my-zms-section-path)
   (f-delete t)))

(defun my-zms-section-save (section)
  "Save the ZMS SECTION into the file system."
  (my-use-skeleton my-zms-section-template-directory
                   (my-zms-section-path section)
                   `(("_section-number"
                      . ,(number-to-string (my-zms-section-number section)))
                     ("_section-name_"
                      . ,(my-zms-section-name section)))))

(defun my-use-skeleton (skeleton-path destination replacements)
  "Copy SKELETON-PATH to DESTINATION and do REPLACEMENTS."
  (f-copy skeleton-path destination)
  (my-replace-all-in-dir replacements destination))

(defun my-replace-all-in-dir (replacements dir)
  "Replace each `car' of REPLACEMENTS to respective `cdr' in each file of DIR."
  (->
   dir
   (f-files)
   (--each (my-replace-all-in-file replacements it))))

(defun my-replace-all-in-file (replacements filename)
  "Replace each `car' of REPLACEMENTS to respective `cdr' in file at FILENAME."
  (f-write (s-replace-all replacements (f-read filename))
           'utf-8
           filename))

(defun my-zms-section-path (section)
  "Return path to the directory of the ZMS section SECTION."
  (->> section (my-zms-section-dirname) (f-join my-zms-directory)))

(defun my-zms-section-dirname (section)
  "Return name of the directory for the ZMS SECTION."
  (format "%s-%s"
          (my-zms-section-number section)
          (my-normalize-string (my-zms-section-name section))))

(defun my-zms-next-section-called (section-name)
  "Return a object of the `my-zms-section' called SECTION-NAME.

Number will be automatically initialized, depends on the previous sections."
  (my-zms-section
   :name section-name
   :number (my-zms-next-section-number)))

(defun my-zms-next-section-number ()
  "Return number of the next ZMS section."
  (--if-let (my-zms-last-section) (1+ (my-zms-section-number it)) 1))

(defun my-zms-last-section ()
  "Return the last section (section with greatest number) of the ZMS sections."
  (-some->>
      (my-zms-sections)
    (-max-by (-on '> 'my-zms-section-number))))

(defun my-zms-sections ()
  "Return list of the all ZMS sections."
  (->>
   (my-zms-sections-dirs-names)
   (-map 'my-zms-section-dirname-to-section)))

(defun my-zms-sections-dirs-names ()
  "Return list of the names of the dirictories of the all ZMS sections."
  (->>
   my-zms-directory
   (f-directories)
   (-map 'f-base)
   (--remove (s-prefix-p "_" it))))

(defun my-zms-section-dirname-to-section (section-dirname)
  "Convert ZMS SECTION-DIRNAME to an object of the `my-zms-section'."
  (let* ((dirname-parts
          (s-split " " (my-humanize-string section-dirname)))
         (number (string-to-number (car dirname-parts)))
         (name (s-join " " (cdr dirname-parts))))
    (my-zms-section :name name :number number)))

(defun my-zms-new-solution-in-current-section ()
  (interactive)
  (my-zms-new-solution (my-zms-current-section)))

(defun my-zms-new-solution (section)
  "Create a new LaTeX file into subdir of the ZMS SECTION dir named solutions."
  (interactive (list (my-zms-read-section)))
  (let ((task-number (my-zms-section-next-solution-number section)))
    (my-zms-insert-solution-to-solution.tex section task-number)
    (my-zms-find-solution section task-number)))

(defun my-zms-insert-solution-to-solution.tex (section number)
  "Insert a command viewing solution with NUMBER to Solution.tex of SECTION."
  (find-file (my-zms-section-solution.tex-path section))
  (with-temp-buffer
    (insert (f-read (my-zms-section-solution.tex-path section)))
    (forward-line -1)
    (end-of-line)
    (newline)
    (insert (format my-zms-view-solution-latex number))))

(defun my-zms-find-solution (section number)
  "Find/visit file of the ZMS SECTION solution with NUMBER file."
  (interactive
   (list
    (my-zms-read-section)
    (read-number "Number of the solution, please:")))
  (find-file (my-zms-section-solution-number-to-path section number)))

(defun my-zms-delete-solution (section number)
  "Find/visit file of the ZMS SECTION solution with NUMBER file."
  (interactive
   (list
    (my-zms-read-section)
    (read-number "Number of the solution, please:")))
  (delete-file (my-zms-section-solution-number-to-path section number)))

(defun my-zms-read-section ()
  "Read ZMS SECTION from the user."
  (helm
   :prompt "Choose section of the ZMS: "
   :sources '((name . "zms-sections")
              (candidates . my--zms-read-section-candidates)
              (action . (("Choose" . identity))))))

(defun my--zms-read-section-candidates ()
  "Candidates for the `my-zms-read-section' function."
  (->>
   (my-zms-sections)
   (--map (cons (my-zms-format-section it) it))))

(defun my-zms-format-section (section)
  "Format SECTION of the ZMS to a string."
  (format "%s. %s"
          (my-zms-section-number section)
          (my-zms-section-name section)))

(defun my-zms-section-next-solution-number (section)
  "Return number of the next task solution of the ZMS SECTION."
  (--if-let (my-zms-section-last-solution-number section) (1+ it) 1))

(defun my-zms-section-solution-number-to-path (section number)
  "Return path to the task solution of the ZMS SECTION with NUMBER."
  (f-join
   (my-zms-section-solutions-path section)
   (format "%s.tex" number)))

(defun my-zms-section-last-solution-number (section)
  "Return number of the last task solution of the SECTION."
  (->>
   section
   (my-zms-section-solutions-path)
   (f-files)
   (--map (string-to-number (f-base it)))
   (my-max)))

(defun my-zms-section-solutions-path (section)
  "Return path to the subdirectory with solutions of the SECTION subdir."
  (f-join
   (my-zms-section-path section)
   my-zms-section-solutions-relatieve-path))

(defun my-zms-section-solution.tex-path (section)
  "Return path to the Solution.tex file of the ZMS SECTION."
  (f-join
   (my-zms-section-path section)
   "Solution.tex"))

(defun my-zms-current-section ()
  "Get either ZMS session placed in the current directory or last created."
  (or
   (my-zms-path-to-section default-directory)
   (my-zms-last-section)))

(defun my-zms-path-to-section (path)
  "Convert PATH to a file of the ZMS SECTION to an object of `my-zms-section'."
  (and
   (my-zms-path-p path)
   (->>
    (f-full path)
    (s-chop-prefix (f-full my-zms-directory))
    (s-split "/")
    (car)
    (my-zms-section-dirname-to-section))))

(defun my-zms-path-p (&optional path)
  (or path (setq path (buffer-file-name)))
  (s-starts-with-p (f-full my-zms-directory) (f-full path)))

(defun fast-exec-zms-keys ()
  "Get some useful keymaps of  `fast-exec' for zms."
  (fast-exec/some-commands
   ("New ZMS Task Solution"     'my-zms-new-solution)
   ("Forward ZMS Task Solution" 'my-zms-new-solution-in-current-section)
   ("New ZMS Section"           'my-zms-new-section)
   ("Delete ZMS Section"        'my-zms-delete-section)
   ("Delete ZMS Task Solution"  'my-zms-delete-solution)))

(fast-exec/register-keymap-func 'fast-exec-zms-keys)
(fast-exec/reload-functions-chain)

(defun my-zms-run-command-recipe ()
  "Recipe of `run-command' for ZMS."
  (when (my-zms-path-p)
    (list
     (list
      :command-name "zms-compile-section"
      :display "Compile Section.tex file of the ZMS section via `pdflatex'"
      :command-line (my-zms--get-compile-command)
      :working-dir (my-zms-section-path (my-zms-current-section))))))

(defun my-zms--get-compile-command ()
  "Get command for compiling of the section file Solution.tex.

See `my-zms-compile-command'"
  (->>
   my-zms-compile-command
   (s-replace "{solution.tex}"
              (my-zms-section-solution.tex-path (my-zms-current-section)))
   (s-replace "{output-directory}"
              (my-zms-section-output-path (my-zms-current-section)))))

(defun my-zms-section-output-path (section)
  "Get path to the output directory of compiling Solution.tex file of SECTION."
  (->
   section
   (my-zms-section-path)
   (f-join "destination")))

(add-to-list 'run-command-recipes 'my-zms-run-command-recipe)

(provide 'my-zms)
;;; my-zms.el ends here