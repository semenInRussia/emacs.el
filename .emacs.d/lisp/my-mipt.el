;;; my-mipt.el --- my-mipt

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
(defcustom my-mipt-dir "c:/Users/hrams/Documents/mipt"
  "Path to the directory in which will be saved all MIPT solutions."
  :type 'string)

(defcustom my-mipt-lessons
  '("f" "m" "i")
  "Lessons of the MIPT."
  :type '(repeat string))

(defcustom my-mipt-courses-ids-file "~/.mipt"
  "File in which will be saved identifiers for web-urls of the MIPT courses."
  :type 'string)

(defclass my-mipt-task ()
  ((class :initform nil :initarg :class :accessor my-mipt-task-class)
   (lesson :initform nil
           :initarg :lesson
           :accessor my-mipt-task-lesson)
   (section :initform nil
            :initarg :section
            :accessor my-mipt-task-section)
   (kind :initform nil ;nofmt
         :initarg :kind
         :accessor my-mipt-task-kind)   ; either 'control or 'normal
   (number  :initform nil
            :initarg :number
            :accessor my-mipt-task-number))
  "Object for task of the MIPT.")

(defvar my-mipt-found-task
  (my-mipt-task)
  "Object of the `my-mipt-task', will be setted automatically when find task.")

(defvar my-mipt-last-task
  nil
  "Last found MIPT task.

Object of the `my-mipt-task', will set automatically when find task.")

(defun my-mipt-task-control-p (task)
  "Return t, when TASK is control."
  (eq (my-mipt-task-kind task) 'control))

(defun my-mipt-task-normal-p (task)
  "Return t, when TASK is normal, no control."
  (not (my-mipt-task-control-p task)))

(defun my-mipt-task-parse (filename)
  "Parse from FILENAME a MIPT task."
  (when (s-matches-p ".+-.-.+-.+\\(-control\\)?\\.tex" filename)
    (-let*
        ((base (f-base (f-no-ext filename)))
         ((class lesson section num is-control)
          (s-split "-" base)))
      (my-mipt-task
       :class (string-to-number class)
       :lesson lesson
       :section (string-to-number section)
       :number (string-to-number num)
       :kind (if (stringp is-control) 'control 'normal)))))

(defun my-mipt-task-path (task)
  "Get path to the TASK's solution."
  (->>
   (format
    "%s-%s-%s-%s%s.tex"
    (my-mipt-task-class task)
    (my-mipt-task-lesson task)
    (my-mipt-task-section task)
    (my-mipt-task-number task)
    (if (my-mipt-task-control-p task) "-control" ""))
   (f-join my-mipt-dir)))

(defun my-mipt-last-task ()
  "Return the last opened MIPT task via `recentf'."
  (-some->>
      recentf-list
    (-concat (-keep #'buffer-file-name (buffer-list)))
    (-first #'my-mipt-task-parse)
    (my-mipt-task-parse)))

(defalias 'my-mipt-current-task 'my-mipt-last-task)

(defun my-mipt-visit-last-task ()
  "Visit last opened task searched via `my-mipt-last-task'."
  (interactive)
  (my-mipt-task-visit (my-mipt-last-task)))

(defun my-mipt-next-task (&optional is-visit)
  "Return the next MIPT task after the current MIPT task.

If IS-VISIT is t, then also visit the next mipt task"
  (interactive (list t))
  (let ((next-task (my-mipt-current-task)))
    (incf (my-mipt-task-number next-task))
    (when is-visit
      (my-mipt-task-visit next-task))
    next-task))

(defun my-mipt-prev-task (&optional is-visit)
  "Return previous task, before the last found task.

If IS-VISIT is t, then also visit the next mipt task."
  (interactive (list t))
  (let ((prev-task (my-mipt-current-task)))
    (decf (my-mipt-task-number prev-task))
    (when is-visit
      (my-mipt-task-visit next-task))
    next-task))

(defun my-mipt-task-visit (task)
  "Visit file of the TASK's solution."
  (interactive (list (my-mipt-find-task)))
  (find-file (my-mipt-task-path task)))

(defun my-mipt-all-tasks ()
  "Return all mipt tasks in the directory `my-mipt-dir'."
  (-keep 'my-mipt-task-parse
         (f-files my-mipt-dir)))

(defun my-mipt-find-task ()
  "Find the task from the minibuffer, take default info from the last task."
  (interactive)
  (setq my-mipt-found-task (my-mipt-task))
  (->>
   (my-mipt-all-tasks)
   (my-mipt--find-lesson-from-tasks)
   (my-mipt--find-class-from-tasks)
   (my-mipt--find-section-from-tasks)
   (my-mipt--find-kind-from-tasks)
   (my-mipt--find-number-from-tasks))
  (my-mipt-complete-task my-mipt-found-task))

(defun my-mipt--find-lesson-from-tasks (tasks)
  "From TASKS find lesson, save in special variable, and return filtered TASKS.

Special variable is `my-mipt-found-task'"
  (let ((lesson (my-mipt-read-lesson)))
    (setf (my-mipt-task-lesson my-mipt-found-task) lesson)
    (->>
     tasks
     (--filter (string-equal (my-mipt-task-lesson it) lesson)))))

(defun my-mipt-read-lesson ()
  "Read from user MIPT's lesson."
  (completing-read "Choose one of MIPT lessons, please: " my-mipt-lessons))

(defun my-mipt--find-class-from-tasks (tasks)
  "From TASKS find class, save in special variable, and return filtered TASKS.

Special variable is `my-mipt-found-task'"
  (let* ((class (my-mipt-choose-one-of-task-classes tasks)))
    (setf (my-mipt-task-class my-mipt-found-task) class)
    (--filter
     (= (my-mipt-task-class it) class)
     tasks)))

(defun my-mipt-choose-one-of-task-classes (tasks)
  "Take TASKS and choose one of classes."
  (->>
   tasks
   (-map #'my-mipt-task-class)
   (my-max)
   (my-mipt-read-class)))

(defun my-mipt-read-class (&optional default)
  "Read from user class of MIPT task, defaults to DEFAULT."
  (read-number "Choose one of MIPT classes, please: " default))

(defun my-mipt--find-section-from-tasks (tasks)
  "From TASKS find section, save in special variable, and return filtered TASKS.

Special variable is `my-mipt-found-task'"
  (let* ((section (my-mipt-choose-one-of-task-sections tasks)))
    (setf (my-mipt-task-section my-mipt-found-task) section)
    (--filter (= (my-mipt-task-section it) section) tasks)))

(defun my-mipt-choose-one-of-task-sections (tasks)
  "Take TASKS and choose one of sections."
  (->>
   tasks
   (-map #'my-mipt-task-section)
   (my-max)
   (my-mipt-read-section)))

(defun my-mipt-read-section (&optional default)
  "Read from user section of MIPT task, defaults to DEFAULT."
  (read-number "Enter section of MIPT task, please: " default))

(defun my-mipt--find-kind-from-tasks (tasks)
  "From TASKS find kind, save in special variable, and return filtered TASKS.

Special variable is `my-mipt-found-task'"
  (let ((kind (my-mipt-choose-one-of-task-kinds tasks)))
    (setf (my-mipt-task-kind my-mipt-found-task) kind)
    (--filter (eq (my-mipt-task-kind it) kind) tasks)))

(defun my-mipt-choose-one-of-task-kinds (tasks)
  "Take TASKS and choose one of kinds."
  (let* ((is-was-normal-tasks (-any #'my-mipt-task-normal-p tasks))
         (is-normal
          (if is-was-normal-tasks
              (y-or-n-p "Your task normal? ")
            (not (y-or-n-p "Your task control? ")))))
    (if is-normal 'normal 'control)))

(defun my-mipt-read-kind (&optional default)
  "Read from user kind of MIPT task, defaults to DEFAULT."
  (if (y-or-n-p "Your task normal? ") 'normal 'control))

(defun my-mipt--find-number-from-tasks (tasks)
  "From TASKS find number, save in special variable, and return filtered TASKS.

Special variable is `my-mipt-found-task'"
  (let* ((number (my-mipt-choose-one-of-task-numbers tasks)))
    (setf (my-mipt-task-number my-mipt-found-task) number)
    (--filter (= (my-mipt-task-number it) number) tasks)))

(defun my-mipt-choose-one-of-task-numbers (tasks)
  "Take TASKS and choose one of classes."
  (let* ((numbers (-map #'my-mipt-task-number tasks))
         (default (my-max numbers)))
    (just-completing-read-numbers "Please, choose number of MIPT task: "
                                  numbers
                                  nil
                                  nil
                                  default)))

(defun my-mipt-read-number (&optional default)
  "Read from user number of MIPT's task, defaults to DEFAULT."
  (read-number "Please, type number of MIPT task: " (or default 1)))

(defun my-mipt-complete-task (task)
  "Complete all fields of TASK, and return modified TASK."
  (my-mipt-task
   :class (or (my-mipt-task-class task) (my-mipt-read-class))
   :lesson (or (my-mipt-task-lesson task) (my-mipt-read-lesson))
   :section (or (my-mipt-task-section task) (my-mipt-read-section))
   :kind (or (my-mipt-task-kind task) (my-mipt-read-kind))
   :number (or (my-mipt-task-number task) (my-mipt-read-number))))

(defun my-mipt-task-browse-course-url (&optional task)
  "Browse URL of the MIPT TASK course."
  (interactive (list (my-mipt-current-task)))
  (browse-url
   (my-mipt-task-course-url task)))

(defun my-mipt-task-course-url (task)
  "Return URL to the MIPT course of the TASK."
  (my-mipt-course-url (my-mipt-task-course-name task)))

(defun my-mipt-course-url (course-name)
  "Return URL to the MIPT course named COURSE-NAME."
  (my-mipt-course-ensure-saved-id course-name)
  (->>
   course-name
   (my-mipt-course-id)
   (s-prepend
    "https://zftsh.online/course/")))

(defun my-mipt-course-ensure-saved-id (course-name)
  "Ensure that the MIPT course named COURSE-NAME is saved in the file.

The file is the file at the path `my-mipt-courses-ids-file'.  If the course is
not saved, then save with ID readed from the minibuffer"
  (unless (my-mipt-course-id course-name)
    (let ((prompt (format "ID of the course %s: " course-name)))
      (my-mipt-course-save-id course-name
                              (read-string prompt)))))

(defun my-mipt-task-course-id (task)
  "Return ID of the MIPT TASK course."
  (let ((name (my-mipt-task-course-name task)))
    (my-mipt-course-id name)))

(defun my-mipt-course-id (name)
  "Return ID of the MIPT course named NAME."
  (let ((alist (my-mipt-all-saved-courses-ids)))
    (-second-item
     (assoc name alist))))

(defun my-mipt-task-course-name (task)
  "Return MIPT course of the TASK as string with a form sush as 9-i-1."
  (format "%s-%s-%s"
          (my-mipt-task-class task)
          (my-mipt-task-lesson task)
          (my-mipt-task-section task)))

(defun my-mipt-all-saved-courses-ids ()
  "Return the list of the MIPT courses names and their IDS saved in the file.

The file is file at the path `my-mipt-courses-ids-file'"
  (->>
   my-mipt-courses-ids-file
   (f-read)
   (s-trim)
   (s-lines)
   (--map (s-split " " it))))

(defun my-mipt-task-save-course-id (task id)
  "Save ID of MIPT TASK course in file `my-mipt-courses-ids-file'."
  (my-mipt-course-save-id (my-mipt-task-course-name task)
                          id))

(defun my-mipt-course-save-id (name id)
  "Save ID of MIPT course named NAME in file `my-mipt-courses-ids-file'."
  (->>
   (my-mipt-all-saved-courses-ids)
   (cons (list name id))
   (my-mipt-task-set-saved-courses-ids)))

(defun my-mipt-task-set-saved-courses-ids (courses)
  "Set the list of the MIPT courses IDS and their names to COURSES.

That information will be saved in the file at the path
`my-mipt-courses-ids-file'.  COURSES is list from the list from a MIPT course
ID and a course name."
  (f-write (s-join
            "\n"
            (--map
             (let ((id (car it))
                   (name (-second-item it)))
               (s-concat name " " id))
             courses))
           'utf-8
           my-mipt-courses-ids-file))

(defun fast-exec-mipt-keys ()
  "Get some useful keymaps of  `fast-exec' for MIPT."
  (fast-exec/some-commands
   ("Next MIPT Task" 'my-mipt-next-task)
   ("Previous MIPT Task" 'my-mipt-prev-task)
   ("Open Last MIPT Task" 'my-mipt-visit-last-task)
   ("Find MIPT Task" 'my-mipt-task-visit)
   ("Open MIPT Task in Web Browser" 'my-mipt-task-browse-web)))

(fast-exec/register-keymap-func 'fast-exec-mipt-keys)
(fast-exec/reload-functions-chain)

(provide 'my-mipt)
;;; my-mipt.el ends here