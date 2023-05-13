;;; my-helm-gitignore.el --- Fetch a gitiginore template for special languages using gitiginore.io  -*- lexical-binding: t; -*-

;; Copyright (C) 2023 semenInRussia

;; Author: semenInRussia <hrams205@gmail.com>
;; Version: 0.0.1
;; Package-Requires: ((dash "2.18.0") (s "1.12.0"))

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

;; Fetch a gitiginore template for special languages using gitiginore.io .

;;; Code:

(defvar helm-gitignore--api-url
  "https://www.gitignore.io/api/%s"
  "Url used to generate .gitignore file.")

(defvar helm-gitignore--list-url
  "https://www.gitignore.io/dropdown/templates.json?term=%s"
  "Url used to get list of templates in raw and human-friendly text.")

(defvar helm-gitignore--cache nil
  "Cache that contains results of querying of candidates from gitignore.io.")

(defvar helm-gitignore--source
  (helm-build-sync-source "gitignore.io"
    :candidates 'helm-gitignore--candidates
    :action 'helm-gitignore--action
    :volatile t
    :requires-pattern 1))

(defun helm-gitignore--candidates ()
  "Query for gitignore templates."
  (let ((delay 0.1))
    (unless helm-gitignore--cache
      (run-at-time delay nil 'helm-gitignore--query-candidates helm-pattern))
    (and
     (sit-for delay)
     (prog1 helm-gitignore--cache
       (and helm-gitignore--cache (setq helm-gitignore--cache nil))))))

(defun helm-gitignore--query-candidates (pattern)
  "Query for gitignore templates using given PATTERN."
  (request
    (format helm-gitignore--list-url pattern)
    :parser 'json-read
    :success (cl-function
              (lambda (&key data &allow-other-keys)
                (setq helm-gitignore--cache
                      (mapcar 'helm-gitignore--format-result data))
                (and helm-alive-p (helm-update))))
    :error (cl-function
            (lambda (&key error-thrown &allow-other-keys)
              (message error-thrown)))))

(defun helm-gitignore--action (candidate)
  "Generate .gitignore given at least a CANDIDATE and present to screen."
  (let ((candidates (helm-marked-candidates)))
    (unless candidates (setq candidates (list candidate)))
    (request
      (helm-gitignore--generate-url candidates)
      :parser 'buffer-string
      :success (cl-function
                (lambda (&key data &allow-other-keys)
                  (when data
                    (with-current-buffer (get-buffer-create "*gitignore*")
                      (gitignore-mode)
                      (erase-buffer)
                      (insert data)
                      (goto-char (point-min))
                      (pop-to-buffer (current-buffer))))))
      :error (cl-function
              (lambda (&key error-thrown &allow-other-keys)
                (message error-thrown))))))

(defun helm-gitignore--format-result (result)
  "Pluck data from RESULT to create a Helm candidate."
  (cons (assoc-default 'text result) (assoc-default 'id result)))

(defun helm-gitignore--generate-url (list)
  "Create url from LIST to generate .gitignore using gitignore.io."
  (format helm-gitignore--api-url (mapconcat 'identity list ",")))

;;;###autoload
(defun helm-gitignore ()
  "Helm to generate .gitignore using gitignore.io."
  (interactive)
  (helm :sources 'helm-gitignore--source :buffer "*helm-gitignore*"))

(provide 'my-helm-gitignore)
;;; my-helm-gitignore.el ends here