;;; my-films.el --- My config for films to watch management

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

;; My config for films to watch management

;;; Code:
(require 'kinopoisk)
(require 's)
(require 'org)

;;;###autoload
(defun my-films-add (film)
  "Add FILM to current org file, this file is db of films."
  (interactive (list (my-films--search-new)))
  (org-meta-return)
  (insert "MUST-SEE ")
  (insert (kinopoisk-film-original-name film))
  (my-org-set-props
   (name      . (kinopoisk-film-name film))
   (year      . (kinopoisk-film-year film))
   (slogan    . (kinopoisk-film-slogan film))
   (id        . (kinopoisk-film-id film))
   (rating    . (kinopoisk-film-rating film))
   (countries . (kinopoisk-film-countries film))))

(defun my-films-format-as-org-heading ()
  "Format an `kinopoisk-film' readed from the minibuffer as an org entry."
  (with-temp-buffer
    (org-mode)
    (my-films-add (my-films--search-new))
    (s-concat (buffer-string) "%?")))

(defmacro my-org-set-props (&rest key-and-val)
  "Set properties of org heading with keys from KEY-AND-VAL and values from it."
  (->>
   key-and-val
   (--map
    `(org-set-property
      ,(symbol-name (car it))
      (format "%s" ,(cdr it))))
   (cons 'progn)))

(defun my-films--search-new ()
  "Search film from the Internet."
  ;; I know that instead of `flet', I can use `cl-flet', but `cl-flet'
  ;; redefine funcitons only in body
  (flet
      ((helm-kinopoisk--handle-film (film) film))
    (helm-kinopoisk-search-films)))

(defun my-films--choose-from-top ()
  "Choose one film from the Kinopoisk top."
  ;; I know that instead of `flet', I can use `cl-flet', but `cl-flet'
  ;; redefine funcitons only in body
  (flet
      ((helm-kinopoisk--handle-film (film) film))
    (call-interactively #'helm-kinopoisk-see-films-top)))

(defun my-films-list ()
  "List of films saved in films management file."
  (interactive)
  (helm
   :buffer "*saved-films*"
   :sources '((name       . "List of Saved Films")
              (candidates . my-films--list-candidates)
              (action     . helm-kinonpoisk-search-source))))

(defun my-films--list-candidates ()
  "Helm candidates for `my-films-list'."
  (->>
   (org-map-entries #'my-films--from-org-heading "\+MUST-SEE" 'agenda)
   (--map (cons (helm-kinopoisk--format-film-for-display it) it))))

(defun my-films--from-org-heading ()
  "Parse org heading at current position to `kinopoisk-film'."
  (when (just-line-prefix-p "*")
    (kinopoisk-film
     :id (string-to-number (car (org-property-values "id")))
     :year (car (org-property-values "year"))
     :name (car (org-property-values "name")))))

(provide 'my-films)
;;; my-films.el ends here
