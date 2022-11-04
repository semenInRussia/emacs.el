;;; my-org-db.el --- An org database

;;; Commentary:
;; my-org-db is used to index org-files into a sqlite database to make searching
;; easier. It is complementary to things like `org-agenda-files'. I have found
;; that `org-agenda' is too slow to deal with large (thousands+) of org-files
;; that are spread all over your directories.
;;
;; It works with a `save-buffer-hook' that runs a function that adds an org-file
;; to a queue for indexing when you save it. The queue is processed whenever
;; Emacs is idle. This is done for performance reasons.
;; 1. my-org-db parses the org-file which can be slow for large files, and I don't want to wait while working.
;; 2. sqlite can only have one process working on it at a time, so async updates is not a good idea.
;;
;; my-org-db balances performance and accuracy in a way that works "well enough"
;; for me. There are a number of ways it can be out of sync and inaccurate
;; though. The main way is if files get changed outside of emacs, e.g. by git,
;; cloud drive sync programs, or other users in shared drives, files are moved
;; or renamed, etc. my-org-db doesn't have a good way to keep up with these kinds
;; of changes. You can use `M-x C-u my-org-db-refresh' to update all the files in
;; the database.
;;
;; Similarly, my-org-db will generally not know about files you have never opened.
;;
;; The main entry points for you are:
;; `my-org-db-headings' actions for headings in the database - default open
;; `my-org-db-contacts' actions for headings with an EMAIL property - default open
;; `my-org-db-locations' actions for headings with an ADDRESS property - default open
;; `my-org-db-files'  open a file in the db
;; `my-org-db-recentf' open a recent file in the db
;; `my-org-db-links' actions for links in the db
;; `my-org-db-hashtags' actions for hashtags
;; `my-org-db-@' actions for @-labels
;; `my-org-db-properties' searches properties, although I don't find it that useful
;; `my-org-db-editmarks' searches your editmarks
;; `my-org-db-email-addresses' finds email addresses in your files.
;; `my-org-db-src-blocks' search src-blocks

;; `my-org-db-toggle-org-id' If you want to use my-org-db to jump to an org-id instead
;; of `org-id-goto'. I find the built-in function to slow for me.
;;
;; Utilities
;; `my-org-db-index' will prompt you for a directory and index the org-files in that directory. Use a prefix arg to make it recursive.
;; `my-org-db-clean-db' will prune entries where the file no longer exists.
;; `my-org-db-reset-db' will clear all the entries from my-org-db if you want to start over.
;;
;; Advanced usage
;; you can build emacsql queries on my-org-db to do lots of things.


;;; Code:
(require 'cl-lib)
(require 's)    ; for s-trim
(require 'org)
(leaf emacsql-sqlite :ensure t :require t)
(leaf hydra :ensure t :require t)
(leaf ivy :ensure t :require t)


(defcustom my-org-db-root "~/my-org-db/"
  "Root directory for db files."
  :type 'directory
  :group 'my-org-db)


(defcustom my-org-db-name "my-org-db.sqlite"
  "Name of the sqlite database file."
  :type 'string
  :group 'my-org-db)


(defcustom my-org-db-update-functions
  '(my-org-db-update-headlines
    my-org-db-update-links
    my-org-db-update-keywords
    my-org-db-update-src-blocks
    my-org-db-update-hashtags
    my-org-db-update-@-labels
    my-org-db-update-email-addresses
    my-org-db-update-editmarks
    my-org-db-update-targets)
  "List of functions to run when updating a file in `my-org-db'.
Each function takes arguments that are the filename-id, and the
parse tree from `org-element-parse-buffer'. Your function should
delete old data if needed, and insert or update data as needed."
  :group 'my-org-db)


(unless (file-directory-p my-org-db-root)
  (make-directory my-org-db-root t))


(defvar my-org-db
  (emacsql-sqlite (expand-file-name my-org-db-name my-org-db-root))
  "Variable for the ‘my-org-db’ connection.")


(defvar my-org-db-queue
  '()
  "A list of files that need to be updated.")


(defvar my-org-db-log-file
  (expand-file-name "my-org-db.log" my-org-db-root)
  "Path to the log file.")


(defvar my-org-db-ignore-file-regexps
  '(".*.gpg$" "\\.dropbox")
  "A list of regexps of files (including their path) to ignore.")


(defvar my-org-db-ignore-tags
  '()
  "A list of tags to exclude from the database.")


(defvar my-org-db-ignore-properties
  '("RESULT")
  "A list of properties to exclude from the database.")


(defvar my-org-db-ignore-keywords
  '( )
  "A list of keywords to exclude from the database.")


(defvar my-org-db-debug nil "If non-nil log messages.")


(defvar hashtag-rx
  (rx
   (:
    ;; hashtags begin at beginning of line, a blank, or
    ;; after these punctuations. I am not sure what the
    ;; "punctuation" group contains, so I am explicit here
    ;; to also include the brackets.
    (or bol blank (in ",.;:?!(){}[]<>"))
    (= 1 "#")
    (group-n 1
             ;; hashtags do not start with numbers
             ;; or # + punctuation or a space
             (not (in digit punctuation "#+' "))
             ;; The rest of the chars cannot be a space or punctuation
             (one-or-more
              (one-or-more (not (in space punctuation)))
              ;; the words can be joined by - or _
              (zero-or-one (in "-_"))))
    ;; hashtags end at blanks, end of line or punctuation
    (or blank eol punct)))
  "Regular expression to match hashtags.")


(defvar @-rx
  (rx
   (:
    ;; @label begin at beginning of line, a blank, or
    ;; after these punctuations. I am not sure what the
    ;; "punctuation" group contains, so I am explicit here
    ;; to also include the brackets.
    (or bol blank (in ",.;:?!(){}[]<>"))
    (= 1 "@")
    (group-n 1
             ;; The rest of the chars cannot be a space or punctuation
             (one-or-more
              (one-or-more (not (in space punctuation)))
              ;; the words can be joined by - or _
              (zero-or-one (in "-_"))))
    ;; @labels end at blanks, end of line or punctuation
    (or blank eol punct)))
  "Regular expression to match @labels.
Maybe not surprisingly, there are a lot of false positives.
Sometimes, this seems to match emails, it matches things like
decorators in Python, etc.
")


(defvar email-rx
  "<?\\([-+_.~a-zA-Z][-+_.~:a-zA-Z0-9]*@[-.a-zA-Z0-9]+\\)>?"
  "Regular expression for email addresses. Adapted from `thing-at-point-email-regexp'.")


(defun my-org-db-log (format-string &rest args)
  "Insert the FORMAT-STRING formatted with ARGS into log buffer."
  (when my-org-db-debug
    (with-current-buffer (get-buffer-create "*my-org-db-log*")
      (goto-char (point-max))
      (insert (format "%s:\n" (current-time-string)))
      (let* ((msg
              (with-temp-buffer
                (insert
                 (format "%s\n" (apply 'format format-string args)))
                (fill-region (point-min) (point-max))
                (indent-region (point-min) (point-max) 4)
                (buffer-string))))
        (insert msg)))))

(my-org-db-log "Started my-org-db")

;; create the tables if we need to.
(emacsql my-org-db [:PRAGMA (= foreign_keys 1)])

(emacsql my-org-db [:create-table :if :not :exists files
                                  ([(rowid integer :primary-key)
                                    (filename text :unique)
                                    (md5 text)
                                    (last-updated text)
                                    ])
                                  ])

(emacsql my-org-db [:create-table :if :not :exists tags
                                  ([(rowid integer :primary-key)
                                    (tag text :unique)
                                    ])
                                  ])

(emacsql my-org-db [:create-table :if :not :exists properties
                                  ([(rowid integer :primary-key)
                                    (property text :unique)
                                    ])
                                  ])

(emacsql my-org-db [:create-table :if :not :exists keywords
                                  ([(rowid integer :primary-key)
                                    (keyword text :unique)
                                    ])
                                  ])

(emacsql my-org-db [:create-table :if :not :exists headlines
                                  ([(rowid integer :primary-key)
                                    (filename-id integer :not :null)
                                    (title text :not :null)
                                    (level integer :not :null)
                                    (todo-keyword text)
                                    (todo-type text)
                                    archivedp
                                    commentedp
                                    footnote-section-p
                                    (begin integer :not :null)
                                    (tags text)
                                    (priority text)
                                    ]
                                   (:foreign-key [filename-id] :references files [rowid]
                                                 :on-delete :cascade))
                                  ])

(emacsql my-org-db [:create-table :if :not :exists headline-tags
                                  ([(rowid integer :primary-key)
                                    (headline-id integer)
                                    (tag-id integer)
                                    ]
                                   (:foreign-key [headline-id] :references headlines [rowid]
                                                 :on-delete :cascade)
                                   (:foreign-key [tag-id] :references tags [rowid] :on-delete :cascade))
                                  ])


(emacsql my-org-db [:create-table :if :not :exists headline-properties
                                  ([(rowid integer :primary-key)
                                    (headline-id integer)
                                    (property-id integer)
                                    (value text)
                                    ]
                                   (:foreign-key [headline-id] :references headlines [rowid]
                                                 :on-delete :cascade)
                                   (:foreign-key [property-id] :references properties [rowid]
                                                 :on-delete :cascade))
                                  ])


(emacsql my-org-db [:create-table :if :not :exists file-keywords
                                  ([(rowid integer :primary-key)
                                    (filename-id integer)
                                    (keyword-id integer)
                                    (value text)
                                    (begin integer)
                                    ]
                                   (:foreign-key [filename-id] :references files [rowid] :on-delete :cascade)
                                   (:foreign-key [keyword-id] :references keywords [rowid]
                                                 :on-delete :cascade))
                                  ])


(emacsql my-org-db [:create-table :if :not :exists links
                                  ([(rowid integer :primary-key)
                                    (filename-id integer)
                                    (type text)
                                    (path text)
                                    (raw-link text)
                                    (description text)
                                    (search-option text)
                                    (begin integer)
                                    ]
                                   (:foreign-key [filename-id] :references files [rowid]
                                                 :on-delete :cascade))
                                  ])

;; targets
(emacsql my-org-db [:create-table :if :not :exists targets
                                  ([(rowid integer :primary-key)
                                    (target text :unique)
                                    ])
                                  ])

(emacsql my-org-db [:create-table :if :not :exists file-targets
                                  ([(rowid integer :primary-key)
                                    (filename-id integer)
                                    (target-id integer)
                                    (begin integer)
                                    ]
                                   (:foreign-key [filename-id] :references files [rowid] :on-delete :cascade)
                                   (:foreign-key [target-id] :references targets [rowid]
                                                 :on-delete :cascade))
                                  ])

;; hashtags
(emacsql my-org-db [:create-table :if :not :exists hashtags
                                  ([(rowid integer :primary-key)
                                    (hashtag text :unique)
                                    ])
                                  ])

(emacsql my-org-db [:create-table :if :not :exists file-hashtags
                                  ([(rowid integer :primary-key)
                                    (filename-id integer)
                                    (hashtag-id integer)
                                    (begin integer)
                                    ]
                                   (:foreign-key [filename-id] :references files [rowid] :on-delete :cascade)
                                   (:foreign-key [hashtag-id] :references hashtags [rowid]
                                                 :on-delete :cascade))
                                  ])

;; @labels
(emacsql my-org-db [:create-table :if :not :exists atlabels
                                  ([(rowid integer :primary-key)
                                    (atlabel text :unique)
                                    ])
                                  ])

(emacsql my-org-db [:create-table :if :not :exists file-atlabels
                                  ([(rowid integer :primary-key)
                                    (filename-id integer)
                                    (atlabel-id integer)
                                    (begin integer)
                                    ]
                                   (:foreign-key [filename-id] :references files [rowid] :on-delete :cascade)
                                   (:foreign-key [atlabel-id] :references atlabels [rowid]
                                                 :on-delete :cascade))
                                  ])

;; emails
(emacsql my-org-db [:create-table :if :not :exists email-addresses
                                  ([(rowid integer :primary-key)
                                    (email-address text :unique)
                                    ])
                                  ])


;; email-addresses
(emacsql my-org-db [:create-table :if :not :exists file-email-addresses
                                  ([(rowid integer :primary-key)
                                    (filename-id integer)
                                    (email-address-id integer)
                                    (begin integer)
                                    ]
                                   (:foreign-key [filename-id] :references files [rowid] :on-delete :cascade)
                                   (:foreign-key [email-address-id] :references email-addresses [rowid]
                                                 :on-delete :cascade))
                                  ])


;; editmarks
(emacsql my-org-db [:create-table :if :not :exists file-editmarks
                                  ([(rowid integer :primary-key)
                                    (filename-id integer)
                                    (type text)
                                    (content text)
                                    (begin integer)
                                    ]
                                   (:foreign-key [filename-id] :references files [rowid] :on-delete :cascade))
                                  ])

;; src-blocks
(emacsql my-org-db [:create-table :if :not :exists src-blocks
                                  ([(rowid integer :primary-key)
                                    (filename-id integer)
                                    (language text)
                                    (contents text)
                                    (begin integer)
                                    ]
                                   (:foreign-key [filename-id] :references files [rowid] :on-delete :cascade))
                                  ])


(defun my-org-db-connect ()
  "Make sure we are connected."
  (unless (and my-org-db (emacsql-live-p my-org-db))
    (setq my-org-db
          (emacsql-sqlite
           (expand-file-name my-org-db-name my-org-db-root)))))


(defun my-org-db-close ()
  "Close the connection."
  (interactive)
  (when my-org-db
    ;; It seems like you need to call this twice before my-org-db is nil
    (emacsql-close my-org-db)
    (emacsql-close my-org-db)))


(add-hook 'kill-emacs-hook #'my-org-db-close)


(set-process-query-on-exit-flag (emacsql-process my-org-db) nil)


(defun my-org-db-get-filename-id (fname)
  "Return the rowid corresponding to FNAME.
Adds FNAME to the database if it doesn't exist."
  (my-org-db-connect)
  (or
   ;; this is a file in the database
   (caar
    (emacsql my-org-db [:select rowid :from files
                                :where (= filename $s1)
                                ]
             fname))
   ;; no file found, we add one and get the id.
   (prog2
       (emacsql my-org-db [:insert :into files :values [nil $s1 $s2 $s3]]
                (buffer-file-name)
                (md5 (buffer-string))
                nil)
       (caar
        (emacsql my-org-db [:select (funcall last-insert-rowid)])))))


(defun my-org-db-remove-buffer ()
  "Remove the current buffer from the database."
  (interactive)
  (let* ((filename-id (my-org-db-get-filename-id (buffer-file-name))))
    ;; delete links
    (emacsql my-org-db [:delete :from links :where
                                (= links:filename-id $s1)
                                ] filename-id)

    ;; keywords
    (emacsql my-org-db [:delete :from file-keywords
                                :where (= file-keywords:filename-id $s1)
                                ]
             filename-id)

    ;; headlines
    (emacsql my-org-db [:delete :from headlines :where
                                (= headlines:filename-id $s1)
                                ]
             filename-id)

    ;; and the file
    (emacsql my-org-db [:delete :from files :where
                                (= rowid $s2)
                                ] filename-id))
  (my-org-db-log "Removed %s from the database." (buffer-file-name)))

;; * Update the database for a buffer
;;
;; [2021-09-14 Tue] I refactored this into a sequence of functions that are run
;; in each buffer.

(defun my-org-db-update-src-blocks (filename-id parse-tree)
  "Update the src blocks in the buffer."
  ;; delete old entries
  (emacsql my-org-db [:delete :from src-blocks
                              :where (= src-blocks:filename-id $s1)
                              ]
           filename-id)
  (org-babel-map-src-blocks nil

    (emacsql my-org-db [:insert :into src-blocks :values [nil $s1 $s2 $s3 $s4]]
             filename-id lang body beg-block)))

(defun my-org-db-update-keywords (filename-id parse-tree)
  "Updates the keyword table for the org-buffer.
FILENAME-ID is the rowid for the org-file.
PARSE-TREE is from `org-element-parse-buffer'."
  (let ((keywords
         (org-element-map parse-tree 'keyword
           (lambda (kw)
             (list
              (upcase (org-element-property :key kw))
              (org-element-property :value kw)
              (org-element-property :begin kw)))))
        keyword-id)
    ;; * File keywords.
    (emacsql my-org-db [:delete :from file-keywords
                                :where (= file-keywords:filename-id $s1)
                                ]
             filename-id)

    ;; For each keyword, get the id or add to the keywords table and get the id.
    (cl-loop for
             (keyword value begin)
             in keywords
             if
             (not (member keyword my-org-db-ignore-keywords))
             do
             (setq keyword-id
                   (or
                    (caar
                     (emacsql my-org-db [:select rowid :from keywords
                                                 :where (= keyword $s1)
                                                 ]
                              keyword))
                    (emacsql my-org-db [:insert :into keywords :values [nil $s1]]
                             keyword)
                    (caar
                     (emacsql my-org-db
                              [:select (funcall last-insert-rowid)]))))
             ;; Now add to the file-keywords
             (emacsql my-org-db [:insert :into file-keywords :values [nil $s1 $s2 $s3 $s4]]
                      filename-id keyword-id value begin))))


(defun my-org-db-update-hashtags (filename-id parse-tree)
  "Update hashtags in the buffer.
FILENAME-ID is the rowid for the org-file.
PARSE-TREE is from `org-element-parse-buffer'."
  ;; "#\\([^0-9!$%^&*+.]\\(\\w+[-_]?\\)+\\)"
  (let ((hashtags
         (save-excursion
           (goto-char (point-min))
           (let ((hashtags '()))
             (while (re-search-forward hashtag-rx nil t)
               ;; there are many scenarios we do not want to count these.
               ;; no src-blocks as these are often false matches for comments
               ;; These are not always reliable as it relies on font-lock
               (when (and
                      (not (org-in-src-block-p))
                      ;; these are formulas in org lines sometimes
                      (not (eq 'org-meta-line (face-at-point)))
                      (not (save-match-data
                             (equal 'src-block (car (org-element-context))))))
                 (push
                  (cons
                   (match-string-no-properties 1)
                   (match-beginning 0))
                  hashtags)))
             hashtags)))
        hashtag-id)

    ;; hashtags
    ;; first delete existing data on hashtags because it has probably changed.
    (emacsql my-org-db [:delete :from file-hashtags
                                :where (= file-hashtags:filename-id $s1)
                                ]
             filename-id)


    (cl-loop for
             (hashtag . pos)
             in hashtags do
             ;; get hashtag id
             (setq hashtag-id
                   (or
                    (caar
                     (emacsql my-org-db [:select rowid :from hashtags
                                                 :where (= hashtag $s1)
                                                 ]
                              hashtag))
                    (emacsql my-org-db [:insert :into hashtags :values [nil $s1]]
                             hashtag)
                    (caar
                     (emacsql my-org-db
                              [:select (funcall last-insert-rowid)]))))

             (emacsql my-org-db [:insert :into file-hashtags :values [nil $s1 $s2 $s3]]
                      filename-id hashtag-id pos))))


(defun my-org-db-update-targets (filename-id parse-tree)
  "Update the targets table."
  ;; delete entries from the targets table
  (emacsql my-org-db [:delete :from file-targets :where
                              (= file-targets:filename-id $s1)
                              ] filename-id)

  (org-element-map parse-tree 'target
    (lambda (target)
      (let ((target-id
             (or
              (caar
               (emacsql my-org-db [:select rowid :from targets
                                           :where (= target $s1)
                                           ]
                        (org-element-property :value target)))
              (emacsql my-org-db [:insert :into targets :values [nil $s1]]
                       (org-element-property :value target))
              (caar
               (emacsql my-org-db [:select
                                   (funcall last-insert-rowid)
                                   ])))))

        (emacsql my-org-db [:insert :into file-targets :values [nil $s1 $s2 $s3]]
                 filename-id target-id
                 (org-element-property :begin target))))))


(defun my-org-db-update-editmarks (filename-id parse-tree)
  "Update the editmarks table in a buffer.
FILENAME-ID is the rowid for the org-file.
PARSE-TREE is from `org-element-parse-buffer'."
  (when (fboundp 'sem-get-editmarks)
    (emacsql my-org-db [:delete :from file-editmarks
                                :where (= file-editmarks:filename-id $s1)
                                ]
             filename-id)
    (cl-loop for
             (em-type buffer (start . end) em-content)
             in
             (sem-get-editmarks)
             do
             (emacsql my-org-db [:insert :into file-editmarks
                                         :values [nil $s1 $s2 $s3 $s4]]
                      filename-id
                      em-type
                      em-content
                      start))))


(defun my-org-db-update-email-addresses (filename-id parse-tree)
  "Update emails table.
FILENAME-ID is the rowid for the org-file.
PARSE-TREE is from `org-element-parse-buffer'."
  (let ((emailaddresses
         (save-excursion
           (goto-char (point-min))
           (let ((emailaddresses '()))
             (while (re-search-forward email-rx nil t)
               ;; there are many scenarios we do not want to count these.
               ;; no src-blocks as these are often false matches for comments
               ;; These are not always reliable as it relies on font-lock
               (when (and
                      (not (org-in-src-block-p))
                      (not (save-match-data
                             (equal 'src-block (car (org-element-context))))))
                 (push
                  (cons
                   (match-string-no-properties 1)
                   (match-beginning 0))
                  emailaddresses)))
             emailaddresses)))
        email-address-id)

    (emacsql my-org-db [:delete :from file-email-addresses
                                :where (= file-email-addresses:filename-id $s1)
                                ]
             filename-id)

    (cl-loop for
             (email-address . pos)
             in emailaddresses do
             ;; get email-address-id
             (setq email-address-id
                   (or
                    (caar
                     (emacsql my-org-db [:select rowid :from email-addresses
                                                 :where (= email-address $s1)
                                                 ]
                              email-address))
                    (emacsql my-org-db [:insert :into email-addresses :values [nil $s1]]
                             email-address)
                    (caar
                     (emacsql my-org-db
                              [:select (funcall last-insert-rowid)]))))

             (emacsql my-org-db [:insert :into file-email-addresses :values [nil $s1 $s2 $s3]]
                      filename-id email-address-id pos))))


(defun my-org-db-update-@-labels (filename-id parse-tree)
  "Update @labels.
FILENAME-ID is the rowid for the org-file.
PARSE-TREE is from `org-element-parse-buffer'."
  (let ((atlabels
         (save-excursion
           (goto-char (point-min))
           (let ((atlabels '()))
             (while (re-search-forward @-rx nil t)
               ;; there are many scenarios we do not want to count these.
               ;; no src-blocks as these are often false matches for comments
               ;; These are not always reliable as it relies on font-lock
               (when (and
                      (not (org-in-src-block-p))
                      ;; these are formulas in org lines sometimes
                      (not (eq 'org-meta-line (face-at-point)))
                      (not (save-match-data
                             (equal 'src-block (car (org-element-context))))))
                 (push
                  (cons
                   (match-string-no-properties 1)
                   (match-beginning 0))
                  atlabels)))
             atlabels)))
        atlabel-id)
    (emacsql my-org-db [:delete :from file-atlabels
                                :where (= file-atlabels:filename-id $s1)
                                ]
             filename-id)


    (cl-loop for
             (atlabel . pos)
             in atlabels do
             ;; get atlabel id
             (setq atlabel-id
                   (or
                    (caar
                     (emacsql my-org-db [:select rowid :from atlabels
                                                 :where (= atlabel $s1)
                                                 ]
                              atlabel))
                    (emacsql my-org-db [:insert :into atlabels :values [nil $s1]]
                             atlabel)
                    (caar
                     (emacsql my-org-db
                              [:select (funcall last-insert-rowid)]))))

             (emacsql my-org-db [:insert :into file-atlabels :values [nil $s1 $s2 $s3]]
                      filename-id atlabel-id pos))))


(defun my-org-db-update-links (filename-id parsetree)
  "Update links table.
FILENAME-ID is the rowid for the org-file.
PARSE-TREE is from `org-element-parse-buffer'."
  (let* ((links
          (org-element-map parse-tree 'link
            (lambda (link)
              (vector
               nil
               filename-id
               (org-element-property :type link)
               (org-element-property :path link)
               (org-element-property :raw-link link)
               (if (org-element-property :contents-begin link)
                   (buffer-substring-no-properties
                    (org-element-property :contents-begin link)
                    (org-element-property :contents-end link))
                 "")
               (org-element-property :search-option link)
               (org-element-property :begin link)))))
         ;; WHAT IS THIS DOING????
         (ba (-split-at 400 links))
         (handle (nth 0 ba))
         (next (nth 1 ba)))
    ;; * delete old links
    (emacsql my-org-db [:delete :from links :where
                                (= links:filename-id $s1)
                                ] filename-id)

    ;;  ** add new links
    (while handle
      (emacsql my-org-db [:insert :into links :values $v1] handle)
      (setq ba
            (-split-at 400 next)
            handle
            (nth 0 ba)
            next
            (nth 1 ba)))))


(defun my-org-db-update-headlines (filename-id parsetree)
  "Update headlines table.
FILENAME-ID is the rowid for the org-file.
PARSE-TREE is from `org-element-parse-buffer'."
  (let* ((headlines (org-element-map parse-tree 'headline 'identity))
         hlv headline-id
         tags tag-id
         properties property-id)

    (emacsql my-org-db [:delete :from headlines :where
                                (= headlines:filename-id $s1)
                                ]
             filename-id)

    (cl-loop for hl in headlines do
             (save-excursion
               (goto-char (org-element-property :begin hl))
               (setq tags
                     (mapcar 'org-no-properties (org-get-tags))
                     properties
                     (org-entry-properties
                      (org-element-property :begin hl)
                      'all)))

             (setq hlv
                   (vector
                    nil
                    filename-id
                    (org-element-property :raw-value hl)
                    (org-element-property :level hl)
                    (when (org-element-property :todo-keyword hl)
                      (substring-no-properties
                       (org-element-property :todo-keyword hl)))
                    (org-element-property :todo-type hl)
                    (org-element-property :archivedp hl)
                    (org-element-property :commentedp hl)
                    (org-element-property :footnote-section-p hl)
                    (org-element-property :begin hl)
                    ;; this is really a tag string for easy searching in
                    ;; ivy because it seems tricky to build this from a
                    ;; query
                    (when tags
                      (concat ":"
                              (mapconcat
                               'substring-no-properties
                               tags ":")
                              ":"))
                    (if (org-element-property :priority hl)
                        (char-to-string
                         (org-element-property :priority hl))
                      nil)))

             ;; insert headline row and get headline-id
             (emacsql my-org-db [:insert :into headlines :values $v1] hlv)
             (setq headline-id
                   (caar
                    (emacsql my-org-db
                             [:select (funcall last-insert-rowid)])))

             ;; remove old tag data
             (emacsql my-org-db [:delete :from headline-tags
                                         :where (= headline-tags:headline-id $s1)
                                         ]
                      headline-id)

             (cl-loop for tag in tags
                      if
                      (not (member tag my-org-db-ignore-tags))
                      do
                      (setq tag-id
                            (or
                             (caar
                              (emacsql my-org-db [:select rowid :from tags
                                                          :where (= tag $s1)
                                                          ]
                                       tag))
                             (emacsql my-org-db [:insert :into tags :values [nil $s1]]
                                      tag)
                             (caar
                              (emacsql my-org-db [:select
                                                  (funcall last-insert-rowid)
                                                  ]))))
                      (emacsql my-org-db [:insert :into headline-tags :values $v1]
                               (vector nil headline-id tag-id)))

             ;; properties
             (emacsql my-org-db [:delete :from headline-properties
                                         :where (= headline-properties:headline-id $s1)
                                         ]
                      headline-id)

             (setq properties
                   (save-excursion
                     (goto-char (org-element-property :begin hl))
                     (org-entry-properties)))

             (cl-loop for
                      (property . value)
                      in properties
                      if
                      (not (member property my-org-db-ignore-properties))
                      do
                      (setq property-id
                            (or
                             (caar
                              (emacsql my-org-db [:select rowid :from properties
                                                          :where (= property $s1)
                                                          ]
                                       property))
                             (emacsql my-org-db [:insert :into properties
                                                         :values [nil $s1]]
                                      property)
                             (caar
                              (emacsql my-org-db [:select
                                                  (funcall last-insert-rowid)
                                                  ]))))

                      ;; and the values
                      (emacsql my-org-db [:insert :into headline-properties
                                                  :values [nil $s1 $s2 $s3]]
                               headline-id
                               property-id
                               (org-no-properties value))))))


;; ** update a buffer

(defun my-org-db-update-buffer (&optional force)
  "Update the entries in the database for the currently visited buffer.
Optional argument FORCE. if non-nil force the buffer to be added."
  (interactive "P")
  (my-org-db-connect)
  (save-buffer)
  (my-org-db-log "Updating in buffer: %s" (buffer-file-name))
  (org-with-wide-buffer
   (when (or force
             (and
              ;; file does not match an ignore pattern
              (and my-org-db-ignore-file-regexps
                   (not (string-match
                         (regexp-opt my-org-db-ignore-file-regexps)
                         (buffer-file-name))))
              ;; file is not in database
              (null
               (caar
                (emacsql my-org-db [:select [md5] :from files
                                            :where (= filename $s1)
                                            ]
                         (buffer-file-name))))
              (my-org-db-log "%s is a new file" (buffer-file-name)))
             (and
              ;; file does not match an ignore pattern
              (and my-org-db-ignore-file-regexps
                   (not (string-match
                         (regexp-opt my-org-db-ignore-file-regexps)
                         (buffer-file-name))))
              ;; file is in database and it has changed
              (prog1
                  (not (string=
                        (md5 (buffer-string))
                        (caar
                         (emacsql my-org-db [:select [md5] :from files
                                                     :where (= filename $s1)
                                                     ]
                                  (buffer-file-name)))))
                (my-org-db-log "%s has changed." (buffer-file-name)))))
     (let* ((filename-id
             (my-org-db-get-filename-id (buffer-file-name)))
            (parse-tree (org-element-parse-buffer)))

       ;; update the md5 for the file so we can tell later if it has changed.
       (emacsql my-org-db [:update files :set
                                   (= md5 $s1)
                                   :where (= rowid $s2)
                                   ]
                (md5 (buffer-string))
                filename-id)

       (cl-loop for update-func in my-org-db-update-functions do
                (funcall update-func filename-id parse-tree))

       (emacsql my-org-db [:update files :set
                                   (= last-updated $s1)
                                   :where (= rowid $s2)
                                   ]
                (format-time-string "%Y-%m-%d %H:%M:%S")
                filename-id)))))



;; * the hooks
(defun my-org-db-hook-function ()
  "Function to run after starting ‘org-mode’."
  ;; Run when we open in case it changed from some external program. Only for
  ;; org and org_archive files, and not just when we enter org-mode for some
  ;; reason.
  (when (and
         (buffer-file-name)
         (or
          (f-ext? (buffer-file-name) "org")
          (f-ext? (buffer-file-name) "org_archive")))

    (add-to-list 'my-org-db-queue (buffer-file-name) t)
    (my-org-db-log "added %s to the queue." (buffer-file-name))

    ;; add local after save hook in case this is a new file.
    (add-hook 'after-save-hook 'my-org-db-hook-function t t)))


(add-hook 'org-mode-hook 'my-org-db-hook-function)


;; * Idle timer to update

(defun my-org-db-process-queue (&optional force)
  "Update all the files in `my-org-db-queue'.
Use a prefix ARG to FORCE the process instead of waiting for idle time."
  (interactive "P")
  (my-org-db-connect)
  (catch 'done
    (while my-org-db-queue
      (unless (or force (current-idle-time)) (throw 'done nil))
      (my-org-db-log "Updating my-org-db for files %s." my-org-db-queue)
      (let* ((filename (pop my-org-db-queue))
             (org-mode-hook '())
             (enable-local-variables nil)
             (already-open (find-buffer-visiting filename))
             (buf (find-file-noselect filename)))
        (my-org-db-log "Updating %s" filename)
        (with-current-buffer buf (my-org-db-update-buffer force))
        (unless already-open (kill-buffer buf)))))
  (my-org-db-log "Done processing my-org-db queue."))

;; if we are idle for 5 minutes, process the queue.
(setq my-org-db-timer
      (run-with-idle-timer (* 60 5) t 'my-org-db-process-queue))

(defun my-org-db-status ()
  "Print a message of files scheduled for update."
  (interactive)
  (my-org-db-log "Files in queue for update: %s" my-org-db-queue)
  (switch-to-buffer "*my-org-db-log*"))



;; * Update the whole database

(defun my-org-db-refresh (&optional force)
  "Update all the files in the database.

Use a prefix arg to FORCE updates."
  (interactive "P")
  (let* ((files
          (emacsql my-org-db [:select [filename] :from files :order-by rowid :asc]))
         (N (length files))
         (enable-local-variables nil)
         (org-mode-hook '())
         buf already-open)
    (cl-loop for
             (fname)
             in files for i from 0 to N
             if
             (and fname (file-exists-p fname))
             do
             (my-org-db-log "Refreshing %s of %s (%s)" i N fname)
             (setq already-open (find-buffer-visiting fname))
             (setq buf (find-file-noselect fname))
             (with-current-buffer buf
               (condition-case err
                   (my-org-db-update-buffer force)
                 (my-org-db-log "Error updating %s: %s" fname err)))
             (unless already-open (kill-buffer buf))
             else
             do
             (my-org-db-log "deleting %s from the database." fname)
             (emacsql my-org-db [:delete :from files :where
                                         (= filename $s1)
                                         ] fname))))


(defun my-org-db-index (path  &optional recursive)
  "Index all the org-files in PATH.
Optional RECURSIVE is non-nil find files recursively."
  (interactive
   (list (read-directory-name "Path: ") current-prefix-arg))
  (let* ((enable-local-variables nil)
         (org-mode-hook '())
         already-open buf
         (files
          (f-files path
                   (lambda (f)
                     (and
                      (or (f-ext? f "org")
                          ;; I am not sure we should index these.
                          ;; (f-ext? f "org_archive")
                          )
                      (and my-org-db-ignore-file-regexps
                           (not (string-match
                                 (regexp-opt my-org-db-ignore-file-regexps)
                                 f)))))
                   recursive))
         (N (length files)))
    (cl-loop for fname in files
             for i from 1
             do
             (my-org-db-log "%s of %s - %s" i N fname)
             (setq already-open (find-buffer-visiting fname))
             (with-current-buffer (or already-open
                                      (setq buf (find-file-noselect fname)))
               (condition-case err
                   (my-org-db-update-buffer)
                 (my-org-db-log "Error updating %s: %s" fname err)))
             (unless already-open (kill-buffer buf)))))


(defun my-org-db-clean-db ()
  "Remove entries from the database where the file does not exist."
  (cl-loop for
           (fname)
           in
           (emacsql my-org-db [:select :distinct [filename] :from files])
           unless
           (file-exists-p fname)
           do
           (my-org-db-log "%s was not found. Removing it." fname)
           (emacsql my-org-db [:delete :from files :where
                                       (= filename $s1)
                                       ] fname)))


(defun my-org-db-reset-db ()
  "Clear all entries from all tables."
  (interactive)
  (emacsql my-org-db [:delete :from files])
  (emacsql my-org-db [:delete :from tags])
  (emacsql my-org-db [:delete :from properties])
  (emacsql my-org-db [:delete :from keywords])
  (emacsql my-org-db [:delete :from headlines])
  (emacsql my-org-db [:delete :from headline-tags])
  (emacsql my-org-db [:delete :from headline-properties])
  (emacsql my-org-db [:delete :from file-keywords])
  (emacsql my-org-db [:delete :from links])
  (emacsql my-org-db [:delete :from hashtags])
  (emacsql my-org-db [:delete :from file-hashtags])
  (emacsql my-org-db [:delete :from atlabels])
  (emacsql my-org-db [:delete :from file-atlabels])
  (emacsql my-org-db [:delete :from email-addresses])
  (emacsql my-org-db [:delete :from file-email-addresses])
  (emacsql my-org-db [:delete :from src-blocks])
  (emacsql my-org-db [:delete :from file-editmarks])
  (emacsql my-org-db [:delete :from file-targets])
  (emacsql my-org-db [:delete :from targets])
  (my-org-db-log "Everything should be reset."))



;; * my-org-db contacts

(defun my-org-db-contacts-candidates ()
  "List of headings with EMAIL properties."
  (let ((contacts
         (emacsql my-org-db
                  [:select [headlines:title
                            headline-properties:value
                            headlines:tags files:filename files:last-updated headlines:begin]
                           :from headlines
                           :inner :join headline-properties
                           :on (=  headlines:rowid headline-properties:headline-id)
                           :inner :join properties
                           :on (= properties:rowid headline-properties:property-id)
                           :inner :join files :on
                           (= files:rowid headlines:filename-id)
                           :where (= properties:property "EMAIL")
                           ])))
    (cl-loop for
             (title email tags fname last-updated begin)
             in contacts
             collect
             (list
              (format "%30s | %40s | %s"
                      (s-pad-right 30 " " (s-trim title))
                      (s-pad-right 40 " " email)
                      (or tags ""))
              :filename fname :last-updated last-updated :begin begin :email email :title
              (s-trim title)))))


(defun my-org-db--insert-contact-link (x)
  "Insert the contact associated with X."
  (let ((link)
        (candidate (cdr x)))
    ;; check if the file is up-to-date
    ;; (let ((actual-mod-time (float-time (file-attribute-modification-time
    ;;                                          (file-attributes (plist-get candidate :filename))))))
    ;;   (when (org-time<= (plist-get candidate :last-updated) actual-mod-time)
    ;;          (warn "%s is not up to date in my-org-db." (plist-get candidate :filename))
    ;;          (with-current-buffer (find-file-noselect (plist-get candidate :filename))
    ;;            (save-buffer)
    ;;            (my-org-db-update-buffer t))))
    ;; (save-excursion
    ;;   (with-current-buffer
    ;;            (find-file-noselect
    ;;             (plist-get candidate :filename))
    ;;          (goto-char (plist-get candidate :begin))

    ;;          ;; Check we are looking at the right place
    ;;          (unless (and (looking-at org-heading-regexp)
    ;;                       (string= (plist-get candidate :email) (org-entry-get (point) "EMAIL")))
    ;;            (error "It does not appear we are looking at the right place here:\n%s" (plist-get candidate :filename)))

    ;;          (setq link (format
    ;;                      "[[contact:%s][%s]]"
    ;;                      (org-entry-get (point) "EMAIL")
    ;;                      (nth 4 (org-heading-components))))))
    ;; (when (looking-back "]" 1)
    ;;   (insert ", "))
    ;; (insert link)
    (insert
     (format "[[contact:%s][%s]]"
             (plist-get candidate :email)
             (plist-get candidate :title)))))


(defun my-org-db--assign-contact (x)
  "Assign current heading to contact X.
Sets heading TODO state and prompts for deadline if there is not one."
  (interactive)
  (let ((emails
         (org-entry-get-multivalued-property (point) "ASSIGNEDTO")))
    (setq emails
          (append emails (list (plist-get (cdr x) :email))))
    (apply 'org-entry-put-multivalued-property
           (point)
           "ASSIGNEDTO" emails))

  (unless (string= "TODO" (org-get-todo-state)) (org-todo "TODO"))

  (unless (org-get-deadline-time (point)) (org-deadline nil)))


(defun my-org-db--open-contact (x)
  "Open contact X.
This is a little flexible. Sometimes :begin is out of date so instead we use search."
  (find-file (plist-get (cdr x) :filename))
  (goto-char
   (car
    (org-ql-query :select #'point
                  :from (current-buffer)
                  :where `(property "EMAIL" ,(plist-get (cdr x) :email)))))
  (outline-show-entry))


(defun my-org-db--insert-contact (x)
  "Insert \"name\" <email> for X at point.
If point is not looking back on a space insert a comma separator."
  (unless (and (looking-back " " 1) (not (bolp)))
    (insert ","))
  (insert
   (format "\"%s\" <%s>"
           (plist-get (cdr x) :title)
           (plist-get (cdr x) :email))))


(defun my-org-db--email-contact (x)
  "Open an email to the contact"
  (interactive)
  (compose-mail)
  (message-goto-to)
  (insert (plist-get (cdr x) :email))
  (message-goto-subject))


(defun my-org-db--insert-@-label (x)
  "Insert an @ label"
  (interactive)
  (insert
   (format "@%s"
           (s-join "-" (s-split " " (plist-get (cdr x) :title))))))


(defun my-org-db--multi-contact (candidates)
  "Act on CANDIDATES with choice of actions"
  (let ((action
         (completing-read "Action: "
                          '(("insert")
                            ("links")
                            ("email"))
                          nil "^")))
    (cond
     ((string= action "insert")
      (unless (or (bolp) (looking-back " " 1))
        (insert ","))
      (insert
       (mapconcat
        (lambda (x)
          (format "\"%s\" <%s>"
                  (plist-get (cdr x) :title)
                  (plist-get (cdr x) :email)))
        candidates
        ",")))
     ((string= action "email")
      (compose-mail)
      (message-goto-to)
      (insert
       (mapconcat
        (lambda (x) (plist-get (cdr x) :email))
        candidates
        ","))
      (message-goto-subject))
     ((string= action "links")
      (mapcar 'my-org-db--insert-contact-link candidates)))))


(defvar my-org-db-contacts-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-h") 'my-org-db--contacts-help)
    map))

(defun my-org-db--contacts-help ()
  (interactive)
  (org-link-open-from-string
   (format
    "[[%s::*Contacts]]"
    (expand-file-name "scimax.org" scimax-dir))))


(defun my-org-db-contacts ()
  "Ivy command to select an `my-org-db' contact."
  (interactive)
  ;; start with no marked candidates. I am not sure how this works with ivy resume.
  (setq my-org-db-contacts-marked-candidates '())
  (let ((candidates (my-org-db-contacts-candidates)))
    (ivy-read "Contact: " candidates
              :keymap my-org-db-contacts-keymap
              :caller 'my-org-db-contacts
              :multi-action 'my-org-db--multi-contact
              :action '(1
                        ("i" my-org-db--insert-contact "insert")
                        ("o" my-org-db--open-contact "open")
                        ("l" my-org-db--insert-contact-link "Insert link")
                        ("a" my-org-db--assign-contact "Assign to heading")
                        ("e" my-org-db--email-contact "Email contact")
                        ("2" my-org-db--insert-@-label "Insert @label")
                        ("?" my-org-db--contacts-help "Help")))))


(defun my-org-db-contact-transformer (s)
  "Make marked candidates look red."
  (if (s-starts-with? ">" s)
      (propertize s 'face 'font-lock-warning-face)
    s))

(ivy-set-display-transformer
 'my-org-db-contacts
 'my-org-db-contact-transformer)


;; * my-org-db-locations

(defun my-org-db-locations-candidates ()
  "Return a list of headings with an ADDRESS property."
  (let ((locations
         (emacsql my-org-db [:select [headlines:title headline-properties:value headlines:tags files:filename headlines:begin]
                                     :from headlines
                                     :inner :join headline-properties :on
                                     (=  headlines:rowid headline-properties:headline-id)
                                     :inner :join properties :on
                                     (= properties:rowid headline-properties:property-id)
                                     :inner :join files :on
                                     (= files:rowid headlines:filename-id)
                                     :where (= properties:property "ADDRESS")
                                     ])))
    (cl-loop for
             (title address tags fname begin)
             in locations
             collect
             (list
              (format "%60s | %70s | %s"
                      (s-trim title)
                      address
                      (or tags ""))
              :filename fname :begin begin))))



(defun my-org-db-locations ()
  "Open a location in `my-org-db'."
  (interactive)
  (let ((candidates (my-org-db-locations-candidates)))
    (ivy-read "Location: " candidates :action
              '(1
                ("o"
                 (lambda (x)
                   (find-file (plist-get (cdr x) :filename))
                   (goto-char (plist-get (cdr x) :begin)))
                 "open")
                ("l"
                 (lambda (x)
                   (let ((link)
                         (candidate (cdr x)))
                     (with-current-buffer (find-file-noselect
                                           (plist-get candidate :filename))
                       (goto-char (plist-get candidate :begin))
                       (setq link
                             (format
                              "[[location:%s][%s]]"
                              (org-id-get-create)
                              (nth 4 (org-heading-components)))))
                     (insert link)))
                 "insert link")))))


;; * my-org-db src-blocks
(defun my-org-db-src-blocks ()
  "Search src blocks."
  (interactive)
  (let* ((src-blocks
          (emacsql my-org-db [:select [src-blocks:language
                                       src-blocks:contents
                                       src-blocks:begin
                                       files:filename]
                                      :from src-blocks
                                      :inner  :join files
                                      :on (= files:rowid src-blocks:filename-id)
                                      ]))
         (candidates
          (cl-loop for
                   (language contents begin filename)
                   in src-blocks collect
                   (list
                    (format "%s: %s" language contents)
                    :filename filename :begin begin))))

    (ivy-read "query: " candidates
              :action (lambda
                        (candidate)
                        (find-file (plist-get (cdr candidate) :filename))
                        (goto-char (plist-get (cdr candidate) :begin))))))

;; * my-org-db headings
(defun my-org-db-heading-candidates ()
  "Return heading candidates completion."
  (let* ((headings
          (emacsql my-org-db [:select [headlines:level headlines:title headlines:tags
                                                       files:filename headlines:begin
                                                       files:last-updated]
                                      :from headlines
                                      :inner :join files
                                      :on (= files:rowid headlines:filename-id)
                                      :order :by files:last-updated :desc]))
         (candidates
          (cl-loop for
                   (level title tags filename begin last-updated)
                   in headings
                   collect
                   (cons
                    (format "%100s|%20s|%s|%s"
                            (s-pad-right 100 " "
                                         (concat
                                          (make-string level
                                                       (string-to-char "*"))
                                          " " title))
                            (s-pad-right 20 " " (or tags ""))
                            filename last-updated)
                    (list
                     :file filename
                     :last-updated last-updated
                     :begin begin
                     :title title)))))
    candidates))


(defun my-org-db-headings--open (x)
  "Open the heading X."
  (interactive)
  (find-file (plist-get (cdr x) :file))
  (goto-char (plist-get (cdr x) :begin))
  (org-show-context))


(defun my-org-db-headings--store-link (x)
  "Store a link to X."
  (interactive)
  (find-file (plist-get (cdr x) :file))
  (goto-char (plist-get (cdr x) :begin))
  (org-store-link))


(defun my-org-db-headings--insert-link (x)
  "Insert a link to X"
  (interactive)
  (insert
   (format "[[file:%s::*%s][%s]]"
           (plist-get (cdr x) :file)
           (plist-get (cdr x) :title)
           (plist-get (cdr x) :title))))


;;;###autoload
(defun my-org-db-headings ()
  "Use ivy to open a heading with completion."
  (interactive)
  (let* ((candidates (my-org-db-heading-candidates)))
    (ivy-read "heading: " candidates
              :action '(1
                        ("o" my-org-db-headings--open "Open to heading.")
                        ("l" my-org-db-headings--insert-link "Insert link to heading")
                        ("s" my-org-db-headings--store-link "Store link to heading.")))))


;; * my-org-db files

(defun my-org-db-files ()
  "Open a file in ‘my-org-db’ with completion."
  (interactive)
  (find-file
   (ivy-read "File: "
             (mapcar 'car
                     (emacsql my-org-db [:select [filename]
                                                 :from files
                                                 :order :by filename])))))


(defun my-org-db-recentf ()
  "Open a recent file in ‘my-org-db’ with completion.
Recent is sorted by last-updated in the database."
  (interactive)
  (let ((candidates
         (mapcar
          (lambda (x)
            (cons
             (format "%s %s" (cdr x) (car x))
             (car x)))
          (emacsql my-org-db [:select [filename last-updated]
                                      :from files
                                      :order :by last-updated :desc]))))
    (find-file
     (cdr (assoc (ivy-read "File: " candidates) candidates)))))


;; * my-org-db-links

(defun my-org-db-links--open (x)
  "Open the link X."
  (interactive)
  (find-file (plist-get (cdr x) :filename))
  (goto-char (plist-get (cdr x) :begin))
  (org-show-entry))


(defun my-org-db-links ()
  "Open a link."
  (interactive)
  (let ((candidates
         (cl-loop
          for
          (rl fn bg)
          in
          (emacsql my-org-db [:select [raw-link filename begin ]
                                      :from links
                                      :left :join files :on
                                      (= links:filename-id files:rowid)
                                      :order :by filename])
          collect
          ;; (candidate :filename :begin)
          (list (format "%s | %s" rl fn) :filename fn :begin bg))))
    (ivy-read "link: " candidates :action
              '(1 ("o" my-org-db-links--open "Open to link")))))


;; * my-org-db-targets
(defun my-org-db-target ()
  "Search for targets in org-files."
  (interactive)
  (let* ((results
          (emacsql my-org-db [:select [target file-targets:begin files:filename]
                                      :from targets
                                      :left :join file-targets :on
                                      (= targets:rowid file-targets:target-id)
                                      :inner :join files
                                      :on (= files:rowid file-targets:filename-id)
                                      ]))
         (candidates
          (cl-loop for
                   (target begin fname)
                   in results collect
                   (list
                    (format "%30s%s" (s-pad-right 30 " " target) fname)
                    fname
                    begin))))
    (ivy-read "Target: " candidates :action
              (lambda (x)
                (find-file (cl-second x))
                (goto-char (cl-third x))))))

;; * my-org-db-hashtags

;; The idea here is to use #hashtags in org files so we can search them later.
;; These are hard, there are many reasons for #something, including comments in
;; python blocks, #'in-elisp, etc. I sort of like the idea though, because it
;; provides a way to jump to paragraphs. Maybe it would be better to use radio
;; targets for this though.

(defun my-org-db-delete-hashtag (hashtag)
  "Delete HASHTAG from the db."
  (let* ((hashtag-id
          (caar
           (emacsql my-org-db [:select rowid :from hashtags
                                       :where (= hashtag $s1)
                                       ]
                    hashtag))))
    (if (null hashtag-id)
        (my-org-db-log "Weird. no hashtag id found for %s" hashtag)
      (my-org-db-log "removing %s entries: %s" hashtag hashtag-id)

      (emacsql my-org-db [:delete :from file-hashtags
                                  :where (= file-hashtags:hashtag-id $s1)
                                  ]
               hashtag-id)
      (emacsql my-org-db [:delete :from hashtags
                                  :where (= hashtags:rowid $s1)
                                  ]
               hashtag-id))))


(defun my-org-db-delete-hashtags ()
  "Delete all the hashtags in the db."
  (interactive)
  (emacsql my-org-db [:delete :from hashtags])
  (emacsql my-org-db [:delete :from file-hashtags]))


(defun looking-at-hashtag ()
  "Return hashtag if looking at one, else nil.
This is kind of tricky. We look backwards to the beginning the
line and only return a match if it is around the current point."
  (interactive)
  ;; I thought this would work too, but it also isn't reliable
  ;; (save-excursion
  ;;   (let ((p (point)))
  ;;     (when (re-search-backward hashtag-rx (line-beginning-position) t 1)
  ;;    ;; We have to do this little trick to get the whole match. the previous
  ;;    ;; line stops at the current point.
  ;;    (looking-at hashtag-rx)
  ;;    ;; then we make sure the match brackets our point.
  ;;    (when (and (>= p (match-beginning 0))
  ;;               (<= p (match-end 0)))
  ;;      (message "%s" (match-string 0))))))

  ;; I thought this should work, but it doesn't appear to
  ;; (looking-back hashtag-rx (line-beginning-position) t)

  ;; This is clunky to me, but it does work.

  ;; There are some other subtle points.
  ;; 1. should we allow hashtags in src-blocks? This leads to many false hashtags
  ;;    in Python for example for comments..
  ;; [2020-07-28 Tue] I am eliminating src-blocks for hash-tags.
  (let ((p (point))
        (lbp (line-beginning-position)))
    (save-excursion
      (while (and
              (not (org-in-src-block-p))
              (not (looking-at hashtag-rx))
              (>= (point) lbp))
        (backward-char))
      (when (and
             (>= p (match-beginning 0))
             (<= p (match-end 0)))
        (match-string-no-properties 1)))))


(defun my-org-db-hashtags--open (x)
  "Open the hashtag entry X."
  (interactive)
  (find-file (plist-get (cdr x) :filename))
  (goto-char (plist-get (cdr x) :begin))
  (outline-show-entry))


(defun my-org-db-hashtags--insert (x)
  "Insert the hashtag X."
  (interactive)
  (insert (concat "#" (plist-get (cdr x) :hashtag))))


(defun my-org-db-hashtags--other-files (x)
  "Open list of other files containing hashtag X."
  (interactive)
  (let* ((hashtag (org-no-properties (plist-get (cdr x) :hashtag)))
         (results
          (emacsql my-org-db [:select [hashtag file-hashtags:begin files:filename]
                                      :from hashtags
                                      :left :join file-hashtags :on
                                      (= hashtags:rowid file-hashtags:hashtag-id)
                                      :inner :join files
                                      :on (= files:rowid file-hashtags:filename-id)
                                      :where (= hashtag $s1)
                                      ]
                   hashtag))
         (candidates
          (cl-loop for (hashtag begin fname) in results collect fname)))
    (find-file (ivy-read "File: " candidates))))


(defun my-org-db-hashtags--delete (x)
  "Delete entries for hashtag."
  (let* ((hashtag (plist-get (cdr x) :hashtag)))
    (my-org-db-delete-hashtag hashtag)))


(defun my-org-db-hashtags ()
  "Open a #hashtag.
If the cursor is on a hashtag, it uses that as the initial input.
I am not sure how to do multiple hashtag matches right now, that needs a fancier query."
  (interactive)
  (let* ((tip (looking-at-hashtag))
         (hashtag-data
          (emacsql my-org-db [:select [hashtag file-hashtags:begin files:filename]
                                      :from hashtags
                                      :left :join file-hashtags :on
                                      (= hashtags:rowid file-hashtags:hashtag-id)
                                      :inner :join files
                                      :on (= files:rowid file-hashtags:filename-id)
                                      ]))
         (candidates
          (cl-loop for
                   (hashtag begin fname)
                   in hashtag-data
                   collect
                   (list
                    (format "#%40s  %s"
                            (s-pad-right 40 " " hashtag)
                            fname)
                    :hashtag hashtag :begin begin :filename fname))))

    (ivy-read "#hashtag: " candidates :initial-input tip
              :action '(1
                        ("o" my-org-db-hashtags--open "Open file at hashtag")
                        ("i" my-org-db-hashtags--insert "Insert hashtag at point.")
                        ("O" my-org-db-hashtags--other-files "Other files with hashtag")
                        ("D" my-org-db-hashtags--delete "Delete this hashtag")))))


;; * my-org-db-@
(defun my-org-db-@ ()
  "Jump to an @label."
  (interactive)
  (let* ((tip (looking-at-hashtag))
         (@-data
          (emacsql my-org-db [:select [atlabel file-atlabels:begin files:filename]
                                      :from atlabels
                                      :left :join file-atlabels :on
                                      (= atlabels:rowid file-atlabels:atlabel-id)
                                      :inner :join files
                                      :on (= files:rowid file-atlabels:filename-id)
                                      ]))
         (candidates
          (cl-loop for
                   (atlabel begin fname)
                   in @-data
                   collect
                   (list
                    (format "%40s  %s" atlabel fname)
                    :@-label atlabel :begin begin :filename fname))))

    (ivy-read "@label: " candidates
              :action '(1
                        ("o"
                         (lambda (x)
                           "Open the @label entry X."
                           (interactive)
                           (find-file (plist-get (cdr x) :filename))
                           (goto-char (plist-get (cdr x) :begin))
                           (unless (org-before-first-heading-p)
                             (outline-show-entry)))
                         "Open file at @label")))))

;; * org-id integration
;; org-id-goto is very slow for me. This function can replace it.
(defun my-org-db-goto-id (id)
  "Open an org file at ID."
  (let* ((result
          (emacsql my-org-db
                   [:select [files:filename headlines:begin]
                            :from headlines
                            :inner :join headline-properties
                            :on (=  headlines:rowid headline-properties:headline-id)
                            :inner :join properties
                            :on (= properties:rowid headline-properties:property-id)
                            :inner :join files :on
                            (= files:rowid headlines:filename-id)
                            :where (and
                                    (= properties:property "ID")
                                    (= headline-properties:value $s1))
                            ]
                   id))
         match fname p)
    (cond
     ((and result (= 1 (length result)))
      (setq match
            (cl-first result)
            fname
            (cl-first match)
            p
            (cl-second match))
      (find-file fname)
      (goto-char p))

     ((> (length result) 1)
      ;; TODO maybe use ivy to select?
      (my-org-db-log
       "(%s) matches: %s result"
       (length result)
       result))

     (t (my-org-db-log "No match found")))))


(defun my-org-db-toggle-org-id ()
  "Toggle using `my-org-db' to follow org-id links."
  (interactive)
  (if (not (get 'my-org-db-goto-id 'enabled))
      (progn
        (advice-add 'org-id-goto :override #'my-org-db-goto-id)
        (put 'my-org-db-goto-id 'enabled t)
        (my-org-db-log "my-org-db-goto-id advice enabled."))
    (advice-remove 'org-id-goto #'my-org-db-goto-id)
    (put 'my-org-db-goto-id 'enabled nil)
    (my-org-db-log "my-org-db-goto-id advice disabled.")))


;; * tag search
;; We don't really need a special tag search. You can do it with
;; `my-org-db-headings' by adding : into your search. I am leaving this note here
;; in case I ever think of trying to write this again!


;; * property search

(defun my-org-db-properties (property pattern)
  "Search my-org-db for entries where PROPERTY matches PATTERN.
PATTERN follows sql patterns, so % is a wildcard.
It is not currently possible to do multiple property searches."
  (interactive
   (list
    (completing-read "Property: "
                     (-flatten
                      (emacsql my-org-db
                               [:select properties:property
                                        :from properties])))
    (read-string "Pattern: ")))
  (let* ((results
          (emacsql my-org-db
                   [:select [headlines:title
                             properties:property
                             headline-properties:value
                             files:filename files:last-updated headlines:begin]
                            :from headlines
                            :inner :join headline-properties
                            :on (=  headlines:rowid headline-properties:headline-id)
                            :inner :join properties
                            :on (= properties:rowid headline-properties:property-id)
                            :inner :join files :on
                            (= files:rowid headlines:filename-id)
                            :where (and
                                    (= properties:property $s1)
                                    (like headline-properties:value $s2))
                            ]
                   property pattern))
         (candidates
          (cl-loop for
                   (title property value fname last-updated begin)
                   in results
                   collect
                   (list
                    (format "%s | %s=%s | %s" title property value fname)
                    :filename fname :begin begin))))

    (ivy-read "Choose: " candidates
              :action (lambda
                        (x)
                        (let ((candidate (cdr x)))
                          (find-file (plist-get candidate :filename))
                          (goto-char (plist-get candidate :begin)))))))


;; * search editmarks
(defun my-org-db-editmarks ()
  "Search the editmarks table in my-org-db."
  (interactive)

  (let* ((results
          (emacsql my-org-db
                   [:select  [files:filename file-editmarks:begin file-editmarks:content file-editmarks:type]
                             :from file-editmarks
                             :inner :join files :on
                             (= files:rowid file-editmarks:filename-id)
                             ]))
         (candidates
          (cl-loop for
                   (fname begin content type)
                   in results
                   collect
                   (list
                    (format "%s | %s | %s" type fname (s-trim content))
                    :filename fname
                    :begin begin))))

    (ivy-read "Choose: " candidates :action
              (lambda (x)
                (find-file (plist-get (cdr x) :filename))
                (goto-char (plist-get (cdr x) :begin))))))


;; * search for email addresses

(defun my-org-db-email-addresses ()
  (interactive)
  (let* ((results
          (emacsql my-org-db
                   [:select  [files:filename file-email-addresses:begin email-addresses:email-address]
                             :from file-email-addresses
                             :inner :join files :on
                             (= files:rowid file-email-addresses:filename-id)
                             :inner :join email-addresses :on
                             (= email-addresses:rowid
                                file-email-addresses:email-address-id)
                             ]))
         (candidates
          (cl-loop for
                   (fname begin email-address)
                   in results
                   collect
                   (list
                    (format "%s | %s " email-address fname)
                    :filename fname
                    :begin begin))))

    (ivy-read "Choose: " candidates :action
              (lambda (x)
                (find-file (plist-get (cdr x) :filename))
                (goto-char (plist-get (cdr x) :begin))))))


;; * backlinks
(defun my-org-db-backlink-candidates ()
  (let* ((buffer-name (buffer-file-name))
         (fname (file-name-nondirectory buffer-name))
         ;; I am assuming we only want to match on file links
         (potential-matches
          (emacsql my-org-db [:select [filename path begin]
                                      :from links
                                      :left :join files :on
                                      (= links:filename-id files:rowid)
                                      :where (and
                                              (= links:type "file")
                                              (like path $s1))
                                      ]
                   (concat "%" fname "%")))
         (matches
          (cl-loop for
                   (src-filename path begin)
                   in potential-matches collect
                   (cond
                    ;; path is absolute and points to file
                    ((and
                      (file-name-absolute-p path)
                      (string= path buffer-name))
                     (list src-filename begin))
                    ;; path is relative but expands to buffer-name relative to src-filename
                    ((string=
                      (expand-file-name path
                                        (file-name-directory src-filename))
                      buffer-name)
                     (list src-filename begin))
                    (t nil)))))
    (cl-loop for match in matches
             if
             (not (null match))
             collect
             (list
              (format "%s | %s"
                      (first match)
                      (with-temp-buffer
                        (insert-file-contents (first match))
                        (goto-char (second match))
                        (buffer-substring
                         (line-beginning-position)
                         (line-end-position))))
              (first match)
              (second match)))))


(defun my-org-db-backlinks ()
  "Find backlinks to the current file.
This finds other files with links in them to this file."
  (interactive)
  (ivy-read "Backlink: "
            (my-org-db-backlink-candidates)
            :action (lambda
                      (match)
                      (find-file (second match))
                      (goto-char (third match)))))



;; * my-org-db hydra
;; this is just for convenience, so I don't have to remember everything.

(defhydra my-org-db
  (:color blue :hint nil)
  "my-org-db search
"
  ("h" my-org-db-headings "headlines" :column "org element")
  ("p" my-org-db-properties "properties" :column "org element")
  ("s" my-org-db-src-blocks "src-blocks" :column "org element")
  ("k" my-org-db-links "links" :column "org element")
  ("b" my-org-db-backlinks "backlinks" :column "org element")

  ("c" my-org-db-contacts "contacts" :column "derived")
  ("l" my-org-db-locations "locations" :column "derived")

  ("e" my-org-db-email-addresses "email" :column "pattern")
  ("m" my-org-db-editmarks "editmarks"  :column "pattern")
  ("3" my-org-db-hashtags "hashtags"  :column "pattern")
  ("2" my-org-db-@ "@-labels"  :column "pattern")

  ("f" my-org-db-files "files")
  ("r" my-org-db-recentf "recent files"))



;; * my-org-db-macro

(defmacro my-org-db-table (select where)
  "Experiment to see if we can make the syntax nicer."
  `[:select ,select
            :from headlines
            :inner :join headline-properties
            :on (=  headlines:rowid headline-properties:headline-id)
            :inner :join properties
            :on (= properties:rowid headline-properties:property-id)
            :inner :join files :on
            (= files:rowid headlines:filename-id)
            :where ,where])

;; example usage
;; (emacsql my-org-db (my-org-db-table [headlines:title
;;                             headline-properties:value
;;                             headlines:tags files:filename files:last-updated headlines:begin]
;;                            (and (= properties:property $s1)
;;                                 (like headline-properties:value $s2)))
;;       "EMAIL" "%kitchin%")

;; * End
(provide 'my-org-db)

;;; my-org-db.el ends here
