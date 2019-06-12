;;; -*- mode: Lisp; Syntax: common-lisp; Package: sup; Base: 10 eval: (hs-hide-all) -*-

;; 🆑 or gtfo

(defpackage :sup
  (:use #:cl
        #:json-mop
        #:easy-routes
        #:parenscript
        #:spinneret
        #:cl-hash-util
        #:postmodern)
  (:local-nicknames (:alex :alexandria)))

(in-package #:sup)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (let ((*config* (yason:parse (alex:read-file-into-string
                                (asdf:system-relative-pathname :sup "config.json")))))
    (setf cl-mango:*host* "10.0.0.18")
    (setf cl-mango:*port* 5984)
    (setf cl-mango:*scheme* :http)
    (setf cl-mango:*username* "admin")
    (setf cl-mango:*password* (gethash "db-pass" *config*))

    (defparameter *client-id* (gethash "client-id" *config*))
    (defparameter *client-secret* (gethash "client-secret" *config*))
    (setf yason:*parse-json-booleans-as-symbols* t)
    (setf hunchentoot:*catch-errors-p* t)))

(defmacro with-pg (&body body)
  `(with-connection (list "reddit" "sup" "fl33j0b" "localhost" :pooled-p t)
     ,@body))

(defparameter *reddit-user* nil)

(defparameter *listener* nil)

(defparameter *mailbox* (mailbox:make-mailbox))


;; (defclass link ()
;;   ((id :initarg :id
;;        :col-type :text
;;        :accessor link-id)
;;    (hidden :initarg :hidden
;;            :col-type :bool
;;            :accessor link-hidden)
;;    (written :initarg :written
;;             :col-type :bigint
;;             :accessor link-written)
;;    (created-utc :initarg :created-utc
;;                 :col-type :bigint
;;                 :accessor link-created-utc)
;;    (url :initarg :url
;;         :col-type :text
;;         :accessor link-url)
;;    (permalink :initarg :permalink
;;               :col-type :text
;;               :accessor link-permalink)
;;    (author :initarg :author
;;            :col-type :text
;;            :accessor :link-author)
;;    (subreddit-id :initarg :subreddit-id
;;                  :col-type :text
;;                  :accessor link-subreddit-id)
;;    (media-only :initarg :media-only
;;                :col-type :bool
;;                :accessor link-media-only)
;;    (score :initarg :score
;;           :col-type :bigint
;;           :accessor link-score)
;;    (title :initarg :title
;;           :col-type :text
;;           :accessor link-title)
;;    (author-fullname :initarg :author-fullname
;;                     :col-type :text
;;                     :accessor link-author-fullname)
;;    (selftext :initarg :selftext
;;              :col-type :text
;;              :accessor link-selftext)
;;    (subreddit :initarg :subreddit
;;               :col-type :text
;;               :accessor link-subreddit))
;;   (:metaclass dao-class)
;;   (:keys id))

;; (deftable link
;;   (!dao-def)
;;   (!unique-index :id))


;; (defclass subreddit ()
;;   ((id :initarg :id
;;        :col-type :text
;;        :accessor subreddit-id)
;;    (display-name :initarg :display-name
;;                  :col-type :text
;;                  :accessor subreddit-display-name))
;;   (:metaclass dao-class)
;;   (:keys id))

;; (deftable subreddit
;;   (!dao-def)
;;   (!unique-index :id))

;; (defun copy-mango-data ()
;;   (dolist (doc-id (our-couch-query (list (cons "type" "subreddit")
;;                                          (cons "deleted" (alex:alist-hash-table
;;                                                           (list (cons "$exists" 'yason:false)))))))
;;     (let ((doc (yason:parse (mango:doc-get "reddit" (gethash "_id" doc-id)))))
;;       (with-pg (insert-dao (make-instance 'subreddit
;;                                           :id (gethash "id" doc)
;;                                           :display-name (gethash "display_name" doc))))))
;;   (dolist (doc-id (our-couch-query (list (cons "type" "link")
;;                                          (cons "deleted" (alex:alist-hash-table
;;                                                           (list (cons "$exists" 'yason:false)))))
;;                                    :limit 100000
;;                                    :fields (list "_id")))
;;     (let ((doc (yason:parse (mango:doc-get "reddit" (gethash "_id" doc-id)))))
;;       (with-pg (insert-dao (make-instance 'link
;;                                           :id (gethash "id" doc)
;;                                           :hidden (gethash "hidden" doc)
;;                                           :written (gethash "written" doc)
;;                                           :created-utc (rationalize (gethash "created_utc" doc))
;;                                           :url (gethash "url" doc)
;;                                           :permalink (gethash "permalink" doc)
;;                                           :author (gethash "author" doc)
;;                                           :subreddit-id (gethash "subreddit_id" doc)
;;                                           :media-only (gethash "media_only" doc)
;;                                           :score (gethash "score" doc)
;;                                           :title (gethash "title" doc)
;;                                           :author-fullname (gethash "author_fullname" doc)
;;                                           :selftext (gethash "selftext" doc)
;;                                           :subreddit (gethash "subreddit" doc)))))))



(defmacro view-rows (document view args)
  `(gethash "rows"
            (yason:parse
             (mango:query-view "reddit" ,document ,view ,args))))

(defmacro our-couch-query (&rest args)
  `(gethash "docs" (yason:parse (mango:couch-query "reddit" ,@args))))

(defun application-start ()
  (swank:create-server :port 5000 :dont-close t)
  (start-server)
  (loop (sleep 10)))

(defclass access-token ()
  ((access-token :accessor access-token-access-token
                 :json-type :string
                 :json-key "access_token")
   (token-type :accessor access-token-token-type
               :json-type :string
               :json-key "token_type")
   (expires_in :accessor access-token-expires
               :json-type :number
               :json-key "expires_in")
   (scope :accessor access-token-scope
          :json-type :string
          :json-key "scope"))
  (:metaclass json-serializable-class))

(defun make-request (url)
  (drakma:http-request (format nil "~a~a" "https://www.reddit.com" url)))

(defun authenticate ()
  (let ((config (yason:parse (alex:read-file-into-string
                              (asdf:system-relative-pathname :sup "config.json")))))
    (multiple-value-bind (result code)
        (drakma:http-request "https://www.reddit.com/api/v1/access_token"
                             :basic-authorization (list *client-id* *client-secret*)
                             :additional-headers (list (cons "User-Agent" "Supyawl/0.1 by clintm"))
                             :accept "application/json"
                             :content-type "application/json"
                             :parameters (list (cons "grant_type" "password")
                                               (cons "username" (gethash "login" config))
                                               (cons "password" (gethash "password" config)))
                             :method :post
                             :preserve-uri t)
      (alex:switch (code)
        ((or 503 504) (authenticate))
        (otherwise (setf *reddit-user*
                         (json-mop:json-to-clos result 'access-token)))))))

(defun reddit-request (path)
  (drakma:http-request (format nil "https://oauth.reddit.com~a" path)
                       :additional-headers (list
                                            (cons "User-Agent" "supyawl/0.1 by clintm")
                                            (cons "Authorization" (format nil
                                                                          "bearer ~a"
                                                                          (access-token-access-token *reddit-user*))))))

(defun make-id (doc-hash)
  (declare (type hash-table doc-hash))
  (let ((the-reddit-id (gethash "id" doc-hash))
        (the-type (gethash "type" doc-hash)))
    (assert (and (stringp the-reddit-id)
                 (stringp the-type)))
    (format nil "~a:~a" the-reddit-id the-type)))

(defun start-server (&key (port 8086))
  (unless *listener*
    (setf *listener* (make-instance 'easy-routes:easy-routes-acceptor
                                    :address "127.0.0.1"
                                    :document-root (asdf:system-relative-pathname
                                                    :sup "static/am")
                                    :port 8086)))
  (hunchentoot:start *listener*))

(defgeneric to-json (a)
  (:method ((object hash-table))
    (with-output-to-string (sink)
      (yason:encode object sink)))

  (:method ((object string))
    (with-output-to-string (sink)
      (yason:encode object sink)))
  
  (:method ((object list))
    (with-output-to-string (sink)
      (yason:encode object sink))))

(defun resource-path (path)
  (truename
   (asdf:system-relative-pathname
    (intern
     (package-name (find-package :nugget))) path)))

(ps:defpsmacro with-document-ready (&body body)
  `(progn
     ((@ ($ document) ready) ,@body)))

(ps:defpsmacro jq (selector &body body)
  `(-> (sel ,selector)
       ,@body))

(ps:defpsmacro sel (name)
  `($ ,name))

(ps:defpsmacro $. (name)
  `(@ (sel ,name)))

(ps:defpsmacro -> (&body body)
  `(ps:chain ,@body))

(ps:defpsmacro map (func list)
  `(do ((i 0 (incf i)))
       ((>= i (@ ,list length)))
     (funcall ,func (aref ,list i))))

(defmacro with-page ((&key
                        (title nil)
                        (body-class nil)
                        (enable-page-header nil)) &body body)
  `(with-html-string
        (:doctype)
        (:html :lang "en"
          (:head
            (:meta :charset "utf-8")
            ,(if title
                 `(:title ,title)
                 `(:title "Nugget"))
            (:meta
              :name "viewport"
              :content "width=device-width, initial-scale=1.0, maximum-scale=1.0, user-scalable=no")
            (:link :rel "stylesheet"
              :type "text/css" :href "/am/lib/stroke-7/style.css")
            (:link :rel "stylesheet"
              :type "text/css"
              :href "/am/lib/jquery.nanoscroller/css/nanoscroller.css")
            (:link :rel "stylesheet" :type "text/css" :href "/am/css/style.css")
            (:script :src "/am/lib/jquery/jquery.min.js")
            (:style ".am-wrapper { padding-top: 0px !important; }"))
          (:body :style "background-color:rgb(51,51,51);" ,@(when body-class `(:class ,body-class))
            (:div :class "am-wrapper am-nosidebar-left"
              (:div :class "am-content"
                ,(when enable-page-header
                   '(:div :class "page-head"
                     (:h2 "Calendar")
                     (:ol :class "breadcrumb"
                       (:li (:a :href "#" "Home"))
                       (:li (:a :href "#" "Pages")))))
                (:div.main-content
                 ,@body)))
            (:script :src "/am/lib/jquery.nanoscroller/javascripts/jquery.nanoscroller.min.js")
            (:script :src "/am/js/main.min.js")
            (:script :src "/am/lib/bootstrap/dist/js/bootstrap.min.js")
            (:script :src "/am/lib/jquery-ui/jquery-ui.min.js")
            (:script :src "/am/lib/jquery.sparkline/jquery.sparkline.min.js")))))

(defun make-hash (string)
  (ironclad:byte-array-to-hex-string
   (ironclad:digest-sequence
    :sha256
    (ironclad:ascii-string-to-byte-array string))))

(defun get-imgur-id (link)
  (when (ppcre:scan "imgur.com" link)
    (cond
      
      ((ppcre:scan "https://imgur.com/a/" link)
       (ppcre:regex-replace "https://imgur.com/a/" link ""))

      ((ppcre:scan "https://imgur.com/gallery/" link)
       (ppcre:regex-replace "https://imgur.com/gallery/" link ""))

      ((ppcre:scan "https://imgur.com/" link)
       (ppcre:regex-replace "https://imgur.com/" link ""))

      ((ppcre:scan "https://i.imgur.com/" link)
       (ppcre:regex-replace ".gifv|.jpg|.gif|.png"
                            (ppcre:regex-replace "https://i.imgur.com[/a|/]" link "")
                            ""))
      ((ppcre:scan "i.imgur.com" link)
       (ppcre:regex-replace "https://imgur.com/" link ""))

      (t nil))))

(defun get-images-from-doc-hash (doc-hash)
  (alex:when-let* ((preview (gethash "preview" doc-hash))
              (images (gethash "images" preview)))
    images))

(defun comments-handle-link (permalink)
  (declare (type string permalink))
  ;; FIX THIS SHITSHOW
  (ignore-errors
   (multiple-value-bind (data status)
       (drakma:http-request (format nil
                                    "https://www.reddit.com~acomments.json?limit=100"
                                    permalink)
                            :external-format-in :utf8
                            :external-format-out :utf8)
     (unless (equal status 200)
       (log:info "caught error. passing on this one." status)
       (if (equal status 503)
         (let ((decoded (ignore-errors (flexi-streams:octets-to-string data))))
           (log:info decoded))
         (log:info "Bailing out."))
       (return-from comments-handle-link))
     (dolist (listing (yason:parse data))
       (dolist (comment (gethash "children" (gethash "data" listing)))
         (let* ((new-comment (gethash "data" comment))
                (comment-in-db (ignore-errors
                                (yason:parse
                                 (mango:doc-get "reddit"
                                                (gethash "id"
                                                         (car (view-rows "queries" "comment-id"
                                                                         (list (cons "key" (to-json (gethash "id" new-comment))))))))))))
           (setf (gethash "type" new-comment) "comment")
           (if comment-in-db
             (progn
               (setf (gethash "_id" new-comment) (gethash "_id" comment-in-db))
               (setf (gethash "_rev" new-comment) (gethash "_rev" comment-in-db)))
             (setf (gethash "_id" new-comment) (make-id new-comment)))
           (handler-case
               (mango:doc-put "reddit" (to-json new-comment))
             (cl-mango:unexpected-http-response (condition)
               (let ((status (cl-mango::status-body condition)))
                 (log:info "~a ~a ~a ~a"
                           (gethash "_rev" new-comment)
                           status
                           comment-in-db
                           (gethash "id" comment-in-db)))))))))))

(defun hash-extract (name-string list-of-hashes)
  (mapcar (lambda (x)
            (gethash name-string x))
          list-of-hashes))

(defun link-is-image-p (link)
  (cond ((ppcre:scan ".jpg$|.png$|.gif$" link) link)
        ;;((ppcre:scan "imgur.com" link) (format nil "~a.gif" link))
        (t nil)))

(defun get-latest-post-id-for-subreddit (subreddit)
  (handler-case
      (alex:when-let ((dochash (car
                                (our-couch-query (list (cons "subreddit" subreddit)
                                                       (cons "type" "link"))
                                                 :limit 1
                                                 :sort (list (alex:alist-hash-table
                                                              (list (cons "created_utc" "desc"))))
                                                 :fields (list "name" "created_utc")))))
        (values (gethash "name" dochash)
                (gethash "created_utc" dochash)))
    (cl-mango:unexpected-http-response (condition)
      (log:info (cl-mango::status-body condition))
      nil)))

(defun has-subreddit-p (subreddit-name)
  (< 0 (length
        (our-couch-query (list (cons "display_name" subreddit-name)
                               (cons "type" "subreddit"))))))

(defun get-db-subreddits ()
  (our-couch-query (list (cons "type" "subreddit"))
                   :limit 100000
                   :fields (list "_id" "_rev" "display_name")))

(defun get-reddit-subreddits ()
  (authenticate)
  (alex:when-let ((post-data (gethash "data" (yason:parse
                                              (reddit-request "/subreddits/mine")))))
    (hash-extract "data"
                  (gethash "children" post-data))))

(defun sync-subreddits ()
  (alex:if-let ((reddit-subreddits (get-reddit-subreddits)))
    (let* ((reddit-names (hash-extract "display_name" reddit-subreddits))
           (db-subreddits (get-db-subreddits))
           (db-names (hash-extract "display_name" db-subreddits)))
      

      ;; Confirm that all subscribed subreddits are in the database.
      (map nil (lambda (reddit-name)
                 (unless (member reddit-name db-names :test #'string=)
                   (let ((reddit-hash (car (remove-if-not #'(lambda (x)
                                                              (string= (gethash "display_name" x) reddit-name))
                                                          reddit-subreddits))))
                     (setf (gethash "type" reddit-hash) "subreddit")
                     (mango:doc-put "reddit" (to-json reddit-hash))
                     (log:info "Missing ~a" (gethash "display_name" reddit-hash)))))
           reddit-names)
      ;; Remove db subreddits that aren't currently subscribed.
      (map nil (lambda (db-name)
                 (unless (member db-name reddit-names :test #'string=)
                   (let ((db-record (car (remove-if-not #'(lambda (x)
                                                            (string= (gethash "display_name" x) db-name))
                                                        db-subreddits))))
                     (log:info "Removing ~a" db-name)
                     (mango:doc-delete "reddit" (gethash "_id" db-record) (gethash "_rev" db-record)))))
           db-names))
    (log:info "Failed")))

(defun get-subreddit-links (subreddit)
  (multiple-value-bind (data code)
      (handler-case
          (let ((url (format nil "https://www.reddit.com~anew.json?limit=100"
                             (gethash "url" subreddit))))
            (drakma:http-request url))
        (usocket:timeout-error (condition)
          (declare (ignore condition))
          (log:info "Timeout error.  Restarting.")
          (get-subreddit-links subreddit))
        (error (condition)
          (log:info "Some sort of error:" condition)
          (log:info "Restarting.")
          (get-subreddit-links subreddit)))
    (alex:switch (code)
      (200 (typecase data
             (string (alex:if-let ((json-data (yason:parse data)))
                       (hu:hash-get json-data '("data" "children"))
                       (log:info "Links fetch was null? ~a ~a"
                                 code data)))
             (list (alex:flatten data))))
      (t (log:info "Connecting to reddit failed with: ~a for ~a"
                   code
                   (gethash "display_name" subreddit))))))

(defun is-repostp (link-hash)
  (when (gethash "is_original_content" link-hash)
    (log:info "Hey, OC UP IN HERE"))
  (< 0 (length
        (our-couch-query
         (list (cons "url" (gethash "url" link-hash))
               (cons "subreddit" (gethash "subreddit" link-hash)))))))

(defun existing-link-info (link-id)
  (let ((link-info (car (our-couch-query
                         (list (cons "_id" (format nil "~a:~a" link-id "link"))
                               (cons "type" "link"))
                         :fields '("_id" "_rev" "suphidden" "created_utc" "ups" "downs")))))
    (when (typep link-info 'hash-table)
      (values (gethash "_id" link-info)
              (gethash "_rev" link-info)
              (gethash "suphidden" link-info)
              (gethash "ups" link-info)
              (gethash "downs" link-info)))))

(defun handle-possible-new-link (new-link)
  (declare (optimize (debug 3) (speed 0) (safety 3)))
  (labels ((make-new-like-cache (new-link)
             (mango:doc-put "reddit" (to-json (alex:alist-hash-table
                                               (list (cons "type" "like-cache")
                                                     (cons "_id" (format nil "~a:like-cache"
                                                                         (gethash "id" new-link)))
                                                     (cons "id" (gethash "id" new-link))
                                                     (cons "ups" (list (gethash "ups" new-link))))))))
           (update-like-cache (new-link)
             (alex:if-let ((t-the-cache (car (our-couch-query
                                              (list (cons "id" (format nil "~a:like-cache"
                                                                       (gethash "id" new-link)))
                                                    (cons "deleted" (alex:alist-hash-table
                                                                     (list (cons "$exists" 'yason:false)))))))))
               (progn
                 (log:info "~a" t-the-cache)
                 (let ((the-cache (yason:parse t-the-cache)))
                   (let ((ups (gethash "ups" the-cache)))
                     (log:info "update existing")
                     (setf (gethash "ups" the-cache) (append ups (list (gethash "ups" new-link))))
                     (mango:doc-put "reddit" (to-json the-cache))
                     (mailbox:post-mail (to-json
                                         (alex:alist-hash-table
                                          (list (cons "id" (gethash "id" new-link))
                                                (cons "action" "update-votes")
                                                (cons "ups" (gethash "ups" new-link)))))
                                        *mailbox*))))
               ;;(make-new-like-cache new-link)
               )))
    (multiple-value-bind (id revision hidden)
        (existing-link-info (gethash "id" new-link))
      (unless hidden
        ;;(update-like-cache new-link)
        (if revision
          (progn
            
            ;; New copy of an old link, so just update the votes.
            (setf (gethash "_id" new-link) id)
            (setf (gethash "_rev" new-link) revision)
            (setf (gethash "type" new-link) "link")
            (setf (gethash "written" new-link) (get-universal-time))
            (handler-case (mango:doc-put "reddit" (to-json new-link))
              (cl-mango:unexpected-http-response (condition)
                (log:info "Error saving link: ~a ~a" condition new-link))))
          (unless (is-repostp new-link)
            (setf (gethash "type" new-link) "link")
            (setf (gethash "written" new-link) (get-universal-time))
            (log:info "~a: ~a"
                      (hash-get new-link (list "subreddit"))
                      (gethash "title" new-link))

            (setf (gethash "_id" new-link) (make-id new-link))
            (handler-case
                (progn
                  (cl-mango:doc-put "reddit" (to-json new-link))
                  (mailbox:post-mail (to-json (alex:alist-hash-table
                                               (list (cons "id" (gethash "id" new-link))
                                                     (cons "action" "add-link"))))
                                     *mailbox*))
              (cl-mango:unexpected-http-response (condition)
                (log:info "Error saving link: ~a ~a" condition new-link)))))))))

(defun update-subreddit (subreddit)
  (let ((yason:*parse-json-booleans-as-symbols* nil))
    (unless (ppcre:scan "u_" (gethash "display_name" subreddit))
      (dolist (link (hash-extract "data" (get-subreddit-links subreddit)))
        (handle-possible-new-link link)))))

(defun scan-links ()
  (dolist (subreddit (our-couch-query (list (cons "type" "subreddit"))))
    (log:info "~a" (gethash "display_name" subreddit))
    (update-subreddit subreddit)))

(defun find-links-by-pattern (pattern)
  (hash-extract "_id" (our-couch-query (list (cons "type" "link")
                                             (cons "title" (alex:alist-hash-table
                                                            (list (cons "$regex" pattern))))
                                             :limit 20000))))

(defun start-refresh-threads ()
  (bt:make-thread (lambda ()
                    (loop
                      (sync-subreddits)
                      (scan-links)
                      ;; (handler-case
                      ;;     (progn
                      ;;       (sync-subreddits)
                      ;;       (scan-links))
                      ;;   (type-error (condition)
                      ;;     (log:info "Type error:~a" condition)
                      ;;     (authenticate))
                      ;;   (ccl:no-applicable-method-exists (condition)
                      ;;     (declare (ignore condition))
                      ;;     (log:info "Reauthenticating.")
                      ;;     (authenticate))
                      ;;   (cl-mango:unexpected-http-response (condition)
                      ;;     (log:info "Unexpected http response:~a" condition)
                      ;;     nil))
                      ))
                  :name "fetchers"))

(defun stop-refresh-threads ()
  (cl-ivy:stop-thread-by-name "fetchers"))

(defun mark-subreddit-as-read (subreddit)
  (alex:when-let ((posts (our-couch-query
                          (list (cons "subreddit" subreddit)
                                (cons "suphidden" (alex:alist-hash-table
                                                   (list (cons "$exists" 'yason:false))))
                                (cons "type" "link"))
                          :limit 100000)))
    (dolist (post posts)
      (setf (gethash "suphidden" post) 't)
      (mango:doc-put "reddit" (to-json post)))
    (log:info (length posts))))

(defun mark-subreddit-as-unread (subreddit)
  (alex:when-let ((posts (our-couch-query
                          (list (cons "subreddit" subreddit)
                                (cons "suphidden" (alex:alist-hash-table
                                                   (list (cons "$exists" 'yason:true))))
                                (cons "type" "link"))
                          :limit 100000)))
    (dolist (post posts)
      (remhash "suphidden" post)
      (mango:doc-put "reddit" (to-json post)))
    (log:info (length posts))))

(defun %ps-load-comments (comment unique-id)
  (with-html
    (:script
      (ps:ps*
       `(with-document-ready
            (lambda ()
              (-> (sel ,(format nil
                                "#comment-~a" unique-id))
                  (load ,(format nil
                                 "/r/comment/~a"
                                 (gethash "id" comment))))))))))

(defun display-comment (comment)
  (let ((body (gethash "body" comment)))
    (with-html
      (:p
        (:span.pull-right
         ("~a/~a" (gethash "ups" comment)
                  (gethash "downs" comment)))
        (:a :href (format nil "https://reddit.com/u/~a" (gethash "author" comment)) (gethash "author" comment))
        (:br)
        (:span :style "font-size:14px;"
          (alex:if-let ((html-body (hash-get comment '("body_html"))))
            (:raw (html-entities:decode-entities html-body))
            (with-output-to-string (sink)
              (ignore-errors (cl-markdown:markdown body :stream *html*)))))
        (let ((replies (ignore-errors
                        (gethash "children"
                                 (gethash "data"
                                          (gethash "replies" comment))))))
          (dolist (reply (or replies nil))
            (:div :style "border-left:1px solid #eee;padding-left:10px;"
              (display-comment (gethash "data" reply)))))))))

(defun show-embedded-html (html)
  (declare (type string html))
  (with-html
    (:raw
     (html-entities:decode-entities html))))

(defun safe-doc-get (id)
  (handler-case (yason:parse (mango:doc-get "reddit" id))
    (mango:unexpected-http-response (condition)
      (log:info "FAULT: ~a" condition))))

(defun display-link (doc-id)
  (labels ((is-nsfw? (doc-hash)
             (eq (hash-get doc-hash '("over_18")) 'yason:true)))
    (alex:when-let ((doc-hash (car (our-couch-query (list (cons "type" "link")
                                                          (cons "id" doc-id))))))
      (with-html
        (let ((unique-id (format nil "~a" (uuid:make-v4-uuid))))
          (:div :class "col-md-4" :id (format nil "wx~a" unique-id)
            (:div :class "row"
              (:div.col-md-12
               (:div.panel.panel-default.panel-borders.panel-heading-fullwidth
                :style "border:1px solid rgb(7,7,7);"
                (:div :class "panel-heading" :style "font-size:14px;padding: 15px 15px 10px;"
                  (:div.tools
                   (:div.icon (:a :id (format nil "hider-~a" unique-id)
                                (:i :class "s7-look")))
                   (:script
                     (ps:ps*
                      `(-> (sel ,(format nil "#hider-~a" unique-id))
                           (click (lambda (e)
                                    (-> (sel ,(format nil "#link-~a" unique-id))
                                        (toggle))
                                    (-> e (prevent-default)))))))
                   (:div.icon (:a :target (format nil "win-~a" (uuid:make-v4-uuid))
                                :href (format nil "http://10.0.0.18:5984/_utils/#database/reddit/~a" doc-id)
                                (:i :class "s7-server")))
                   (:div.icon
                    (:a :target (format nil "linky-~a" (uuid:make-v4-uuid))
                      :href (format nil "/link/~a" (hash-get doc-hash '("_id")))
                      (:i :class "s7-link")))
                   (:div.icon :id (format nil "favorite-~a" unique-id)
                              (:span.s7-download))
                   (:div.icon :id (format nil "hide-~a" unique-id)
                              (:span.s7-close-circle)))
                  (:div.pull-right
                   (:span :style "font-weight:bold;" :id (format nil "ups~a" (gethash "id" doc-hash))
                     (gethash "ups" doc-hash 0)))
                  (alex:when-let ((title (gethash "title" doc-hash)))
                    (alex:if-let ((url (gethash "url" doc-hash)))
                      (:a :style "font-weight:bold;"
                        :target (format nil "win-~a" (uuid:make-v4-uuid))
                        :href url (html-entities:decode-entities title))
                      title)
                    (:br)
                    (:span
                      (:a :target (format nil "win-~a" (uuid:make-v4-uuid))
                        :href (format nil "https://www.reddit.com~a" (gethash "permalink" doc-hash))
                        (format nil "~a in ~a on ~a"
                                (gethash "author" doc-hash)
                                (gethash "subreddit" doc-hash)
                                (local-time:to-rfc1123-timestring
                                 (local-time:unix-to-timestamp
                                  (rationalize (gethash "created_utc" doc-hash))))))))
                  (:script
                    (ps:ps*
                     `(with-document-ready
                          (lambda ()
                            (-> (sel ,(format nil "#favorite-~a" unique-id))
                                (click (lambda (e)
                                         (chain (sel ,(format nil "#wx~a" unique-id))
                                                (load ,(format nil "/link/favorite/~a" (gethash "id" doc-hash))))
                                         (-> (sel ,(format nil "#wx~a" unique-id))
                                             (toggle))
                                         (-> e (prevent-default)))))
                            (-> (sel ,(format nil "#hide-~a" unique-id))
                                (click (lambda (e)
                                         (chain (sel ,(format nil "#wx~a" unique-id))
                                                (load ,(format nil "/link/hide/~a" (hunchentoot:url-encode
                                                                                    (gethash "id" doc-hash)))))
                                         (-> (sel ,(format nil "#wx~a" unique-id))
                                             (toggle))
                                         (-> e (prevent-default))))))))))
                (if (is-nsfw? doc-hash)
                  (let ((nsfw-button-id (format nil "nsfw~a" unique-id)))
                    (:button.btn-primary.btn-xs :id nsfw-button-id
                                                :style "margin-top:5px;margin-left:5px;"
                                                :onclick (ps:ps*
                                                          `(progn
                                                             (-> (sel ,(format nil "#link-~a" unique-id))
                                                                 (load ,(format nil "/link/body/~a" (gethash "_id" doc-hash))))
                                                             (-> (sel ,(format nil "#~a" nsfw-button-id)) (hide))))
                                                "NSFW"))
                  (:script
                    (ps:ps*
                     `(with-document-ready
                          (lambda ()
                            (-> (sel ,(format nil "#link-~a" unique-id))
                                (load ,(format nil "/link/body/~a" (gethash "_id" doc-hash))))
                            (update-graph ,(gethash "id" doc-hash)))))))
                (:div.panel-body :id (format nil "graph~a" (gethash "id" doc-hash))
                                 :style "height:30px;margin-bottom:5px;"
                                 )
                (:div.panel-body :class "link-body"
                                 :style "padding: 10px 15px 15px;"
                                 :id (format nil "link-~a" unique-id))
                (:div :class "panel-heading"
                  (:div
                    :class "icon"
                    :onclick (ps:ps* `(if (string= (-> (sel ,(format nil "#comments-~a" (gethash "id" doc-hash)))
                                                       (css "display"))
                                                   "none")
                                        (progn
                                          (-> (sel ,(format nil "#comments-~a"
                                                            (gethash "id" doc-hash)))
                                              (toggle))
                                          (-> (sel ,(format nil "#comments-~a" (gethash "id" doc-hash)))
                                              (load ,(format nil "/comments/~a" (gethash "id" doc-hash)))))
                                        (-> (sel ,(format nil "#comments-~a"
                                                          (gethash "id" doc-hash)))
                                            (toggle))))
                    
                    (:span :class "s7-download")))
                (:div.panel-body :id (format nil "comments-~a" (gethash "id" doc-hash))
                                 :style "display:none;"
                                 "Loading..."))))))))))

(defroute stop-threads ("/stop-fetchers") ()
  (stop-refresh-threads)
  (hunchentoot:redirect "/links"))

(defroute start-threads ("/start-fetchers") ()
  (start-refresh-threads)
  (hunchentoot:redirect "/links"))

(defun fetcher-is-running-p ()
  (member "fetchers" (mapcar #'bt:thread-name
                             (bt:all-threads))
          :test #'string=))

(defmacro links-page (&rest body)
  `(with-page ()
     (:div :class "row"
       (:div :class "col-md-12"
         (:form :method :post :action "/search"
           (:div :class "col-md-3"
             (:div :class "btn-group btn-space"
               (:a :class "btn btn-default btn-xs"
                 :type "button" :href "/favorites" "Favorites")
               (:a :class "btn btn-default btn-xs" :type "button" :href "/links" "Index")
               (:a :class "btn btn-default btn-xs" :type "button" :href "/live" "Live")
               (:a :class "btn btn-default btn-xs" :id "hidebutton" :type "button" :href "#" "C")
               (:script (ps:ps (-> (sel "#hidebutton")
                                   (click (lambda (e)
                                            (-> (sel ".link-body") (hide))
                                            (-> e (prevent-default)))))))
               (if (fetcher-is-running-p)
                 (:a :class "btn btn-primary btn-xs"
                   :type "button" :href "/stop-fetchers" "Stop Collector")
                 (:a :class "btn btn-default btn-xs"
                   :type "button" :href "/start-fetchers" "Start Collectors"))
              
               (:input :name "pattern" :type "text"
                 :class "input-xs form-control pull-left" :placeholder "Search..." :id "searchtext"))))
         (:div :class "col-md-9"
           (:div :class "btn-group btn-space"
             (map 'nil (lambda (subreddit)
                         (:a :class "btn btn-primary btn-xs"
                           :href (format nil "/subreddit/~a" (gethash "name" subreddit))
                           (gethash "display_name" subreddit)))
                  (sort (our-couch-query (list (cons "type" "subreddit")))
                        #'string<
                        :key (lambda (x)
                               (string-upcase (gethash "display_name" x)))))))))
     ,@body
     (:script
       (:raw
        (ps:ps
          (defun update-graph (link-id)
            (-> $ (ajax (create :type "get"
                                :url (concatenate 'string "/graph/history/" link-id)
                                :success (lambda (obj)
                                           (-> (sel (concatenate 'string "#graph" link-id))
                                               (sparkline obj
                                                          (create :width "100%"
                                                                  :height "35"
                                                                  :line-width 1.7)))))))))))))

(defun display-links (link-ids)
  (links-page
   (:div :class "col-md-12" :id "links"
     (map nil #'display-link link-ids))))

(defun scan-for-duplicates ()
  (map nil
       (lambda (subreddit)
         (let ((name (gethash "display_name" subreddit)))
           (log:info name))
         (let ((ids (our-couch-query (list (cons "type" "link")
                                           (cons "subreddit" (gethash "display_name" subreddit)))
                                     :fields (list "id")
                                     :limit 500000)))
           (map nil
                (lambda (ids)
                  (let ((dupes (our-couch-query (list (cons "type" "link")
                                                      (cons "id" (gethash "id" ids))))))
                    (when (< 1 (length dupes))
                      (map nil (lambda (bye)
                                 (mango:doc-delete "reddit" (gethash "_id" bye) (gethash "_rev" bye)))
                           (cdr dupes))
                      (let ((good (gethash "id" (car dupes))))
                        (log:info "purged" good)))))
                ids)))
       (get-db-subreddits)))

(defun purge-link (link)
  (let ((comments (our-couch-query (list (cons "link_id" (format nil "t3_~a"
                                                                 (gethash "id" link))))
                                   :limit 20000)))
    (when (< 0 (length comments))
      (log:info "comments: ~a" (length comments))
      (mapcar (lambda (comment)
                (handler-case 
                    (mango:doc-delete "reddit"
                                (gethash "_id" comment)
                                (gethash "_rev" comment))
                  (cl-mango:unexpected-http-response (condition)
                    (log:info "delete gave an error. ~a ~a"
                              (cl-mango::status-body condition)
                              (cl-mango::status-code condition))
                    nil)))
              comments))
    (handler-case
        (mango:doc-delete "reddit"
                    (gethash "_id" link)
                    (gethash "_rev" link))
      (cl-mango:unexpected-http-response (condition)
        (log:info "delete gave an error. ~a ~a"
                  (cl-mango::status-body condition)
                  (cl-mango::status-code condition))
        nil))))

(defun purge-subreddit (subreddit-name)
  (let ((links (our-couch-query (list (cons "subreddit" subreddit-name))
                                :limit 1000)))
    (log:info "links: ~a" (length links))
    
    (when (= 0 (length links))
      (return-from purge-subreddit))

    (mapcar #'purge-link links)

    (purge-subreddit subreddit-name)))

(defun find-orphaned-links ()
  (let ((links (our-couch-query (list (cons "type" "link"))
                                :fields (list "_id" "_rev" "subreddit")
                                :limit 100000))
        (subreddit-names (mapcar (lambda (x)
                                   (gethash "display_name" x))
                                 (our-couch-query (list (cons "type" "subreddit"))
                                                  :fields (list "display_name")))))
    (remove-if #'(lambda (link)
                   (member (gethash "subreddit" link) subreddit-names
                           :test #'string=))
               links)))

(defun remove-orphaned-links ()
  (map 'nil #'purge-link (find-orphaned-links)))

(defun mark-everything-unread ()
  (dolist (subreddit (hash-extract "display_name" (get-db-subreddits)))
    (mark-subreddit-as-unread subreddit)))

(defun mark-everything-read ()
  (dolist (subreddit (get-db-subreddits))
    (mark-subreddit-as-read (gethash "display_name" subreddit))))

(defroute get-votes ("/votes/:id") ()
  (setf (hunchentoot:content-type*) "application/json")
  (alex:when-let ((vote-hash (car (our-couch-query (list (cons "id" id)
                                                         (cons "type" "link"))
                                                   :fields (list "ups" "downs")))))
    (with-output-to-string (sink)
      (yason:encode vote-hash sink))))

(defroute display-single-link ("/link/:id") ()
  (with-page ()
    (display-link id)))

(defroute link-search ("/search" :method :post) (pattern)
  (display-links (find-links-by-pattern pattern)))

(defroute display-subreddit ("/subreddit/:subid") ()
  (let ((links (hash-extract "id"
                             (our-couch-query (list (cons "type" "link")
                                                    (cons "subreddit_id" subid))
                                              :fields (list "id")
                                              :sort (list
                                                     (alex:alist-hash-table
                                                      (list (cons "created_utc" "desc"))))))))
    (log:info (length links))
    (log:info (car links))
    (display-links links)))

(defroute favorites ("/favorites") ()
  (let ((link-ids (hash-extract "id"
                                (gethash "rows"
                                         (yason:parse
                                          (cl-mango:query-view "reddit" "tests" "favorites"
                                                               (list (cons "limit" "500")
                                                                     (cons "descending" "true"))))))))
    (display-links link-ids)))

(defroute links ("/links") ()
  (display-links
   (hash-extract "id"
                 (gethash "rows"
                          (yason:parse
                           (cl-mango:query-view "reddit" "tests" "links"
                                                (list (cons "limit" "100")
                                                      (cons "descending" "true"))))))))

(defroute index ("/") ()
  (hunchentoot:redirect "/links"))

(defroute hide-link ("/link/hide/:id") ()
  (alex:when-let ((the-link (our-couch-query (list (cons "id" id)
                                                   (cons "type" "link"))
                                             :limit 1)))
    (map nil (lambda (link)
               (setf (gethash "suphidden" link) t)
               (mango:doc-put "reddit" (to-json link)))
         (remove-if-not (lambda (x)
                          (string= "link" (hash-get x (list "type"))))
                        the-link)))
  "ok")

(defroute favorite-link ("/link/favorite/:id") ()
  (alex:when-let ((the-link (car (our-couch-query (list (cons "id" id)
                                                        (cons "type" "link"))))))
    (alex:if-let ((is-hidden-already (gethash "suphidden" the-link nil)))
      ;; The link is being hidden from /favorites and not /links
      ;; So send it back into the viewable pile.
      (progn
        (remhash "favorite" the-link)
        (remhash "suphidden" the-link)
        (mango:doc-put "reddit" (to-json the-link)))
      (progn
        (setf (gethash "favorite" the-link) 't)
        (setf (gethash "suphidden" the-link) 't)
        (mango:doc-put "reddit" (to-json the-link))))))

(defroute render-comment ("/r/comment/:id") ()
  (let ((comments (our-couch-query (list (cons "type" "comment")
                                         (cons "id" id)))))
    (with-html-string
      (display-comment (car comments)))))

(defroute show-comments ("/comments/:id") ()
  (let ((link (car (our-couch-query (list (cons "id" id))))))
    (comments-handle-link (gethash "permalink" link)))
  (let ((comments (our-couch-query
                   (list (cons "type" "comment")
                         (cons "link_id" (format nil "t3_~a" id)))
                   :sort (list (alex:alist-hash-table
                                (list (cons "ups" "desc")))))))
    (with-html-string (map 'nil #'display-comment comments))))

(defroute show-link-body ("/link/body/:id") ()
  (alex:when-let ((doc-hash (yason:parse
                             (mango:doc-get "reddit" id))))
    (with-html-string
      (:div.row
       (:div.col-md-12
        (let ((crossposted-reddit-video (hash-get doc-hash '("crosspost_parent_list" 0 "secure_media" "reddit_video" "fallback_url")))
              (crossposted-media-content (hash-get doc-hash '("crosspost_parent_list" 0 "media_embed" "content")))
              (reddit-preview-of-imgur-gif (hash-get doc-hash '("preview" "reddit_video_preview" "fallback_url")))
              (is-reddit-video (hash-get doc-hash '("secure_media" "reddit_video" "fallback_url")))
              (is-embedded-image (hash-get doc-hash '("preview" "images")))
              (has-selftext (hash-get doc-hash '("selftext_html")))
              (has-oembed-media (hash-get doc-hash '("secure_media" "oembed" "html")))
              (url-is-imgur-image (alex:when-let ((url (gethash "url" doc-hash)))
                                    (when (ppcre:scan "imgur.com" url)
                                      (ppcre:regex-replace "https?://i?.?imgur.com/" url ""))))
              (url-is-video (alex:when-let ((url (gethash "url" doc-hash)))
                              (when (ppcre:scan ".mp4|.MP4" url)
                                url)))
              (url-is-image (alex:when-let ((url (gethash "url" doc-hash)))
                              (when (ppcre:scan ".jpg$|.png$|.gif$|.JPG$|.PNG$|.GIF$" url)
                                url)))
              (has-crosspost-parent-media (let ((crosspost-parent-list
                                                  (hash-get doc-hash '("crosspost_parent_list"))))
                                            (when (listp crosspost-parent-list)
                                              (hash-get (car crosspost-parent-list)
                                                        '("preview" "reddit_video_preview" "fallback_url"))))))
          (cond (crossposted-reddit-video (:video :preload "auto" :class "img-responsive" :controls 1
                                            (:source :src crossposted-reddit-video)))
                (reddit-preview-of-imgur-gif (:video :preload "auto" :class "img-responsive" :controls 1
                                               (:source :src reddit-preview-of-imgur-gif)))
                (has-oembed-media (:raw (html-entities:decode-entities
                                         has-oembed-media)))
                (is-reddit-video (:video :preload "auto" :class "img-responsive" :controls 1
                                   (:source :src is-reddit-video)))
                ;; Matches if the post is a crosspost and the original
                ;; has a video hosted at reddit
                (crossposted-media-content (:raw (html-entities:decode-entities
                                                  crossposted-media-content)))
                (has-crosspost-parent-media (progn
                                              (log:info "Yes!")
                                              (:video :preload "auto" :class "img-responsive" :controls 1
                                                (:source :src has-crosspost-parent-media))))
                (is-embedded-image (dolist (image-hash is-embedded-image)
                                     (cond ((hash-get image-hash '("variants"))
                                            (let ((has-mp4 (hash-get image-hash
                                                                     '("variants"
                                                                       "mp4"
                                                                       "source"
                                                                       "url")))
                                                  (has-gif (hash-get image-hash
                                                                     '("variants"
                                                                       "gif"
                                                                       "source"
                                                                       "url")))
                                                  (has-still-image (hash-get image-hash '("source" "url"))))
                                              (cond (has-mp4 (:video :preload "auto" :class "img-responsive"
                                                               :controls 1
                                                               (:source :src (html-entities:decode-entities has-mp4))))
                                                    (has-gif (:img :class "img-responsive"
                                                               :src (html-entities:decode-entities has-gif)))
                                                    (has-still-image (:img :class "img-responsive"
                                                                       :src (html-entities:decode-entities has-still-image)))))))))
                (has-selftext (:raw (html-entities:decode-entities has-selftext)))
                (url-is-image (:img.img-responsive :src url-is-image))
                (url-is-video (:video :class "img-responsive" :preload "auto" :controls "1"
                                (:source :src (html-entities:decode-entities url-is-video))))
                (url-is-imgur-image (:blockquote.imgur-embed-pub :data-id url-is-imgur-image)
                                    (:script :src "//s.imgur.com/min/embed.js")))))))))

(defun cleanup ()
  (labels ((make-shadow-doc (doc-hash)
             (let ((new-doc (make-hash-table :test #'equal)))
               (setf (gethash "_id" new-doc) (gethash "_id" doc-hash))
               (setf (gethash "_rev" new-doc) (gethash "_rev" doc-hash))
               (setf (gethash "permalink" new-doc) (gethash "permalink" doc-hash))
               (setf (gethash "title" new-doc) (gethash "title" doc-hash))
               new-doc)))
    (let ((now (get-universal-time)))
      (length (mapcar (lambda (doc)
                        (mango:doc-put "reddit" (to-json (make-shadow-doc doc))))
                      (our-couch-query (list (cons "type" "link")
                                             (cons "written" (alex:alist-hash-table
                                                              (list (cons "$lt" (- now 172800))))))
                                       :fields (list "_id" "_rev" "title" "permalink")
                                       :limit 10000))))))

(defun delete-comments ()
  (let ((comments (our-couch-query (list (cons "type" "comment"))
                                   :limit 10000
                                   :fields (list "_id" "_rev"))))
    (dolist (comment comments)
      (mango:doc-delete "reddit" (gethash "_id" comment) (gethash "_rev" comment)))
    (length comments)))

(defroute make-history ("/graph/history/:id") ()
  (setf (hunchentoot:content-type*) "application/json")
  (alex:if-let ((doc (handler-case (mango:doc-get "reddit"
                                                  (format nil "~a:like-cache" id))
                       (mango:unexpected-http-response (condition)
                         (declare (ignore condition))
                         nil))))
    (to-json (gethash "ups" (yason:parse doc)))
    "[]"))

(defroute make-graph ("/graph/:id") ()
  (with-html-string
    (:div :class "col-md-3"
      (:div :id (format nil "graph~a" id)))))

(defroute ajax-link ("/singlelink/:id") ()
  (with-html-string (display-link id)))

(defroute live ("/live") ()
  (links-page
   (:div :id "graphs" :class "col-md-12")
   (:div :id "loader")
   (:script
     (:raw
      (ps*
       `(progn
          (defvar ws nil)
          (defun ws-connect ()
            (defvar ws nil)
            (setf ws (new (-web-socket "ws://127.0.0.1:8088/ws/news")))
            (setf (@ ws onopen)
                  (lambda ()
                    (-> console (log "Connected."))
                    (set-interval (lambda ()
                                    (-> ws (send "ping")))
                                  5000)))
            (setf (@ ws onerror) (lambda (event)
                                   (-> console (log (concatentate 'string
                                                                  "Websockets error"
                                                                  (@ event data)
                                                                  "\n")))))
            (setf (@ ws onclose) (lambda ()
                                   (-> console (log "Connection closed."))))
            (setf (@ ws onmessage)
                  (lambda (event)
                    (if (string= (@ event data) "pong")
                      (-> console (log "pongers")))
                    (if (string= (@ event data) "ping")
                      (-> ws (send "pong"))
                      (progn
                        (let ((env (-> -j-s-o-n (parse (@ event data)))))
                          (when (string= (@ env action) "add-link")
                            (add-link (@ env id)))
                          (when (string= (@ env action) "update-votes")
                            (update-votes env))))))))

          (defun update-votes (env)
            (let ((current-ups (parse-int (-> (sel (+ "#ups" (@ env id)))
                                              (html))))
                  (new-ups (parse-int (@ env ups))))
              (unless (equal current-ups new-ups)
                (if (< current-ups new-ups)
                  (progn
                    (update-graph (@ env id))
                    (-> (sel (+ "#ups" (@ env id))) (remove-class "text-danger"))
                    (-> (sel (+ "#ups" (@ env id))) (add-class "text-success"))
                    (-> (sel (+ "#ups" (@ env id))) (html (@ env ups))))
                  (progn
                    (-> (sel (+ "#ups" (@ env id))) (remove-class "text-success"))
                    (-> (sel (+ "#ups" (@ env id))) (add-class "text-danger"))
                    (-> (sel (+ "#ups" (@ env id))) (html (@ env ups))))))))

          (defun update-graph (link-id)
            (-> $ (ajax (create :type "get"
                                :url (concatenate 'string "/graph/history/" link-id)
                                :success (lambda (obj)
                                           (-> (sel (concatenate 'string "#graph" link-id))
                                               (sparkline obj
                                                          (create :width "100%"
                                                                  :height "35"
                                                                  :line-width 1.7))))))))
          (defun add-link (link-id)
            (-> $ (ajax (create :type "get"
                                :url (concatenate 'string "/singlelink/" link-id)
                                :error (lambda (e)
                                         (-> console (log e)))
                                :success (lambda (text)
                                           (-> (sel "#loader")
                                               (append text)))))))
          
          (-> ($ document)
              (ready
               (lambda ()
                 (ws-connect))))))))))

(defclass user (hunchensocket:websocket-client)
  ((name :initarg :user-agent
         :accessor user-name
         :initform (error "Needs a name."))))

(defclass chat-room (hunchensocket:websocket-resource)
  ((name :initarg :name
         :initform (error "Needs a name.")
         :accessor chat-room-name))
  (:default-initargs :client-class 'user))

(defvar *chat-rooms* (list (make-instance 'chat-room :name "/ws/news")))

(defun find-room (request)
  (log:info "~a" (hunchentoot:script-name request))
  (find (hunchentoot:script-name request) *chat-rooms* :test #'string= :key #'chat-room-name))

(pushnew 'find-room hunchensocket:*websocket-dispatch-table*)

(defun broadcast (room message &rest args)
  (dolist (peer (hunchensocket:clients room))
    (hunchensocket:send-text-message peer (apply #'format nil message args))))

(defun new-articles-to-live ()
  (dolist (link (our-couch-query (list (cons "type" "link")
                                       (cons "suphidden" (alex:alist-hash-table
                                                          (list (cons "$exists" 'yason:false)))))))
    (mailbox:post-mail (to-json
                        (alex:alist-hash-table
                         (list (cons "action" "add-link")
                               (cons "id" (gethash "id" link)))))
                       *mailbox*)))

(defmethod hunchensocket:client-connected ((room chat-room) user)
  (declare (ignore user))
  (new-articles-to-live))

(defmethod hunchensocket:client-disconnected ((room chat-room) user)
  (broadcast room "~a has left ~a" (user-name user) (chat-room-name room)))

(defmethod hunchensocket:text-message-received ((room chat-room) user message)
  ;;(log:info "Message: ~a" message)
  ;;(broadcast room "~a says ~a" (name user) message)
  )

(defvar *ws-server* nil)

(defun start-ws-reader-thread ()
  (bt:make-thread (lambda ()
                    (loop
                      (alex:when-let ((message (mailbox:read-mail *mailbox*)))
                        (broadcast (car *chat-rooms*) message))))
                   :name "ws broadcast"))

(defun start-ws-server ()
  (unless *ws-server*
    (setf *ws-server* (make-instance 'hunchensocket:websocket-acceptor :port 8088))
    (hunchentoot:start *ws-server*)))

(defun prune-links ()
  (let ((subreddits (hash-extract "display_name"
                                  (our-couch-query (list (cons "type" "subreddit")))))
        (links (our-couch-query (list (cons "type" "link"))
                                :fields (list "_id" "_rev" "subreddit")
                                :limit 100000)))
    (dolist (link links)
      (unless (member (gethash "subreddit" link)
                      subreddits
                      :test #'string=)
        (mango:doc-delete "reddit"
                          (gethash "_id" link)
                          (gethash "_rev" link))))
    (when (= 100000 (length links))
      (prune-links))))

(defun prune-comments ()
  (let ((all-comments (our-couch-query (list (cons "type" "comment"))
                                       :limit 1)))))

(defun kill-it-all ()
  (dolist (sub (our-couch-query (list (cons "type" "subreddit"))
                                :fields (list "_id" "_rev")))
    (mango:doc-delete "reddit"
                      (gethash "_id" sub)
                      (gethash "_rev" sub)))
  (dolist (link (our-couch-query (list (cons "type" "link"))
                                 :fields (list "_id" "_rev")
                                 :limit 10000))
    (mango:doc-delete "reddit" (gethash "_id" link) (gethash "_rev" link))))


(defun test ()
  (let ((cache-items (our-couch-query (list (cons "type" "like-cache"))
                                      :fields (list "_id" "_rev")
                                      :limit 10000)))
    (let ((mango:*explain* nil))
      (mango:bulk-docs "reddit" (to-json
                                 (alex:alist-hash-table
                                  (list (cons "docs"
                                              (mapcar (lambda (doc)
                                                        (alexandria:alist-hash-table
                                                         (list (cons "_id" (gethash "_id" doc))
                                                               (cons "_rev" (gethash "_rev" doc))
                                                               (cons "deleted" 'yason:true))))
                                                      cache-items)))))))
    (when (equalp (length cache-items) 10000)
      (log:info "cycle")
      (test))))
