;;; -*- mode: Lisp; Syntax: common-lisp; Package: sup; Base: 10 eval: (hs-hide-all) -*-

(defpackage :sup
  (:use #:cl
        #:json-mop
        #:easy-routes
        #:parenscript
        #:spinneret
        #:cl-mango
        #:cl-hash-util)
  (:import-from :alexandria
                when-let
                when-let*
                if-let
                if-let
                alist-hash-table))

(in-package #:sup)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf cl-mango:*host* "127.0.0.1")
  (setf cl-mango:*port* 5984)
  (setf cl-mango:*scheme* :http)
  (setf cl-mango:*username* "admin")
  (setf cl-mango:*password* "h4r01d")

  (setf lparallel:*kernel* (lparallel:make-kernel 10 :name "suckit"))
  (defparameter *client-id* "NgPcSAMcznk3aQ")
  (defparameter *client-secret* "nZZdcddz-BbYEVwpwkhiGOjzRoQ"))

(defparameter *reddit-user* nil)

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

(defmacro couch-query (selector &rest args)
  `(gethash "docs" (yason:parse
                    (doc-find "reddit" (make-selector ,selector ,@args)))))

(defun make-request (url)
  (drakma:http-request (format nil "~a~a" "https://www.reddit.com" url)))

(defun authenticate ()
  (setf *reddit-user*
        (json-mop:json-to-clos (flexi-streams:octets-to-string
                                (let ((drakma:*text-content-types* (list (list "application/json"))))
                                  (drakma:http-request "https://www.reddit.com/api/v1/access_token"
                                                       :basic-authorization (list *client-id* *client-secret*)
                                                       :additional-headers (list (cons "User-Agent" "Supyawl/0.1 by clintm"))
                                                       :accept "application/json"
                                                       :content-type "application/json"
                                                       :parameters (list (cons "grant_type" "password")
                                                                         (cons "username" "clintm")
                                                                         (cons "password" "@Fancywalking2reddit"))
                                                       :method :post
                                                       :preserve-uri t)))
                               'access-token)))

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

(defparameter *listener* nil)

(defun start-server (&key (port 8086))
  (unless *listener*
    (setf *listener* (make-instance 'easy-routes:easy-routes-acceptor
                                    ;;:address "127.0.0.1"
                                    :port port)))
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

(defparameter *person* nil)

(defmacro with-session ((&key (require-user t)) &body body)
  (alexandria:with-gensyms (this-person)
    `(let* ((,this-person (alexandria:when-let ((uid (hunchentoot:cookie-in "uid")))
                            (car (couch-query (list (cons "type" "person")
                                                    (cons "uid" uid)))))))
       (let ((*person* ,this-person))
         ,(when require-user
            `(unless (and *person* (hash-get *person* (list "email")))
               (hunchentoot:redirect "/login")))
         ,@body))))

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
              :type "text/css" :href "https://r.ivy.io/am/lib/stroke-7/style.css")
            (:link :rel "stylesheet"
              :type "text/css"
              :href "https://r.ivy.io/am/lib/jquery.nanoscroller/css/nanoscroller.css")
            (:link :rel "stylesheet" :type "text/css" :href "https://r.ivy.io/am/css/style.css")
            (:script :src "https://r.ivy.io/am/lib/jquery/jquery.min.js")
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
            (:script :src "https://r.ivy.io/am/lib/jquery.nanoscroller/javascripts/jquery.nanoscroller.min.js")
            (:script :src "https://r.ivy.io/am/js/main.min.js")
            (:script :src "https://r.ivy.io/am/lib/bootstrap/dist/js/bootstrap.min.js")
            (:script :src "https://r.ivy.io/am/lib/jquery-ui/jquery-ui.min.js")
            (:script :src "https://r.ivy.io/gfycat.min.js")))))

(defmacro with-login-page (&rest body)
  `(with-page (:body-class "am-splash-screen")
     (:div :class "am-wrapper am-login"
       (:div :class "am-content"
         (:div :class "main-content"
           (:div :class "login-container"
             (:div :class "panel panel-default"
               (:div :class "panel-body"
                 ,@body))))))))

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
  (when-let* ((preview (gethash "preview" doc-hash))
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
     ;;(sleep 2)
     (dolist (listing (yason:parse data))
       (dolist (comment (gethash "children" (gethash "data" listing)))
         (let* ((new-comment (gethash "data" comment))
                (comment-in-db (car
                                (couch-query (list (cons "id" (gethash "id" new-comment))
                                                   (cons "type" "comment"))
                                             :limit 1
                                             :fields (list "_id" "_rev" "type")))))
           (setf (gethash "type" new-comment) "comment")
           (if comment-in-db
               (progn
                 (setf (gethash "_id" new-comment) (gethash "_id" comment-in-db))
                 (setf (gethash "_rev" new-comment) (gethash "_rev" comment-in-db)))
               (setf (gethash "_id" new-comment) (make-id new-comment)))
           ;;(log:info "writing comment")
           (handler-case
               (doc-put "reddit" (to-json new-comment))
             (cl-mango:unexpected-http-response (condition)
               (let ((status (cl-mango::status-body condition)))
                 (log:info status))))))))))

(defun scan-comments ()
  (log:info "comments")
  (map 'nil #'comments-handle-link
       (hash-extract "permalink"
                     (couch-query (list (cons "type" "link")
                                        (cons "suphidden" (alist-hash-table
                                                           (list (cons "$exists" 'yason:false)))))
                                  :fields (list "permalink")
                                  :limit 10000)))
  (mapcar #'comments-handle-link
          (hash-extract "permalink"
                        (couch-query (list (cons "type" "link")
                                           (cons "favorite" 'yason:true))))))

(defun link-is-image-p (link)
  (cond ((ppcre:scan ".jpg$|.png$|.gif$" link) link)
        ;;((ppcre:scan "imgur.com" link) (format nil "~a.gif" link))
        (t nil)))

(defun get-latest-post-id-for-subreddit (subreddit)
  (handler-case (let ((dochash (car (couch-query (list (cons "subreddit" subreddit))
                                                 :limit 1
                                                 :sort (list (alist-hash-table
                                                              (list (cons "created" "desc"))))
                                                 :fields (list "id")))))
                  (when dochash
                    (gethash "id" dochash)))
    (cl-mango:unexpected-http-response (condition)
      (log:info (cl-mango::status-body condition))
      nil)))

(defun has-subreddit-p (subreddit-name)
  (< 0 (length
        (couch-query (list (cons "display_name" subreddit-name)
                           (cons "type" "subreddit"))))))

(defun get-db-subreddits ()
  (couch-query (list (cons "type" "subreddit"))
               :limit 100000
               :fields (list "_id" "_rev" "display_name")))

(defun get-reddit-subreddits ()
  (hash-extract "data"
                (gethash "children" (gethash "data" (yason:parse
                                                     (reddit-request "/subreddits/mine"))))))

(defun sync-subreddits ()
  (let* ((reddit-subreddits (get-reddit-subreddits))
         (reddit-names (hash-extract "display_name" reddit-subreddits))
         (db-subreddits (get-db-subreddits))
         (db-names (hash-extract "display_name" db-subreddits)))
    

    ;; Confirm that all subscribed subreddits are in the database.
    (map nil (lambda (reddit-name)
               (unless (member reddit-name db-names :test #'string=)
                 (let ((reddit-hash (car (remove-if-not #'(lambda (x)
                                                            (string= (gethash "display_name" x) reddit-name))
                                                        reddit-subreddits))))
                   (setf (gethash "type" reddit-hash) "subreddit")
                   (doc-put "reddit" (to-json reddit-hash))
                   (log:info "missing" (gethash "display_name" reddit-hash)))))
         reddit-names)
    ;; Remove db subreddits that aren't currently subscribed.
    (map nil (lambda (db-name)
               (unless (member db-name reddit-names :test #'string=)
                 (let ((db-record (car (remove-if-not #'(lambda (x)
                                                          (string= (gethash "display_name" x) db-name))
                                                      db-subreddits))))
                   (doc-delete "reddit" (gethash "_id" db-record) (gethash "_rev" db-record)))))
         db-names)))

(defun hash-extract (name-string list-of-hashes)
  (mapcar (lambda (x)
            (gethash name-string x))
          list-of-hashes))

(defun existing-link-info (link-id)
  (let ((link-info (car (couch-query (list (cons "id" (format nil "~a:~a" link-id "link"))
                                           (cons "type" "link"))
                                     :fields (list "_id" "_rev" "suphidden")))))
    (when (typep link-info 'hash-table)
      (values (gethash "_id" link-info)
              (gethash "_rev" link-info)
              (gethash "suphidden" link-info)))))

(defun get-subreddit-links (subreddit &key (latest-id nil))
  (declare (type hash-table subreddit))
  (gethash "children"
           (gethash "data"
                    (yason:parse
                     (handler-case (drakma:http-request
                                    (let ((url (gethash "url" subreddit)))
                                      (if latest-id
                                          (format nil "https://www.reddit.com~a/.json?limit=100&after=~a"
                                                  url
                                                  latest-id)
                                          (format nil "https://www.reddit.com~a/.json?limit=100"
                                                  url))))
                       (usocket:timeout-error (condition)
                         (declare (ignore condition))
                         (log:info "Timeout error.  Restarting.")
                         (get-subreddit-links subreddit :latest-id latest-id))
                       (error (condition)
                         (log:info "Some sort of error:" condition)
                         (log:info "Restarting.")
                         (get-subreddit-links subreddit :latest-id latest-id)))))))

(defun handle-possible-new-link (new-link)
  (multiple-value-bind (id revision hidden)
      (existing-link-info (gethash "id" new-link))
    (unless hidden
      (setf (gethash "type" new-link) "link")
      (setf (gethash "written" new-link) (get-universal-time))
      (if revision
          (progn
            (setf (gethash "_id" new-link) id)
            (setf (gethash "_rev" new-link) revision))
          (progn
            (setf (gethash "_id" new-link) (make-id new-link))))
      (handler-case
          (progn
            (cl-mango:doc-put "reddit" (to-json new-link))
            (comments-handle-link (gethash "permalink" new-link)))
        (cl-mango:unexpected-http-response (condition)
          (declare (ignore condition))
          nil)))))

(defun scan-links ()
  (log:info "links")
  (map nil (lambda (subreddit)
             (unless (ppcre:scan "u_" (gethash "display_name" subreddit))
               (let ((latest-id (get-latest-post-id-for-subreddit
                                 (gethash "display_name" subreddit))))
                 (map nil #'handle-possible-new-link
                      (hash-extract "data"
                                    (get-subreddit-links subreddit :latest-id (or latest-id 0)))))))
       (couch-query (list (cons "type" "subreddit")))))

(defun find-links-by-pattern (pattern)
  (hash-extract "_id"
                (gethash "docs"
                         (yason:parse
                          (doc-find "reddit" (make-selector
                                              (list (cons "type" "link")
                                                    (cons "title" (alist-hash-table
                                                                   (list (cons "$regex" pattern)))))
                                              :limit 20000))))))

(defparameter *feed-list* (list (list "Clozure master" "https://github.com/Clozure/ccl/commits/master.atom")
                                (list "log4cl master" "https://github.com/sharplispers/log4cl/commits/master.atom")))

(defun update-feeds ()
  (map nil
       (lambda (feed)
         (destructuring-bind (feed-name feed-url) feed
           (let* ((parsed (ignore-errors
                           (gethash :entries
                                    (cl-feedparser:parse-feed
                                     (drakma:http-request feed-url))))))
             (map nil (lambda (story)
                        (unless (couch-query (list (cons "id" (gethash :id story))))
                          (doc-put "reddit" (to-json
                                             (alist-hash-table
                                              (list (cons "id" (gethash :id story))
                                                    (cons "type" "link")
                                                    (cons "ups" 0)
                                                    (cons "downs" 0)
                                                    (cons "selftext_html" (hash-get story (list :content 0 :value)))
                                                    (cons "url" (gethash :link story))
                                                    (cons "permalink" (gethash :link story))
                                                    (cons "subreddit" "RSS")
                                                    (cons "name" "RSS")
                                                    (cons "created" (get-universal-time))
                                                    (cons "title" (format nil "~a ~a"
                                                                          feed-name
                                                                          (gethash :title story)))))))))
                  parsed))))
       *feed-list*))

(defun start-refresh-threads ()
  (bt:make-thread (lambda ()
                    (loop
                      (authenticate)
                      (sync-subreddits)
                      (scan-links)
                      (sleep 60)))
                  :name "link fetcher")
  (bt:make-thread (lambda ()
                    (loop
                      (scan-comments)
                      (sleep 60)))
                  :name "comment fetcher"))

(defun stop-refresh-threads ()
  (cl-ivy:stop-thread-by-name "link fetcher")
  (cl-ivy:stop-thread-by-name "comment fetcher")
  (cl-ivy:stop-thread-by-name "RSS fetcher"))

(defun mark-subreddit-as-read (subreddit)
  (let ((posts (couch-query (list (cons "subreddit" subreddit)
                                  (cons "suphidden" (alist-hash-table
                                                     (list (cons "$exists" 'yason:false))))
                                  (cons "type" "link"))
                            :limit 100000)))
    (map 'nil (lambda (doc-hash)
                (setf (gethash "suphidden" doc-hash) 't)
                (doc-put "reddit" (to-json doc-hash)))
         posts)
    (length posts)))

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
          (if-let ((html-body (hash-get comment '("body_html"))))
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

(defun display-link (doc-id)
  (let ((doc-hash (yason:parse (doc-get "reddit" doc-id))))
    (with-html
      (let ((unique-id (format nil "~a" (uuid:make-v4-uuid))))
        (:div.row :id (format nil "wx~a" unique-id)
                  (:div.col-md-6
                   (:div.panel.panel-default.panel-borders.panel-heading-fullwidth
                    :style "border:1px solid rgb(7,7,7);"
                    (:div.panel-heading
                     (:div.tools (:div.icon
                                  (:a :target (format nil "win-~a" (uuid:make-v4-uuid))
                                    :href (format nil "https://grid.ivy.io/_utils/#database/reddit/~a" doc-id)
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
                      ("~a/~a   " (gethash "ups" doc-hash)
                                  (gethash "downs" doc-hash)))
                     (when-let ((title (gethash "title" doc-hash)))
                       (if-let ((url (gethash "url" doc-hash)))
                         (:a :target (format nil "win-~a" (uuid:make-v4-uuid))
                           :href url (html-entities:decode-entities title))
                         title)
                       (:br)
                       (:span  (:a :target (format nil "win-~a" (uuid:make-v4-uuid))
                              :href (format nil "https://www.reddit.com~a" (gethash "permalink" doc-hash))
                              (format nil "~a in ~a"
                                      (gethash "author" doc-hash)
                                      (gethash "subreddit" doc-hash)))))
                     
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
                    (:script
                      (ps:ps*
                       `(with-document-ready (lambda ()
                                               (-> (sel ,(format nil "#link-~a" unique-id))
                                                   (load ,(format nil "/link/body/~a" (gethash "_id" doc-hash))))))))
                    (:div.panel-body
                     :id (format nil "link-~a" unique-id))))
                  (:div.col-md-6
                   (:div.panel.panel-default.panel-borders.panel-heading-fullwidth
                    :style "border:1px solid rgb(7,7,7);"
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
                                     :style "display:none;"))))))))

(defun display-links (link-ids)
  (with-page ()
    (:div :class "row"
      (:div :class "col-md-12"
        (:form :method :post :action "/search"
          (:div :class "col-md-2"
            (:div :class "btn-group btn-space"
              (:a :class "btn btn-default btn-xs" :type "button" :href "/favorites" "Favorites")
              (:a :class "btn btn-default btn-xs" :type "button" :href "/" "Index")
              (:input :name "pattern" :type "text" :class "input-xs form-control pull-left" :placeholder "Search..." :id "searchtext"))))
        (:div :class "col-md-10"
          (:div :class "btn-group btn-space"
            (map 'nil (lambda (subreddit)
                        (:a :class "btn btn-primary btn-xs"
                          :href (format nil "/subreddit/~a" (gethash "name" subreddit))
                          (gethash "display_name" subreddit)))
                 (sort (couch-query (list (cons "type" "subreddit")))
                       #'string<
                       :key (lambda (x)
                              (string-upcase (gethash "display_name" x)))))))))
    (map nil #'display-link link-ids)))

(defun scan-for-duplicates ()
  (map nil
       (lambda (subreddit)
         (let ((name (gethash "display_name" subreddit)))
           (log:info name))
         (let ((ids (couch-query (list (cons "type" "link")
                                       (cons "subreddit" (gethash "display_name" subreddit)))
                                 :fields (list "id")
                                 :limit 500000)))
           (map nil
                (lambda (ids)
                  (let ((dupes (couch-query (list (cons "type" "link")
                                                  (cons "id" (gethash "id" ids))))))
                    (when (< 1 (length dupes))
                      (map nil (lambda (bye)
                                 (doc-delete "reddit" (gethash "_id" bye) (gethash "_rev" bye)))
                           (cdr dupes))
                      (let ((good (gethash "id" (car dupes))))
                        (log:info "purged" good)))))
                ids)))
       (get-db-subreddits)))




(defroute ui/login ("/login" :method :get) ()
  (with-page (:body-class "am-splash-screen")
    (:div :class "am-wrapper am-login"
      (:div :class "am-content"
        (:div :class "main-content"
          (:div :class "login-container"
            (:div :class "panel panel-default"
              (:div :class "panel-body"
                (:form :action "/login" :method "post" :class "form-horizontal"
                  (:div :class "login-form"
                    (:div :class "form-group"
                      (:div :class "input-group"
                        (:span :class "input-group-addon"
                          (:i :class "icon s7-mail"))
                        (:input
                          :name "email"
                          :type "email"
                          :placeholder "Email"
                          :autocomplete "off"
                          :class "form-control")))
                    (:div :class "form-group"
                      (:div :class "input-group"
                        (:span :class "input-group-addon"
                          (:i :class "icon s7-lock"))
                        (:input
                          :name "password"
                          :type "password"
                          :placeholder "Password"
                          :class "form-control")))
                    (:div :class "form-group login-submit"
                      (:button :data-dismiss "modal" :type "submit"
                        :class "btn btn-primary btn-lg" "Log me in"))
                    (:div :class "form-group footer row"
                      (:div :class "col-xs-6"
                        (:a :href "/signup" "Sign Up"))
                      (:div :class "col-xs-6"
                        (:a :class "pull-right" :href "/forgot" "Forgot password?")))))))))))))

(defroute ui/logout ("/logout" :method :get) ()
  (hunchentoot:set-cookie "uid" :value nil)
  (hunchentoot:redirect "https://downvote.ivy.io/"))

(defroute display-single-link ("/link/:id") ()
  (with-page ()
    (display-link id)))

(defroute link-search ("/search" :method :post) (pattern)
  (display-links (find-links-by-pattern pattern)))

(defroute display-subreddit ("/subreddit/:subid") ()
  (display-links
   (hash-extract "id"
                 (cl-mango:query-view "reddit"
                                      "tests"
                                      "by-subreddit"
                                      :parameters (list (cons "key" (to-json subid))
                                                        (cons "limit" "500")
                                                        (cons "descending" (with-output-to-string (sink)
                                                                             (yason:encode 'yason:true sink))))))))

(defroute favorites ("/favorites") ()
  (with-session ()
    (display-links
     (hash-extract "id"
                   (cl-mango:query-view "reddit" "tests" "favorites"
                                        :parameters (list (cons "limit" "500")
                                                          (cons "descending" (with-output-to-string (sink)
                                                                               (yason:encode 'yason:true sink)))))))))

(defroute links ("/links") ()
  (with-session ()
    (display-links
     (hash-extract "id"
                   (cl-mango:query-view "reddit" "tests" "links"
                                        :parameters (list (cons "descending" (with-output-to-string (sink)
                                                                               (yason:encode 'yason:true sink)))))))))

(defroute index ("/") ()
  (with-session ()
    (hunchentoot:redirect "/links")))

(defroute hide-link ("/link/hide/:id") ()
  (with-session ()
    (when-let ((the-link (couch-query (list (cons "id" id)
                                            (cons "type" "link"))
                                      :limit 1)))
      (map nil (lambda (link)
                 (setf (gethash "suphidden" link) t)
                 (doc-put "reddit" (to-json link)))
           (remove-if-not (lambda (x)
                            (string= "link" (hash-get x (list "type"))))
                          the-link))))
  "ok")

(defroute favorite-link ("/link/favorite/:id") ()
  (with-session ()
    (when-let ((the-link (car (couch-query (list (cons "id" id)
                                                 (cons "type" "link"))))))
      (if-let ((is-hidden-already (gethash "suphidden" the-link nil)))
        ;; The link is being hidden from /favorites and not /links
        ;; So send it back into the viewable pile.
        (progn
          (remhash "favorite" the-link)
          (remhash "suphidden" the-link)
          (doc-put "reddit" (to-json the-link)))
        (progn
          (setf (gethash "favorite" the-link) 't)
          (setf (gethash "suphidden" the-link) 't)
          (doc-put "reddit" (to-json the-link)))))))

(defroute render-comment ("/r/comment/:id") ()
  (declare (optimize (speed 3) (debug 0) (safety 1)))
  (with-session ()
    (let ((comments (couch-query (list (cons "type" "comment")
                                       (cons "id" id)))))
      (with-html-string
        (display-comment (car comments))))))

(defroute show-comments ("/comments/:id") ()
  (with-session ()
    (let ((comments (couch-query
                     (list (cons "type" "comment")
                           (cons "link_id" (format nil "t3_~a" id)))
                     :sort (list (alist-hash-table
                                  (list (cons "ups" "desc")))))))
      (with-html-string (map 'nil #'display-comment comments)))))

(defroute show-link-body ("/link/body/:id") ()
  (with-session ()
    (let ((doc-hash (yason:parse (doc-get "reddit" id))))
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
                (url-is-imgur-image (when-let ((url (gethash "url" doc-hash)))
                                      (when (ppcre:scan "imgur.com" url)
                                        (ppcre:regex-replace "https?://i?.?imgur.com/" url ""))))
                (url-is-video (when-let ((url (gethash "url" doc-hash)))
                                (when (ppcre:scan ".mp4|.MP4" url)
                                  url)))
                (url-is-image (when-let ((url (gethash "url" doc-hash)))
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
                                      (:script :src "//s.imgur.com/min/embed.js"))))))))))

(defroute ui/login-post ("/login" :method :post) (email password)
  (if (not (< 3 (length email)))
      (redirect "https://downvote.ivy.io/login")
      (alexandria:if-let ((person (car (couch-query (list (cons "type" "person")
                                                          (cons "email" email)
                                                          (cons "password" (make-hash password)))))))
        (progn (hunchentoot:set-cookie "uid"
                                       :expires (local-time:timestamp-to-universal
                                                 (local-time:unix-to-timestamp
                                                  (+ (* 60 60 600)
                                                     (local-time:timestamp-to-unix
                                                      (local-time:now))))) 
                                       :value (gethash "uid" person))
               (hunchentoot:redirect "https://downvote.ivy.io/"))
        
        (with-login-page
            (:div :class "am-wrapper am-login"
              (:div :class "am-content"
                (:div :class "main-content"
                  (:div :class "login-container"
                    (:div :class "panel panel-default"
                      (:div :style "color:white;" :class "panel-body"
                        (:span "Incorrect username or password."))))))
              (:script
                (ps:ps
                  (with-document-ready
                      (lambda ()
                        (set-timeout
                         (lambda ()
                           (setf (@ window location) "/"))
                         4000))))))))))

(setf lparallel:*kernel* (lparallel:make-kernel 10))

(defun purge-link (link)
  (let ((comments (couch-query (list (cons "link_id" (format nil "t3_~a"
                                                             (gethash "id" link))))
                               :limit 20000)))
    (when (< 0 (length comments))
      (log:info "comments: ~a" (length comments))
      (lparallel:pmapcar (lambda (comment)
                           (handler-case 
                               (doc-delete "reddit"
                                           (gethash "_id" comment)
                                           (gethash "_rev" comment))
                             (cl-mango:unexpected-http-response (condition)
                               (log:info "delete gave an error. ~a ~a"
                                         (cl-mango::status-body condition)
                                         (cl-mango::status-code condition))
                               nil)))
                         comments))
    (handler-case
        (doc-delete "reddit"
                    (gethash "_id" link)
                    (gethash "_rev" link))
      (cl-mango:unexpected-http-response (condition)
        (log:info "delete gave an error. ~a ~a"
                  (cl-mango::status-body condition)
                  (cl-mango::status-code condition))
        nil))))

(defun purge-subreddit (subreddit-name)
  (let ((links (couch-query (list (cons "subreddit" subreddit-name))
                            :limit 1000)))
    (log:info "links: ~a" (length links))
    
    (when (= 0 (length links))
      (return-from purge-subreddit))

    (purge-links links)

    (purge-subreddit subreddit-name)))

(defun find-orphaned-links ()
  (let ((links (couch-query (list (cons "type" "link"))
                            :fields (list "_id" "_rev" "subreddit")
                            :limit 100000))
        (subreddit-names (mapcar (lambda (x)
                                   (gethash "display_name" x))
                                 (couch-query (list (cons "type" "subreddit"))
                                              :fields (list "display_name")))))
    (remove-if-not #'(lambda (link)
                       (member (gethash "subreddit" link)
                               subreddit-names
                               :test #'string=)) links)))

(defun remove-orphaned-links ()
  (map 'nil #'purge-link (find-orphaned-links)))

(defun mark-everything-read ()
  (mapcar (lambda (subreddit-hash)
            (mark-subreddit-as-read (gethash "display_name" subreddit-hash)))
          (get-db-subreddits)))
