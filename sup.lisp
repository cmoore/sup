;;; -*- mode: Lisp; Syntax: COMMON-LISP; Package: CL-MANGO; Base: 10 eval: (hs-hide-all) -*-

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
  (setf cl-mango:*password* "3113nsburg")

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

(defmethod cl-ivy:make-hash ((object string))
  (cl-ivy:make-hash object))

(defmethod cl-ivy:make-hash ((object hash-table))
  (cl-ivy:make-hash (to-json object)))

(defmethod cl-ivy:make-hash ((object list))
  (cl-ivy:make-hash (to-json object)))

(defvar *comments-box* (safe-queue:make-mailbox))

(defparameter *delete-box* (safe-queue:make-mailbox))

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
          (:body :style "background-color:rgb(51,51,51);"
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
  (declare (type string permalink)
           (optimize (debug 0) (speed 3) (safety 1)))
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
           (when comment-in-db
             (setf (gethash "_id" new-comment) (gethash "_id" comment-in-db))
             (setf (gethash "_rev" new-comment) (gethash "_rev" comment-in-db)))
           (setf (gethash "type" new-comment) "comment")
           ;;(log:info "writing comment")
           (handler-case (progn
                           (doc-put "reddit" (to-json new-comment)))
             (cl-mango:unexpected-http-response (condition)
               (let ((status (cl-mango::status-body condition)))
                 (log:info status))))))))))

(defun comments-process-mailbox ()
  (comments-handle-link
   (safe-queue:mailbox-receive-message *comments-box*)))

(defun start-link-processor-thread ()
  (bt:make-thread (lambda ()
                    (loop (comments-process-mailbox)))
                  :name "comment worker"))

(defun send-permalink (permalink)
  (safe-queue:mailbox-send-message *comments-box* permalink))

(defun load-comments ()
  (let ((links (append (couch-query (list (cons "type" "link")
                                          (cons "suphidden" (alist-hash-table
                                                             (list (cons "$exists" 'yason:false))))))
                       (couch-query (list (cons "type" "link")
                                          (cons "favorite" (alist-hash-table
                                                            (list (cons "$exists" 'yason:true)))))))))
    (dolist (link links)
      (send-permalink (gethash "permalink" link)))))

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
                                           (cons "favorite" 'yason:true)))))
  (sleep 30))

(defun link-is-image-p (link)
  (cond ((ppcre:scan ".jpg$|.png$|.gif$" link) link)
        ;;((ppcre:scan "imgur.com" link) (format nil "~a.gif" link))
        (t nil)))

(defun get-latest-post-id-for-subreddit (subreddit)
  (handler-case (let ((dochash (car (couch-query (list (cons "subreddit" subreddit))
                                                 :limit 1
                                                 :sort (list (alist-hash-table
                                                              (list (cons "created" "desc"))))))))
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

(defun existing-link-info (new-link)
  (let ((link-info (car (couch-query (list (cons "id" new-link))
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
                         (get-subreddit-links subreddit :latest-id latest-id)))))))

(defun handle-possible-new-link (new-link)
  (multiple-value-bind (id revision hidden)
      (existing-link-info (gethash "id" new-link))
    (unless id
      (send-permalink (gethash "permalink" new-link))
      ;;(log:info "new link")
      )
    (unless hidden
      (setf (gethash "type" new-link) "link")
      (setf (gethash "written" new-link) (get-universal-time))
      (when revision
        (setf (gethash "_id" new-link) id)
        (setf (gethash "_rev" new-link) revision))
      (handler-case (progn
                      ;;(send-permalink (gethash "permalink" new-link))
                      (cl-mango:doc-put "reddit" (to-json new-link)))
        (cl-mango:unexpected-http-response (condition)
          (declare (ignore condition))
          nil)))))

(defun scan-links ()
  (log:info "LINKS")
  (map nil (lambda (subreddit)
             (unless (ppcre:scan "u_" (gethash "display_name" subreddit))
               (let ((latest-id (get-latest-post-id-for-subreddit
                                 (gethash "display_name" subreddit))))
                 ;; (let ((name (gethash "display_name" subreddit)))
                 ;;   (log:info name))
                 (map nil #'handle-possible-new-link
                      (hash-extract "data"
                                    (get-subreddit-links subreddit :latest-id latest-id))))))
       (couch-query (list (cons "type" "subreddit"))))
  (sleep 30))

(defun start-scrubber-thread ()
  (bt:make-thread (lambda ()
                    (loop
                      (let ((link-to-delete (safe-queue:mailbox-receive-message *delete-box*)))
                        ;; Don't delete the link since its existence is how we know we've
                        ;; seen it before.
                        ;; (handler-case
                        ;;     (doc-delete "reddit" (gethash "_id" x) (gethash "_rev" x))
                        ;;   (cl-mango:unexpected-http-response (condition)
                        ;;     (declare (ignore condition))
                        ;;     nil))
                        (map nil (lambda (comment-to-delete)
                                   (handler-case
                                       (doc-delete "reddit"
                                                   (gethash "_id" comment-to-delete)
                                                   (gethash "_rev" comment-to-delete))
                                     (cl-mango:unexpected-http-response (condition)
                                       (declare (ignore condition))
                                       nil)))
                             (couch-query (list
                                           (cons "link_id"
                                                 (format nil "t3_~a" (gethash "id" link-to-delete)))))))))
                  :name "Scrubber Lang"))

(defun load-scrubber ()
  (map nil
       (lambda (killit)
         (safe-queue:mailbox-send-message *delete-box* killit))
       (let ((latest-timestamp (hash-get
                                (car (couch-query (list (cons "created" (alist-hash-table
                                                                         (list (cons "$exists" 'yason:true))))
                                                        (cons "link_id" (alist-hash-table
                                                                         (list (cons "$exists" 'yason:true)))))
                                                  :fields (list "created")
                                                  :limit 1))
                                (list "created"))))
         (couch-query (list (cons "written" (alist-hash-table
                                             (list (cons "$lt" (format nil "~a"
                                                                       (- latest-timestamp
                                                                          (* 60 60 24))))))))
                      :limit 10000
                      :fields (list "id")))))

(defun start-refresh-threads ()
  ;;(start-scrubber-thread)

  (start-link-processor-thread)

  (bt:make-thread (lambda ()
                    (loop
                      (authenticate)
                      (sync-subreddits)
                      (scan-links)))
                  :name "sup post fetcher")
  (bt:make-thread (lambda ()
                    (loop
                      (log:info "FLASH!  AAAAAAA-aaaaaaaaaaaa")
                      (load-comments)
                      (sleep (* 15 60))))
                  :name "comment churner"))

(defun refresh-data ()
  (authenticate)
  (sync-subreddits)
  (scan-links)
  (scan-comments)
  (sleep 30))

(defun stop-refresh-threads ()
  (cl-ivy:stop-threads-by-name "comment worker")
  (cl-ivy:stop-threads-by-name "comment churner")
  (cl-ivy:stop-threads-by-name "Scrubber Lang")
  (cl-ivy:stop-thread-by-name "sup post fetcher"))

(defun mark-all-as-read ()
  (let ((num (length
              (mapc (lambda (x)
                      (setf (gethash "suphidden" x) 't)
                      (cl-mango:doc-put "reddit" (to-json x)))
                    (couch-query (list (cons "type" "link")
                                       (cons "suphidden" (alist-hash-table
                                                          (list (cons "$exists" 'yason:false)))))
                                 :sort (list (alist-hash-table
                                              (list (cons "created" "desc"))))
                                 :limit 1000)))))
    (when (< 0 num)
      (mark-all-as-read))))

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
          (dolist (reply (or replies '()))
            (:div :style "border-left:1px solid #eee;padding-left:10px;"
              (display-comment (gethash "data" reply)))))))))

(defun show-embedded-html (html)
  (declare (type string html))
  (with-html
    (:raw
     (html-entities:decode-entities html))))

(defroute show-comments ("/comments/:id") ()
  (let ((comments (couch-query
                     (list (cons "type" "comment")
                           (cons "link_id" (format nil "t3_~a" id)))
                     :sort (list (alist-hash-table
                                  (list (cons "ups" "desc")))))))
    (with-html-string (map 'nil #'display-comment comments))))

(defun display-link (doc-hash)
  (with-html
    (let ((unique-id (format nil "~a" (uuid:make-v4-uuid))))
      (:div.row :id (format nil "wx~a" unique-id)
                (:div.col-md-6
                 (:div.panel.panel-default.panel-borders.panel-heading-fullwidth
                  :style "border:1px solid rgb(7,7,7);"
                  (:div.panel-heading
                   (:div.tools
                    (:div.icon
                     (:a :href (format nil "/link/~a" (hash-get doc-hash '("id")))
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
                         :href url title)
                       title)
                     (:br)
                     (:a :target (format nil "win-~a" (uuid:make-v4-uuid))
                       :href (format nil "https://www.reddit.com~a" (gethash "permalink" doc-hash))
                       (gethash "subreddit" doc-hash)))
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
                                                 (load ,(format nil "/link/hide/~a" (gethash "id" doc-hash))))
                                          (-> (sel ,(format nil "#wx~a" unique-id))
                                              (toggle))
                                          (-> e (prevent-default))))))))))
                  (:div.panel-body
                   (:div.row
                    (:div.col-md-12

                     (let ((crossposted-reddit-video (hash-get doc-hash '("crosspost_parent_list" 0 "secure_media" "reddit_video" "fallback_url")))
                           (reddit-preview-of-imgur-gif (hash-get doc-hash '("preview" "reddit_video_preview" "fallback_url")))
                           (is-reddit-video (hash-get doc-hash '("secure_media" "reddit_video" "fallback_url")))
                           (is-embedded-image (hash-get doc-hash '("preview" "images")))
                           (has-selftext (hash-get doc-hash '("selftext_html")))
                           (has-oembed-media (hash-get doc-hash '("secure_media" "oembed" "html")))
                           (has-crosspost-parent-media (let ((crosspost-parent-list (hash-get doc-hash '("crosspost_parent_list"))))
                                                         (when (listp crosspost-parent-list)
                                                           (hash-get (car crosspost-parent-list)
                                                                     '("preview" "reddit_video_preview" "fallback_url"))))))
                       (cond (crossposted-reddit-video (:video :class "img-responsive" :controls 1
                                                         (:source :src crossposted-reddit-video)))
                             (reddit-preview-of-imgur-gif (:video :class "img-responsive" :controls 1
                                                                                          (:source :src reddit-preview-of-imgur-gif)))
                             (has-oembed-media (:raw (html-entities:decode-entities
                                                         has-oembed-media)))
                             (is-reddit-video (:video :class "img-responsive" :controls 1
                                                (:source :src is-reddit-video)))
                             ;; Matches if the post is a crosspost and the original
                             ;; has a video hosted at reddit
                             (has-crosspost-parent-media (progn
                                                           (log:info "Yes!")
                                                           (:video :class "img-responsive" :controls 1
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
                                                           (cond (has-mp4 (:video :class "img-responsive"
                                                                            :controls 1
                                                                            (:source :src (html-entities:decode-entities has-mp4))))
                                                                 (has-gif (:img :class "img-responsive"
                                                                            :src (html-entities:decode-entities has-gif)))
                                                                 (has-still-image (:img :class "img-responsive"
                                                                                    :src (html-entities:decode-entities has-still-image)))))))))
                             (has-selftext (:raw (html-entities:decode-entities has-selftext)))))))
                   (:div.row
                    (:div.col-md-12
                     (:a :target (format nil "win-~a" (uuid:make-v4-uuid))
                       :href (format nil "https://grid.ivy.io/_utils/#database/reddit/~a"
                                     (gethash "_id" doc-hash))
                       "db"))))))
                (:div.col-md-6
                 (:div.panel.panel-default.panel-borders
                  :style "border:1px solid rgb(7,7,7);"
                  (:div.panel-body :id (format nil "comments-~a" (gethash "id" doc-hash)))
                  (:script
                    (ps:ps* `(with-document-ready (lambda ()
                                                    (-> (sel ,(format nil "#comments-~a" (gethash "id" doc-hash)))
                                                        (load ,(format nil "/comments/~a" (gethash "id" doc-hash))))))))))))))

(defroute hide-link ("/link/hide/:id") ()
  (when-let ((the-link (couch-query (list (cons "id" id)
                                     (cons "type" "link"))
                               :limit 1)))
    (map nil (lambda (link)
               (setf (gethash "suphidden" link) t)
               (doc-put "reddit" (to-json link)))
         (remove-if-not (lambda (x)
                          (string= "link" (hash-get x (list "type"))))
                        the-link)))
  "ok")

(defroute favorite-link ("/link/favorite/:id") ()
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
        (log:info "new favorite")
        (setf (gethash "favorite" the-link) 't)
        (setf (gethash "suphidden" the-link) 't)
        (doc-put "reddit" (to-json the-link))))))

(defroute render-comment ("/r/comment/:id") ()
  (declare (optimize (speed 3) (debug 0) (safety 1)))
  (let ((comments (couch-query (list (cons "type" "comment")
                                     (cons "id" id)))))
    (with-html-string
      (display-comment (car comments)))))

(defroute favorites ("/favorites") ()
  (with-page ()
    (mapcar #'display-link
            (couch-query (list (cons "type" "link")
                               (cons "favorite" 't))
                         :sort (list (alist-hash-table
                                      (list (cons "created" "desc"))))))))

(defroute links ("/links") ()
  (with-page ()
    (:div :class "row"
      (:div :class "col-md-6"
        (:a :class "btn btn-default btn-xs"
          :style "position:fixed;top:0;left:0;"
          :href "/favorites" "Favorites")))
    (map nil #'display-link
         (couch-query (list (cons "type" "link")
                            (cons "suphidden" (alist-hash-table
                                               (list (cons "$exists" 'yason:false)))))
                      :sort (list (alist-hash-table
                                   (list (cons "created" "asc"))))
                      :limit 500))))

(defroute index ("/") ()
  (hunchentoot:redirect "/links"))

(defun scan-for-duplicates ()
  (map nil
       (lambda (subreddit)
         (let ((name (gethash "display_name" subreddit)))
           (log:info name))
         (let ((ids (couch-query (list (cons "type" "link")
                                       (cons "subreddit" (gethash "display_name" subreddit)))
                                 :fields (list "id")
                                 :limit 10000)))
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

(defroute display-single-link ("/link/:id") ()
  (let ((link (car (couch-query (list (cons "type" "link")
                                      (cons "id" id))))))
    (with-page ()
      (display-link link))))
