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
  (setf cl-mango:*password* "h4r01d")

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

(defvar *comments-box* (sb-concurrency:make-mailbox))

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
                                    :address "127.0.0.1"
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

(defun delete-links ()
  (let ((num-links (length
                    (mapcar (lambda (link)
                              (doc-delete "reddit" (gethash "_id" link) (gethash "_rev" link)))
                            (couch-query (list (cons "type" "link"))
                                         :fields (list "_id" "_rev")
                                         :limit 1000)))))
    (unless (equal 0 num-links)
      (log:info "link step")
      (delete-links))))

(defun delete-comments ()
  (let ((num-comments (length
                       (mapcar (lambda (comment)
                                 (handler-case
                                     (doc-delete "reddit" (gethash "_id" comment) (gethash "_rev" comment))
                                   (cl-mango:unexpected-http-response (condition)
                                     (declare (ignore condition))
                                     nil)))
                               (couch-query (list (cons "type" "comment"))
                                            :fields (list "_id" "_rev")
                                            :limit 1000)))))
    (unless (equal 0 num-comments)
      (log:info "comment step")
      (delete-comments))))

(defun delete-all-links-and-comments ()
  (bt:make-thread (lambda ()
                    (delete-links)
                    (log:info "Links deleted."))
                  :name "link vaccum")
  (bt:make-thread (lambda ()
                    (delete-comments)
                    (log:info "Comments deleted"))
                  :name "comments vaccum"))

(defun comments-handle-link (permalink)
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
     (sleep 2)
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
  (let ((current-permalink (sb-concurrency:receive-message *comments-box*)))
    (log:info "linky" current-permalink)
    (comments-handle-link current-permalink)))

(defun start-link-processor-thread ()
  (bt:make-thread (lambda ()
                    (loop (comments-process-mailbox)))
                  :name "comment worker"))

(defun send-permalink (permalink)
  (sb-concurrency:send-message *comments-box* permalink))

(defun load-comments ()
  (map nil #'send-permalink
       (append (couch-query (list (cons "type" "link")
                                   (cons "suphidden" (alist-hash-table
                                                      (list (cons "$exists" 'yason:false))))))
               (couch-query (list (cons "type" "link")
                                  (cons "favorite" (alist-hash-table
                                                    (list (cons "$exists" 'yason:true)))))))))

(defun scan-comments ()
  (log:info "comments")
  ;; (map 'nil #'comments-handle-link
  ;;      (couch-query (list (cons "type" "link")
  ;;                         (cons "suphidden" (alist-hash-table
  ;;                                            (list (cons "$exists" 'yason:false)))))
  ;;                   :fields (list "permalink")
  ;;                   :limit 10000))
  (mapcar #'comments-handle-link
          (couch-query (list (cons "type" "link")
                             (cons "favorite" 'yason:true))))
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
      (log:info "new link"))
    (unless hidden
      (setf (gethash "type" new-link) "link")
      (setf (gethash "written" new-link) (get-universal-time))
      (when revision
        (setf (gethash "_id" new-link) id)
        (setf (gethash "_rev" new-link) revision))
      (handler-case (progn
                      (send-permalink (gethash "permalink" new-link))
                      (cl-mango:doc-put "reddit" (to-json new-link)))
        (cl-mango:unexpected-http-response (condition)
          (declare (ignore condition))
          nil)))))

(defun scan-links ()
  (log:info "LINKS")
  (map nil (lambda (subreddit)
             (let ((latest-id (get-latest-post-id-for-subreddit
                               (gethash "display_name" subreddit))))
               (map nil #'handle-possible-new-link
                    (hash-extract "data"
                                  (get-subreddit-links subreddit :latest-id latest-id))))
             (sleep 2))
       (couch-query (list (cons "type" "subreddit")))))

(defun start-refresh-threads ()
  ;; (bt:make-thread (lambda ()
  ;;                   (loop
  ;;                     (scan-comments)))
  ;;                 :name "sup comment fetcher")
  (bt:make-thread (lambda ()
                    (loop
                      (authenticate)
                      (sync-subreddits)
                      (scan-links)))
                  :name "sup post fetcher"))

(defun stop-refresh-threads ()
  (cl-ivy:stop-thread-by-name "sup post fetcher")
  (cl-ivy:stop-thread-by-name "sup comment fetcher"))

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

(defun display-link (doc-hash &optional original-id)
  (declare (optimize (debug 3)))
  (when-let ((crosspost-list (gethash "crosspost_parent_list" doc-hash)))
    (when (listp crosspost-list)
      (display-link (car crosspost-list) (gethash "id" doc-hash))
      (return-from display-link)))
  (with-html
    (let ((comments (couch-query (list (cons "type" "comment")
                                       (cons "link_id" (format nil "t3_~a" (or original-id
                                                                               (gethash "id" doc-hash)))))
                                 :sort (list (alist-hash-table
                                              (list (cons "ups" "desc"))))
                                 :fields (list "id")))
          (unique-id (format nil "~a" (uuid:make-v4-uuid)))
          (showed-video nil)
          (showed-image nil))

      (:div.row :id (format nil "wx~a" unique-id)
                (:div.col-md-6
                 (:div.panel.panel-default.panel-borders.panel-heading-fullwidth
                  :style "border:1px solid rgb(7,7,7);"
                  (:div.panel-heading
                   (:div.tools
                    (:div.icon :id (format nil "favorite-~a" unique-id)
                               (:span.s7-download))
                    (:div.icon :id (format nil "hide-~a" unique-id)
                               (:span.s7-close-circle)))
                   (:div.pull-right
                    ("   ~a/~a" (gethash "ups" doc-hash)
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
                                                 (load ,(format nil "/link/favorite/~a" (or original-id
                                                                                            (gethash "id" doc-hash)))))
                                          (-> (sel ,(format nil "#wx~a" unique-id))
                                              (toggle))
                                          (-> e (prevent-default)))))
                             (-> (sel ,(format nil "#hide-~a" unique-id))
                                 (click (lambda (e)
                                          (chain (sel ,(format nil "#wx~a" unique-id))
                                                 (load ,(format nil "/link/hide/~a" (or original-id
                                                                                        (gethash "id" doc-hash)))))
                                          (-> (sel ,(format nil "#wx~a" unique-id))
                                              (toggle))
                                          (-> e (prevent-default))))))))))
                  (:div.panel-body
                   (:div.row
                    (:div.col-md-12
                     (hash-get doc-hash '("selftext"))

                     (when-let ((secure-media-hash (hash-get doc-hash '("secure_media"))))
                       (when-let ((type (and (hash-table-p secure-media-hash)
                                             (gethash "type" secure-media-hash))))
                         (cond ((or (string= type "youtube.com")
                                    (string= type "m.youtube.com")
                                    (string= type "streamable.com")
                                    (string= type "gfycat.com"))
                                (let ((embed (gethash "oembed" secure-media-hash)))
                                  (setf showed-video t)
                                  (:raw
                                   (html-entities:decode-entities
                                    (gethash "html" embed))))))))
                     
                     ;; Reddit's video walled garden.
                     (when-let ((fallback-video-url (hash-get doc-hash
                                                              '("secure_media"
                                                                "reddit_video"
                                                                "fallback_url"))))
                       (setf showed-video t)
                       (:video :class "img-responsive" :controls 1
                         (:source :src fallback-video-url)))

                     (unless showed-video
                       (when-let ((images (hash-get doc-hash '("preview" "images"))))
                         (map nil
                              (lambda (image-hash)
                                (when-let ((mp4-link (hash-get image-hash '("variants" "mp4"))))
                                  (setf showed-video t)
                                  (:video :class "img-responsive" :controls 1
                                    (:source :src mp4-link))
                                  ("Movie link: ~a" mp4-link))
                                (when-let ((gif-link (hash-get image-hash '("variants" "gif"))))
                                  (setf showed-image t)
                                  (:img.img-responsive :src gif-link))
                                (when-let ((best-image (car (reverse (gethash "resolutions" image-hash)))))
                                  (setf showed-image t)
                                  (:img.img-responsive :src (ppcre:regex-replace-all "&amp;"
                                                                                     (gethash "url" best-image)
                                                                                     "&"))))
                              images))
                       
                       (if-let ((fuck (get-imgur-id (hash-get doc-hash '("url")))))
                         (progn
                           (:blockquote :class "imgur-embed-pub" :data-id fuck)
                           (:script :async 1 :src "//s.imgur.com/min/embed.js"))
                         (unless showed-image
                           (when-let ((image-url (link-is-image-p (gethash "url" doc-hash))))
                             (:img.img-responsive :src image-url)))))))
                   (:div.row
                    (:div.col-md-12
                     (:a :target (format nil "win-~a" (uuid:make-v4-uuid))
                       :href (format nil "http://localhost:5984/_utils/#database/reddit/~a"
                                     (gethash "_id" doc-hash))
                       "db"))))))
                (:div.col-md-6
                 (when (< 0 (length comments))
                   (:div.panel.panel-default.panel-borders :style "border:1px solid rgb(7,7,7);"
                                                           (:div.panel-body
                                                            (dolist (comment comments)
                                                              (:div :id (format nil "comment-~a"
                                                                                (gethash "id" comment)))
                                                              (%ps-load-comments comment (gethash "id" comment)))))))))))

(defroute hide-link ("/link/hide/:id") ()
  (let ((the-link (car (couch-query (list (cons "id" id)
                                          (cons "type" "link"))
                                    :limit 1))))
    (when the-link
      (setf (gethash "suphidden" the-link) t)
      (doc-put "reddit" (to-json the-link))))
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
          (with-output-to-string (sink)
            (ignore-errors (cl-markdown:markdown body :stream *html*))))
        (let ((replies (ignore-errors
                        (gethash "children"
                                 (gethash "data"
                                          (gethash "replies" comment))))))
          (dolist (reply (or replies '()))
            (:div :style "border-left:1px solid #eee;padding-left:10px;"
              (display-comment (gethash "data" reply)))))))))

(defroute render-comment ("/r/comment/:id") ()
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
  (declare (optimize (debug 0) (speed 3) (safety 1)))
  (with-page ()
    (:div :class "row"
      (:div :class "col-md-6"
        (:a :class "btn btn-default btn-xs"
          :style "position:fixed;top:0;left:0;"
          :href "/favorites" "Favorites")))
    (mapcar #'display-link
            (couch-query (list (cons "type" "link")
                               (cons "suphidden" (alist-hash-table
                                                  (list (cons "$exists" 'yason:false)))))
                         :sort (list (alist-hash-table
                                      (list (cons "created" "asc"))))
                         :limit 100))))

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
