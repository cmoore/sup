;;; -*- mode: Lisp; Syntax: COMMON-LISP; Package: CL-MANGO; Base: 10 eval: (hs-hide-all) -*-

(defpackage :sup
  (:use #:cl
        #:json-mop
        #:easy-routes
        #:parenscript
        #:cl-who
        #:cl-mango))

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


(defmethod cl-ivy:make-hash ((object hash-table))
  (cl-ivy:make-hash (to-json object)))

(defmethod cl-ivy:make-hash ((object list))
  (cl-ivy:make-hash (to-json object)))


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
                        (css nil)
                        (body-class nil)
                        (enable-page-header nil)
                        (js nil)) &body body)
  `(let ((who:*html-no-indent-tags* (list :pre :textarea :input :form)))
     (cl-who:with-html-output-to-string
         (*standard-output* nil :indent nil :prologue t)
       (who:htm
        (:html :lang "en"
          (:head
            (:meta :charset "utf-8")
            ,(if title
                 `(:title ,title)
                 `(:title "Nugget"))
            (:meta :name "viewport" :content "width=device-width, initial-scale=1.0, maximum-scale=1.0, user-scalable=no")
            (:link :rel "stylesheet" :type "text/css" :href "https://r.ivy.io/am/lib/stroke-7/style.css")
            (:link :rel "stylesheet" :type "text/css" :href "https://r.ivy.io/am/lib/jquery.nanoscroller/css/nanoscroller.css")
            (:style :type "text/css"
              (who:str (css-lite:css ((".am-wrapper") (:padding-top "0px !important")))))
            ,@(when css
                `,css)
            (:link :rel "stylesheet" :type "text/css" :href "https://r.ivy.io/am/css/style.css")
            (:script :src "https://r.ivy.io/am/lib/jquery/jquery.min.js"))
          (:body :style "background-color:black;" ,@(when body-class `(:class ,body-class))
            (:div :class "am-wrapper am-nosidebar-left"
              (:div :class "am-content"
                ,(when enable-page-header
                   '(:div :class "page-head"
                     (:h2 "Calendar")
                     (:ol :class "breadcrumb"
                       (:li (:a :href "#" "Home"))
                       (:li (:a :href "#" "Pages")))))
                (:div :class "main-content"
                  ,@body)))
            (:script :src "https://r.ivy.io/am/lib/jquery.nanoscroller/javascripts/jquery.nanoscroller.min.js")
            (:script :src "https://r.ivy.io/am/js/main.min.js")
            (:script :src "https://r.ivy.io/am/lib/bootstrap/dist/js/bootstrap.min.js")
            (:script :src "https://r.ivy.io/am/lib/jquery-ui/jquery-ui.min.js")
            (:script :src "https://r.ivy.io/gfycat.min.js")
            ,@(when js
                `,js)))))))

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
  (alexandria:when-let* ((preview (gethash "preview" doc-hash))
                         (images (gethash "images" preview)))
    images))

(defun test ()
  (gethash "docs"
              (yason:parse
               (doc-find "reddit"
                         (make-selector
                          (list (cons "link_id" "t3_a0q1lq")))))))

(defun delete-comments ()
  (let ((num-comments (length
                       (mapcar (lambda (comment)
                                 (doc-delete "reddit" (gethash "_id" comment) (gethash "_rev" comment)))
                               (gethash "docs"
                                        (yason:parse
                                         (doc-find "reddit" (make-selector (list (cons "type" "comment"))
                                                                           :fields (list "_id" "_rev")
                                                                           :limit 1000))))))))
    (unless (equal 0 num-comments)
      (log:info "step")
      (delete-comments))))

(defun comments-handle-link (link)
  ;; FIX THIS SHITSHOW
  (ignore-errors
   (multiple-value-bind (data status)
       (drakma:http-request (format nil
                                    "https://www.reddit.com~acomments.json?limit=100"
                                    (gethash "permalink" link))
                            :external-format-in :utf8
                            :external-format-out :utf8)
     (unless (equal status 200)
       (log:info "caught error. passing on this one.")
       (return-from comments-handle-link))
     (mapcar (lambda (listing)
               (mapcar (lambda (comment)
                         (let* ((new-comment (gethash "data" comment)))
                           (let ((comment-in-db (car
                                                 (gethash
                                                  "docs"
                                                  (yason:parse
                                                   (doc-find "reddit"
                                                             (make-selector
                                                              (list (cons "id" (gethash "id" new-comment)))
                                                              :limit 1
                                                              :fields (list "_id" "_rev" "ups" "downs"))))))))
                             (if comment-in-db
                                 (let ((db-ups (gethash "ups" comment-in-db))
                                       (db-downs (gethash "downs" comment-in-db))
                                       (comment-ups (gethash "ups" new-comment))
                                       (comment-downs (gethash "downs" new-comment)))
                                   ;;(log:info db-ups db-downs comment-ups comment-downs)
                                   (when (and (not (equal db-ups comment-ups))
                                              (not (equal db-downs comment-downs)))
                                     (setf (gethash "_id" new-comment) (gethash "_id" comment-in-db))
                                     (setf (gethash "_rev" new-comment) (gethash "_rev" comment-in-db))
                                     (log:info "updating existing comment")))
                                 (progn
                                   (setf (gethash "type" new-comment) "comment")
                                   (handler-case (doc-put "reddit" (to-json new-comment))
                                     (cl-mango:unexpected-http-response (condition)
                                       (let ((status (cl-mango::status-body condition)))
                                         (log:info status)))))))))
                       (gethash "children" (gethash "data" listing))))
             (yason:parse data))))
  (sleep 1))

(defun scan-comments ()
  (map 'nil #'comments-handle-link
       (gethash "docs"
                (yason:parse
                 (doc-find "reddit"
                           (make-selector
                            (list (cons "type" "link")
                                  (cons "suphidden" (alexandria:alist-hash-table
                                                     (list (cons "$exists" 'yason:false)))))
                            :fields (list "permalink"))))))
  (sleep 10))

(defun get-latest-post-id-for-subreddit (subreddit)
  (let ((dochash (car (gethash "docs"
                               (yason:parse
                                (doc-find "reddit"
                                          (make-selector (list (cons "subreddit" subreddit))
                                                         :limit 1
                                                         :sort (list (alexandria:alist-hash-table
                                                                      (list (cons "created" "desc")))))))))))
    (when dochash
      (gethash "id" dochash))))

(defun has-subreddit-p (subreddit-name)
  (< 0 (length
        (gethash "docs"
                 (yason:parse
                  (doc-find "reddit" (make-selector
                                      (list (cons "display_name" subreddit-name)
                                            (cons "type" "subreddit")))))))))

(defun get-db-subreddits ()
  (gethash "docs"
           (yason:parse
            (cl-mango:doc-find "reddit" (make-selector
                                         (list (cons "type" "subreddit"))
                                         :limit 1000
                                         :fields (list "_id" "_rev" "display_name"))))))

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
                   (log:info "missing" reddit-hash))))
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

(defun has-link-id-p (link-id)
  (< 0 (length (gethash "docs"
                        (yason:parse
                         (doc-find "reddit"
                                   (make-selector
                                    (list (cons "type" "link")
                                          (cons "id" link-id))
                                    :fields (list "_id"))))))))

(defun scan-links ()
  (map nil (lambda (subreddit)
             (let ((latest-id (get-latest-post-id-for-subreddit
                               (gethash "display_name" subreddit))))
               (let ((links (gethash "children"
                                     (gethash "data"
                                              (yason:parse
                                               (drakma:http-request
                                                (let ((url (gethash "url" subreddit)))
                                                  (if latest-id
                                                      (format nil "https://www.reddit.com~a.json?limit=100&after=~a"
                                                              url
                                                              latest-id)
                                                      (format nil "https://www.reddit.com~a.json?limit=100"
                                                              url)))))))))
                 (mapc (lambda (tmp)
                         (let ((new-link (gethash "data" tmp)))
                           (unless (has-link-id-p (gethash "id" new-link))
                             (setf (gethash "type" new-link) "link")
                             (setf (gethash "written" new-link) (get-universal-time))
                             (handler-case (progn
                                             (cl-mango:doc-put "reddit" (to-json new-link)))
                               (cl-mango:unexpected-http-response (condition)
                                 (log:info (cl-mango::status-body condition))
                                 nil)))))
                       links))))
       (gethash "docs"
                (yason:parse
                 (doc-find "reddit" (make-selector
                                     (list (cons "type" "subreddit"))))))))

(defun start-refresh-threads ()
  (bt:make-thread (lambda ()
                    (loop
                      (scan-comments)))
                  :name "sup comment fetcher")
  (bt:make-thread (lambda ()
                    (loop
                      (authenticate)
                      (sync-subreddits)
                      (scan-links)
                      (sleep 30)
                      nil))
                  :name "sup post fetcher"))

(defun mark-all-as-read ()
  (let ((num (length
              (mapc (lambda (x)
                      (setf (gethash "suphidden" x) 't)
                      (cl-mango:doc-put "reddit" (to-json x)))
                    (gethash "docs"
                             (yason:parse
                              (doc-find "reddit" (make-selector (list (cons "type" "link")
                                                                      (cons "suphidden" (alexandria:alist-hash-table
                                                                                         (list (cons "$exists" 'yason:false)))))
                                                                :sort (list (alexandria:alist-hash-table
                                                                             (list (cons "created" "desc"))))
                                                                :limit 1000))))))))
    (when (< 0 num)
      (mark-all-as-read))))


(defun mark-subreddit-as-read (subreddit)
  (let ((posts (gethash "docs"
                        (yason:parse
                         (doc-find "reddit" (make-selector
                                             (list (cons "subreddit" subreddit)
                                                   (cons "suphidden" (alexandria:alist-hash-table
                                                                      (list (cons "$exists" 'yason:false))))
                                                   (cons "type" "link"))
                                             :limit 10000))))))
    (map 'nil (lambda (doc-hash)
                (setf (gethash "suphidden" doc-hash) 't)
                (doc-put "reddit" (to-json doc-hash)))
         posts)
    (length posts)))

(defun get-all-of-type (doc-type)
  (gethash "docs"
           (yason:parse
            (doc-find "reddit" (make-selector (list (cons "type" doc-type)))))))

(defroute hide-link ("/link/hide/:id") ()
  (let ((the-link (car (gethash "docs"
                                (yason:parse (doc-find "reddit"
                                                       (make-selector
                                                        (list (cons "id" id)
                                                              (cons "type" "link"))
                                                        :limit 1)))))))
    (when the-link
      (setf (gethash "suphidden" the-link) t)
      (doc-put "reddit" (to-json the-link))))
  "ok")

(defroute links ("/links") ()
  (with-page (:js ((:script
                     (who:str
                      (ps:ps (-> document
                                 (add-event-listener "DOMContentLoaded"
                                                     (@ gfy-collection init) nil)))))))
    (mapcar (lambda (doc-hash)
              (let ((comments (gethash "docs"
                                       (yason:parse
                                        (doc-find "reddit"
                                                  (make-selector
                                                   (list (cons "type" "comment")
                                                         (cons "link_id" (format nil
                                                                                 "t3_~a"
                                                                                 (gethash "id" doc-hash))))
                                                   :sort (list
                                                          (alexandria:alist-hash-table
                                                           (list (cons "ups" "desc"))))))))))
                (who:htm
                 (:div :class "row" :id (format nil "wx~a" (gethash "id" doc-hash))
                   (:div :class "col-md-6"
                     (:div :class "panel panel-default panel-borders"
                       (:div :class "panel-heading"
                         (:button :id (format nil "hide-~a" (gethash "id" doc-hash))
                           :class "btn btn-xs btn-default pull-right" "Close")
                         (alexandria:when-let ((title (gethash "title" doc-hash)))
                           (alexandria:if-let ((url (gethash "url" doc-hash)))
                             (who:htm (:a :target (format nil "win-~a" (uuid:make-v4-uuid))
                                        :href url (who:str title)))
                             (who:str title))
                           (who:htm (:br)))
                         (:a
                           :target (format nil "win-~a" (uuid:make-v4-uuid))
                           :href (format nil "http://localhost:5984/_utils/#database/reddit/~a"
                                         (gethash "id" doc-hash))
                           (who:str (gethash "id" doc-hash)))
                         (who:str (format nil "   ~a/~a"
                                          (gethash "ups" doc-hash)
                                          (gethash "downs" doc-hash)))

                         (:script (who:str
                                   (ps:ps*
                                    `(with-document-ready
                                         (lambda ()
                                           (-> (sel ,(format nil "#hide-~a" (gethash "id" doc-hash)))
                                               (click (lambda (e)
                                                        (chain (sel ,(format nil "#wx~a" (gethash "id" doc-hash)))
                                                               (load ,(format nil
                                                                              "/link/hide/~a"
                                                                              (gethash "id" doc-hash))))
                                                        (-> (sel ,(format nil "#wx~a" (gethash "id" doc-hash)))
                                                            (toggle))
                                                        (-> e (prevent-default)))))))))))
                       (:div :class "panel-body"
                         (alexandria:when-let ((selftext (gethash "selftext" doc-hash)))
                           (who:str (gethash "selftext" doc-hash)))
                         (alexandria:when-let ((images (get-images-from-doc-hash doc-hash)))
                           (map 'nil (lambda (image)
                                       (alexandria:when-let* ((resolutions (gethash "resolutions" image))
                                                              (image-url (gethash "url" (car (reverse resolutions)))))
                                         (who:htm
                                          (:img :class "img-responsive" :src image-url))))
                                images))
                         (:br)
                         (:a :target (format nil "win-~a" (uuid:make-v4-uuid))
                           :href (format nil "https://www.reddit.com~a" (gethash "permalink" doc-hash))
                           (who:str (gethash "subreddit" doc-hash))))))
                   (:div :class "col-md-6"
                     (when (< 0 (length comments))
                       (who:htm (:div :class "panel panel-default panel-borders"
                                  (:div :class "panel-body"
                                    (mapcar (lambda (comment)
                                              (who:htm
                                               (:p
                                                 (who:str (format nil "~a/~a"
                                                                  (gethash "ups" comment)
                                                                  (gethash "downs" comment)))
                                                 (:br)
                                                 (who:str (gethash "body" comment)))))
                                            comments))))))))))
            (gethash "docs"
                     (yason:parse
                      (doc-find "reddit" (make-selector (list (cons "type" "link")
                                                              (cons "suphidden" (alexandria:alist-hash-table
                                                                                 (list (cons "$exists" 'yason:false)))))
                                                        :sort (list (alexandria:alist-hash-table
                                                                     (list (cons "created" "asc"))))
                                                        :limit 500)))))))


