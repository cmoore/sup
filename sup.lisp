;;; -*- mode: Lisp; Syntax: COMMON-LISP; Package: CL-MANGO; Base: 10 eval: (hs-hide-all) -*-

(defpackage :sup
  (:use #:cl
        #:json-mop
        #:easy-routes
        #:parenscript
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

(defmango comment "reddit"
  ((reddit-id :accessor comment-reddit-id
              :json-type :string
              :json-key "id")
   (body :accessor comment-body
         :json-type :string
         :json-key "body")
   (ups :accessor comment-ups
        :json-type :number
        :json-key "ups")
   (downs :accessor comment-downs
          :json-type :number
          :json-key "downs")
   (author :accessor comment-author
           :json-type :string
           :json-key "author")
   (score :accessor comment-score
          :json-type :number
          :json-key "score")
   (link-id :accessor comment-link-id
            :json-type :string
            :json-key "link_id")
   (created-utc :accessor comment-created-utc
                :json-type :number
                :json-key "created_utc")
   (created :accessor comment-created
            :json-type :number
            :json-key "created")))

(defmango subreddit "reddit"
  ((display-name :accessor subreddit-display-name
                 :json-type :string
                 :json-key "display_name")
   (icon-img :accessor subreddit-icon-img
             :json-type :string
             :json-key "icon_img")
   (url :accessor subreddit-url
        :json-type :string
        :json-key "url")
   (title :accessor subreddit-title
          :json-type :string
          :json-key "title")
   (reddit-id :accessor subreddit-reddit-id
              :json-type :string
              :json-key "id")))

(defmango link "reddit"
          ((reddit-id :accessor link-reddit-id
                      :json-type :string
                      :json-key "reddit-id")
           (subreddit :accessor link-subreddit
                      :json-type :string
                      :json-key "subreddit")
           (title :accessor link-title
                  :json-type :string
                  :json-key "title")
           (permalink :accessor link-permalink
                      :json-type :string
                      :json-key "permalink")
           (url :accessor link-url
                :json-type :string
                :json-key "url")
           (author :accessor link-author
                   :json-type :string
                   :json-key "author")
           (downs :accessor link-downs
                  :json-type :number
                  :json-key "downs")
           (ups :accessor link-ups
                :json-type :number
                :json-key "ups")
           (text :accessor link-text
                 :json-type :string
                 :json-key "selftext")
           (thumbnail :accessor link-thumbnail
                      :json-type :string
                      :json-key "thumbnail")
           (created :accessor link-created
                    :json-type :number
                    :json-key "created")
           (written :accessor link-written
                    :json-type :number
                    :json-key "written"
                    :initform (get-universal-time))
           (suphidden :accessor link-suphidden
                      :json-type :bool
                      :json-key "suphidden"
                      :initform nil)))

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
     (yason:encode list sink))))


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

(defun scan-comments ()
  (authenticate)
  (mapcar (lambda (link)
            (log:info "link")
            (let ((data (drakma:http-request
                         (format nil
                                 "https://www.reddit.com~acomments.json?limit=100"
                                 (gethash "permalink" link)))))
              (mapcar (lambda (listing)
                        (mapcar (lambda (comment)
                                  (let ((new-comment (gethash "data" comment)))
                                    (unless (comment-find (list (cons "reddit-id" (gethash "id" new-comment))))
                                      (setf (gethash "type" new-comment) "comment")
                                      (handler-case (doc-put "reddit" (to-json new-comment))
                                        (cl-mango:unexpected-http-response (condition)
                                                                           (declare (ignore condition))
                                                                           (log:info "caught error"))))))
                                (gethash "children" (gethash "data" listing))))
                      (yason:parse data))))
          (gethash "docs"
                   (yason:parse
                    (doc-find "reddit" (make-selector (list (cons "type" "link")
                                                            (cons "suphidden" (alexandria:alist-hash-table
                                                                               (list (cons "$exists" 'yason:false)))))))))))

(defun get-latest-post-id-for-subreddit (subreddit)
  (let ((dochash (car (gethash "docs"
                               (yason:parse
                                (doc-find "reddit"
                                          (make-selector (list (cons "subreddit" subreddit))
                                                         :limit 1
                                                         :sort (list (alexandria:alist-hash-table
                                                                      (list (cons "created" "desc")))))))))))
    (when dochash
      (gethash "reddit-id" dochash))))


(defun has-subreddit-p (subreddit-name)
  (subreddit-find (list (cons "display_name" subreddit-name)
                        (cons "type" "subreddit"))))

(defun get-db-subreddits ()
  (gethash "docs"
                (yason:parse
                 (cl-mango:doc-find "reddit" (make-selector
                                              (list
                                               (cons "type" "subreddit"))
                                              :limit 1000)))))

(defun get-reddit-subreddits ()
  (mapcar (lambda (x)
            (gethash "data" x))
          (gethash "children" (gethash "data" (yason:parse
                                               (reddit-request "/subreddits/mine"))))))



;;;                  (unless (has-subreddit-p (gethash "display_name" reddit-subreddit))
;;;                    (setf (gethash "type" reddit-subreddit) "subreddit")
;;;                    (doc-put "reddit-test" (to-json reddit-subreddit)))

(defun sync-subreddits ()
  (let ((reddit-subreddits (get-reddit-subreddits))
        (db-subreddits (get-db-subreddits)))
;;;     (mapcar (lambda (reddit-subreddit)
;;;               (unless (has-subreddit-p (gethash "display_name" reddit-subreddit))
;;;                 (setf (gethash "type" reddit-subreddit) "subreddit")
;;;                 (doc-put "reddit-test" (to-json reddit-subreddit))))
;;;             reddit-subreddits)

    (let ((db-subreddit-names (hash-extract "display_name" db-subreddits)))
      (mapcar (lambda (reddit-subreddit-name)
                (unless (member reddit-subreddit-name db-subreddit-names)
                  (format nil "Drop ~a" reddit-subreddit-name)))
              (hash-extract "display_name" reddit-subreddits)))))


(defun hash-extract (name-string list-of-hashes)
  (mapcar (lambda (x)
            (gethash name-string x))
          list-of-hashes))

(defun test ()
  (let ((reddit-subreddit-names (hash-extract "display_name" (get-reddit-subreddits))))
    (remove-if #'null
               (mapcar (lambda (db-subreddit-name)
                         (unless (member db-subreddit-name reddit-subreddit-names
                                         :test #'string=)
                           db-subreddit-name))
                       (hash-extract "display_name" (get-db-subreddits))))))
                       

(defun scan-links ()
  (authenticate)
  (mapc (lambda (subreddit)
          (let ((latest-id (get-latest-post-id-for-subreddit
                            (subreddit-display-name subreddit))))
            (let ((links (gethash "children"
                                  (gethash "data"
                                           (yason:parse
                                            (drakma:http-request
                                             (if latest-id
                                                 (format nil "https://www.reddit.com~a.json?limit=100&after=~a"
                                                         (subreddit-url subreddit)
                                                         latest-id)
                                                 (format nil "https://www.reddit.com~a.json?limit=100"
                                                         (subreddit-url subreddit)))))))))
              (mapc (lambda (tmp)
                      (let ((new-link (gethash "data" tmp)))
                        (unless (link-find (list (cons "reddit-id" (gethash "id" new-link))))
                          (setf (gethash "type" new-link) "link")
                          (setf (gethash "written" new-link) (get-universal-time))
                          (setf (gethash "reddit-id" new-link) (gethash "id" new-link))
                          (handler-case (cl-mango:doc-put "reddit" (to-json new-link))
                            (cl-mango:unexpected-http-response (condition)
                              (declare (ignore condition))
                              nil)
                            (error (condition)
                              (log:info "WTF"))))))
                    links))))
        (subreddit-get-all)))

(defun refresh ()
  (scan-links)
  (scan-comments)
  (sleep 180))

(defun start-refresh-thread ()
  (bt:make-thread (lambda ()
                    (loop
                      (refresh)
                      nil))
                  :name "sup post fetcher/thresher"))

(defun mark-all-as-read ()
  (length
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
                                                     :limit 1000)))))))

(defroute comments "/comments" ()
  (with-page ()
    (mapcar (lambda (comment)
              (who:htm
               (:div :class "row"
                 (:div :class "col-md-6"
                   (:div :class "panel panel-default panel-borders"
                     (:div :class "panel-body"
                       (:a :href (format nil "https://www.reddit.com/u/~a" (comment-author comment)) (who:str (comment-author comment)))
                       (:br)
                       (who:str (comment-body comment))))))))
            (comment-find-explicit (list (cons "type" "comment"))
                                   :sort (list (alexandria:alist-hash-table
                                                (list (cons "created" "desc"))))))))

(defroute index ("/") ()
  (with-page ()
    (:div :class "row"
      (mapcar (lambda (subreddit)
                (who:htm
                 (:div :class "col-md-6"
                   (:div :class "panel panel-default panel-borders"
                     (:div :class "panel-heading"
                       (:img
                        :style "max-height:40px;"
                        :class "pull-right,img-responsive"
                        :src (subreddit-icon-img subreddit))
                       (who:str (subreddit-title subreddit)))
                     (:div :class "panel-body"
                       (:a :href (format nil "https://www.reddit.com~a" (subreddit-url subreddit)) (who:str (subreddit-url subreddit))))))))
              (subreddit-get-all)))))

(defroute hide-link ("/link/hide/:id") ()
  (let ((the-link (car (link-find (list (cons "reddit-id" id))))))
    (when the-link
      (setf (link-suphidden the-link) t)
      (link-update the-link)))
  "ok")

(defroute links ("/links") ()
  (declare (optimize (debug 3)))
  (with-page (:js ((:script
                    (who:str
                     (ps:ps (-> document
                                (add-event-listener "DOMContentLoaded"
                                                    (@ gfy-collection init) nil)))))))
    (:div :class "row"
     (:div :class "col-md-6"
      (mapcar (lambda (doc-hash)
                (who:htm
                 (:div
                  :class "panel panel-default panel-borders"
                  :id (format nil "wx~a" (gethash "reddit-id" doc-hash))
                  (:div :class "panel-heading"
                   (:button :id (format nil "hide-~a" (gethash "reddit-id" doc-hash))
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
                                  (gethash "reddit-id" doc-hash))
                    (who:str (gethash "reddit-id" doc-hash)))
                   (who:str (format nil "   ~a/~a"
                                    (gethash "ups" doc-hash)
                                    (gethash "downs" doc-hash)))

                   (:script (who:str
                             (ps:ps*
                              `(with-document-ready
                                (lambda ()
                                  (-> (sel ,(format nil "#hide-~a" (gethash "reddit-id" doc-hash)))
                                      (click (lambda (e)
                                               (chain (sel ,(format nil "#wx~a" (gethash "reddit-id" doc-hash)))
                                                      (load ,(format nil
                                                                     "/link/hide/~a"
                                                                     (gethash "reddit-id" doc-hash))))
                                               (-> (sel ,(format nil "#wx~a" (gethash "reddit-id" doc-hash)))
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
                   (who:str (gethash "url" doc-hash))
                   (:br)
                   (:a :target (format nil "win-~a" (uuid:make-v4-uuid))
                    :href (format nil "https://www.reddit.com~a" (gethash "permalink" doc-hash))
                    (who:str (gethash "subreddit" doc-hash)))))))
              (gethash "docs"
                       (yason:parse
                        (doc-find "reddit" (make-selector (list (cons "type" "link")
                                                                (cons "suphidden" (alexandria:alist-hash-table
                                                                                   (list (cons "$exists" 'yason:false)))))
                                                          :sort (list (alexandria:alist-hash-table
                                                                       (list (cons "created" "asc"))))
                                                          :limit 500)))))))))

(defun mark-subreddit-as-read (subreddit)
  (map 'nil (lambda (doc-hash)
              (setf (gethash "suphidden" doc-hash) 't)
              (doc-put "reddit" (to-json doc-hash)))
       (gethash "docs"
                (yason:parse
                 (doc-find "reddit" (make-selector
                                     (list (cons "subreddit" subreddit)
                                           (cons "suphidden" (alexandria:alist-hash-table
                                                              (list (cons "$exists" 'yason:false))))
                                           (cons "type" "link"))
                                     :limit 10000))))))
