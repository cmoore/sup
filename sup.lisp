
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
  (setf cl-mango:*password* "h4r01d"))

(defparameter *client-id* "NgPcSAMcznk3aQ")

(defparameter *client-secret* "nZZdcddz-BbYEVwpwkhiGOjzRoQ")

(defparameter *local-state* (cl-ppcre:regex-replace-all "-" (format nil "~a" (uuid:make-v4-uuid)) ""))

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
              :json-key "id")
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
   (created :accessor link-created
            :json-type :number
            :json-key "created")
   (written :accessor link-written
            :json-type :number
            :json-key "written"
            :initform (get-universal-time))))

(defun make-request (url)
  (drakma:http-request (format nil "~a~a" "https://www.reddit.com" url)))

(defun authenticate ()
  (setf *reddit-user*
        (json-mop:json-to-clos (flexi-streams:octets-to-string
                                (let ((drakma:*text-content-types* (list (list "application/json"))))
                                  (drakma:http-request "https://www.reddit.com/api/v1/access_token"
                                                       :basic-authorization (list *client-id* *client-secret*)
                                                       ;;:additional-headers (list (cons "User-Agent" "Supyawl/0.1 by clintm"))
                                                       :accept "application/json"
                                                       :content-type "application/json"
                                                       :parameters (list (cons "grant_type" "password")
                                                                         (cons "username" "clintm")
                                                                         (cons "password" "@fancywalking2"))
                                                       :method :post
                                                       :preserve-uri t)))
                               'access-token)))

(defun reddit-request (path)
  (drakma:http-request
   (format nil "https://oauth.reddit.com~a" path)
   :additional-headers (list
                        (cons "User-Agent" "supyawl/0.1 by clintm")
                        (cons "Authorization" (format nil
                                                      "bearer ~a"
                                                      (access-token-access-token *reddit-user*))))))

(defun get-my-subscribed-subreddits ()
  (mapcar (lambda (x)
            (let ((subreddit (json-mop:json-to-clos (with-output-to-string (sink)
                                                      (yason:encode (gethash "data" x) sink))
                                                    'subreddit)))
              (unless (subreddit-find (list (cons "id" (subreddit-reddit-id subreddit))))
                (subreddit-put subreddit))))
          (gethash "children" (gethash "data" (yason:parse
                                               (reddit-request "/subreddits/mine"))))))

(defparameter *listener* nil)

(defun start-server (&key (port 8086))
  (unless *listener*
    (setf *listener* (make-instance 'easy-routes:easy-routes-acceptor
                                    :address "127.0.0.1"
                                    :port port)))
  (hunchentoot:start *listener*))

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
                        (white-header t)
                        (body-class nil)
                        (enable-top-toolbar nil)
                        (enable-page-header nil)
                        (js nil)
                        (js-init nil)) &body body)
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
            ,(unless enable-top-toolbar
               '(:style :type "text/css"
                 (who:str (css-lite:css ((".am-wrapper") (:padding-top "0px !important"))))))
            ,@(when css
                `,css)
            (:link :rel "stylesheet" :type "text/css" :href "https://r.ivy.io/am/css/style.css")
            (:script :src "https://r.ivy.io/am/lib/jquery/jquery.min.js"))
          (:body ,@(when body-class `(:class ,body-class))
            (:div :class ,(format nil "am-wrapper am-nosidebar-left ~a" (or (and white-header "am-white-header")
                                                                            ""))
              ,(when enable-top-toolbar
                 `(:nav :class "navbar navbar-default navbar-fixed-top am-top-header"
                    (:div :class "container-fluid"
                      (:div :class "navbar-header"
                        (:div :class "page-title"
                          (:span ,title))
                        (:a :href "/" :class "navbar-brand"))
                      (:a :href "#" :data-toggle "collapsed" :data-target "#am-navbar-collapse" :class "am-toggle-top-header-menu collapsed"
                        (:span :class "icon s7-angle-down"))
                      (:div :id "am-navbar-collapse" :class "collapse navbar-collapse"
                        (:ul :class "nav navbar-nav am-nav-right"
                          (:li (:a :href "/" "Home"))
                          (when (string= (person-role *greg-person*) "customer")
                            (who:htm
                             (:li (:a :href "/media" "Media"))
                             (:li (:a :href "/customer/setup" "Add Business")))))
                        (:ul :class "nav navbar-nav navbar-right am-icons-nav"
                          (if *greg-person*
                              (who:htm (:li :class "pull-right "(:a :href "/logout" "Logout")))
                              (who:htm (:li :class "pull-right" (:a :href "/login" "Login")))))))))
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
                       (:img :style "max-height:40px;" :class "pull-right,img-responsive" :src (subreddit-icon-img subreddit))
                       (who:str (subreddit-title subreddit)))
                     (:div :class "panel-body"
                       (:a :href (format nil "https://www.reddit.com~a" (subreddit-url subreddit)) (who:str (subreddit-url subreddit))))))))
              (subreddit-get-all)))))

(defroute hide-link ("/link/hide/:id") ()
  (let ((the-link (car (gethash "docs"
                                (yason:parse
                                 (cl-mango:doc-find "reddit"
                                                    (make-selector (list (cons "id" id)))))))))
    (when the-link
      (setf (gethash "suphidden" the-link) t)
      (cl-mango:doc-put "reddit" (with-output-to-string (sink)
                                   (yason:encode the-link sink)))))
  "ok")

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

(defroute links ("/links") ()
  (with-page (:js ((:script
                     (who:str
                      (ps:ps (-> document (add-event-listener "DOMContentLoaded" (@ gfy-collection init) nil)))))))
    (mapcar (lambda (doc-hash)
              (who:htm
               (:div :class "row" :id (format nil "wx~a" (gethash "id" doc-hash))
                 (:div :class "col-md-7 col-md-offset-3"
                   (:div :class "panel panel-default panel-borders"
                     (:div :class "panel-heading"
                       (:button :id (format nil "hide-~a" (gethash "id" doc-hash))
                         :class "btn btn-xs btn-default pull-right" "Close")
                       (alexandria:when-let ((title (gethash "title" doc-hash)))
                         (who:str title)
                         (who:htm (:br)))
                       (:a
                         :target (format nil "win-~a" (uuid:make-v4-uuid))
                         :href (format nil "http://localhost:5984/_utils/#database/reddit/~a" (gethash "id" doc-hash))
                         (who:str (gethash "id" doc-hash)))
                       (who:str (format nil "   ~a/~a" (gethash "ups" doc-hash) (gethash "downs" doc-hash)))

                       (:script (who:str
                                 (ps:ps*
                                  `(-> (sel ,(format nil "#hide-~a" (gethash "id" doc-hash)))
                                       (click (lambda (e)
                                                (-> (sel ,(format nil "#wx~a" (gethash "id" doc-hash)))
                                                    (load ,(format nil "/link/hide/~a" (gethash "id" doc-hash))))
                                                (-> (sel ,(format nil "#wx~a" (gethash "id" doc-hash))) (toggle))
                                                (-> e (prevent-default)))))))))
                     (:div :class "panel-body"
                       (alexandria:when-let ((selftext (gethash "selftext" doc-hash)))
                         (who:str (gethash "selftext" doc-hash)))
                       (alexandria:when-let ((link (gethash "link" doc-hash)))
                         (who:htm
                          (who:str link)
                          (:br)))
                       (alexandria:when-let ((url (gethash "url" doc-hash)))
                         (when (ppcre:scan "gfycat" url)
                           (let ((gfyid (ppcre:regex-replace-all "https://gfycat.com/"
                                                                 url
                                                                 "")))
                             (who:htm (:div :class "gfyitem" :data-id gfyid))))
                         
                         (when (ppcre:scan "imgur.com" url)
                           (let* ((the-url (gethash "url" doc-hash))
                                  (imgurid (get-imgur-id the-url)))
                             (cond
                               
                               ((ppcre:scan ".gifv" the-url)
                                (who:htm
                                 (:img :class "img-responsive"
                                   :src (ppcre:regex-replace ".gifv" the-url ".gif"))))
                               
                               ((or (= 6 (length imgurid))
                                    (= 7 (length imgurid)))
                                (who:htm
                                 (:blockquote :class "imgur-embed-pub" :lang "en" :data-id imgurid)
                                 (:script :async t :src "//s.imgur.com/min/embed.js")))
                               
                               (t (progn
                                    (who:str the-url)
                                    (who:htm (:br))
                                    (who:str imgurid)))))
                           
                           (who:htm (:br))
                           ;; (let ((imgurid (get-imgur-id url)))
                           ;;   (if (= (length imgurid) 7)
                           ;;       (who:htm
                           ;;        (who:str imgurid)
                           ;;        (:img :src url :class "img-responsive"))))
                           )
                         (when (ppcre:scan ".jpg|.png|.gif" url)
                           (who:htm (:img :src url :class "img-responsive"))))
                       (:br)
                       (:a :target (format nil "win-~a" (uuid:make-v4-uuid))
                         :href (format nil "https://www.reddit.com~a" (gethash "permalink" doc-hash))
                         (who:str (gethash "subreddit" doc-hash)))
                       (:br)
                       (who:str (gethash "url" doc-hash))))))))

            (gethash "docs"
                     (yason:parse
                      (doc-find "reddit" (make-selector (list (cons "type" "link")
                                                              (cons "suphidden" (alexandria:alist-hash-table
                                                                                 (list (cons "$exists" 'yason:false)))))
                                                        :sort (list (alexandria:alist-hash-table
                                                                     (list (cons "created" "desc"))))
                                                        :limit 1000)))))))

(defun scan-comments ()
  (mapcar (lambda (link)
            (log:info "link")
            (let ((data (drakma:http-request
                         (format nil
                                 "https://www.reddit.com~acomments.json?limit=100"
                                 (link-permalink link)))))
              (mapcar (lambda (listing)
                        (mapcar (lambda (comment)
                                  (let ((new-comment (json-mop:json-to-clos
                                                      (with-output-to-string (sink)
                                                        (yason:encode (gethash "data" comment) sink))
                                                      'comment)))
                                    (unless (comment-find (list (cons "id" (comment-reddit-id new-comment))))
                                      (handler-case (comment-put new-comment)
                                        (cl-mango:unexpected-http-response (condition)
                                          (declare (ignore condition))
                                          (log:info "caught error"))))))
                                (gethash "children" (gethash "data" listing))))
                      (yason:parse data))))
          (link-get-all)))

(defun get-latest-post-id-for-subreddit (subreddit)
  (log:info "check" subreddit)
  (let ((dochash (car (gethash "docs"
                               (yason:parse
                                (doc-find "reddit"
                                          (make-selector (list (cons "subreddit" subreddit))
                                                         :limit 1
                                                         :sort (list (alexandria:alist-hash-table
                                                                      (list (cons "created" "desc")))))))))))
    (when dochash
      (gethash "id" dochash))))

(defun scan-links ()
  (authenticate)
  (mapc (lambda (sub)
          (cl-mango:doc-delete "reddit" (gethash "_id" sub) (gethash "_rev" sub)))
        (gethash "docs"
                 (yason:parse
                  (cl-mango:doc-find "reddit" (make-selector (list (cons "type" "subreddit")))))))
  (get-my-subscribed-subreddits)
  (mapc (lambda (subreddit)
          (let ((latest-id (get-latest-post-id-for-subreddit (subreddit-display-name subreddit))))
            (log:info latest-id)
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
              (log:info "links" (length links))
              (mapc (lambda (tmp)
                      (let ((new-link (gethash "data" tmp)))
                        (setf (gethash "type" new-link) "link")
                        (setf (gethash "written" new-link) (get-universal-time))
                        (setf (gethash "_id" new-link) (gethash "id" new-link))
                        (handler-case (cl-mango:doc-put "reddit" (with-output-to-string (sink)
                                                                   (yason:encode new-link sink)))
                          (cl-mango:unexpected-http-response (condition)
                            (declare (ignore condition))))))
                    links))))
        (subreddit-get-all)))

(defun refresh ()
  (log:info "Links")
  (scan-links)
  (sleep 180))

(defun start-refresh-thread ()
  (bt:make-thread (lambda ()
                    (loop
                      (refresh)
                      nil))
                  :name "thresher"))

(defun clean-out ()
  (mapc (lambda (d)
          (doc-delete "reddit"
                      (gethash "_id" d)
                      (gethash "_rev" d)))
        (gethash "docs"
                 (yason:parse
                  (doc-find "reddit"
                            (make-selector (list (cons "type" "link") (cons "subreddit" "carcrash"))))))))
(defun clean-out ()
  (let ((links (link-find-explicit (list (cons "type" "link")) :limit 100)))
    (when (< 0 (length links))
      (log:info (length links))
      (mapc #'link-delete links)
      (log:info "step")
      (clean-out))))

(defun mark-all-as-read ()
  (length
   (mapc (lambda (x)
           (setf (gethash "suphidden" x) 't)
           (cl-mango:doc-put "reddit" (with-output-to-string (sink)
                                        (yason:encode x sink))))
         (gethash "docs"
                  (yason:parse
                   (doc-find "reddit" (make-selector (list (cons "type" "link")
                                                           (cons "suphidden" (alexandria:alist-hash-table
                                                                              (list (cons "$exists" 'yason:false)))))
                                                     :sort (list (alexandria:alist-hash-table
                                                                  (list (cons "created" "desc"))))
                                                     :limit 10000)
                             ))))))









(defparameter *append-box* (mailbox:make-mailbox))
(defparameter *overwrite-box* (mailbox:make-mailbox))

(defun cap-string (string)
  (let* ((splitted (split-sequence:split-sequence #\Space string))
         (first-word (string-capitalize (car splitted)))
         (correct (format nil "~{~a~^ ~}" (append (list first-word) (cdr splitted)))))
    correct))

(defun start-boxer-thread ()
  (bt:make-thread (lambda ()
                    (loop
                      (boxer)
                      (sleep 1)))
                  :name "boxer"))
(defun boxer ()
  (labels ((write-all-overwrites ()
             (multiple-value-bind (item more)
                 (mailbox:read-mail *overwrite-box*)
               (when item
                 (log:info "overwrite")
                 (with-open-file (out "~/Desktop/speech.txt"
                                      :direction :output
                                      :if-exists :supersede)
                   (format out "~a.~%" (cap-string item)))
                 (when more
                   (write-all-overwrites)))))
           (write-all-appends ()
             (multiple-value-bind (item more)
                 (mailbox:read-mail *append-box*)
               (when item
                 (log:info "append")
                 (with-open-file (out "~/Desktop/speech.txt"
                                      :direction :output
                                      :if-exists :append)
                   (format out "~a." item))
                 (when more
                   (write-all-appends))))))
    (write-all-overwrites)
    (write-all-appends)))

(defroute append-speech ("/speech/add/append" :method :post) (bundle)
  (mailbox:post-mail bundle *append-box*)
  "ok")

(defroute add-speech ("/speech/add/overwrite" :method :post) (bundle)
  (mailbox:post-mail bundle *overwrite-box*)
  "ok")

(defroute test ("/speech") ()
  (with-page ()
    (:div :class "row"
      (:div :style "font-size:24px;" :id "output" :class "col-md-6 col-md-offset-3" ""))
    (:script
      (who:str
       (ps
         (defvar speech nil)
         ;; (defun append-full (sentence)
         ;;   (setf (@ (-> document (get-element-by-id "output")) inner-h-t-m-l) sentence))
         ;; (defun append-partial (sentence)
         ;;   (incf (@ (-> document (get-element-by-id "output")) inner-h-t-m-l) sentence))
         (with-document-ready
             (lambda ()
               (setf speech (new (webkit-speech-recognition)))
               (setf (@ speech lang) "en-US")
               (setf (@ speech continuous) t)
               (setf (@ speech interim-results) false)
                                
               (setf (@ speech onresult) (lambda (event)
                                           (map (lambda (block)
                                                  (-> console (log "result"))
                                                  (when (@ block is-final)
                                                    (let ((output (@ block 0 transcript))
                                                          (reg (regex "/^ /")))
                                                      (if (-> reg (exec output))
                                                          ((@ $ ajax) (create url "http://localhost:8086/speech/add/append"
                                                                              data (create "bundle" output)
                                                                              method "POST"))
                                                          ((@ $ ajax) (create url "http://localhost:8086/speech/add/overwrite"
                                                                              data (create "bundle" output)
                                                                              method "POST"))))))
                                                (@ event results))))
               (setf (@ speech onend) (lambda (event)
                                        (-> console (log "onend"))
                                        (-> speech (start))))
               (-> speech (start)))))))))



(defclass chat-room (hunchensocket:websocket-resource)
  ((name :initarg :name :initform (error "Name this room!") :reader name))
  (:default-initargs :client-class 'user))

(defclass user (hunchensocket:websocket-client)
  ((name :initarg :user-agent :reader name :initform (error "Name this user!"))))

(defvar *chat-rooms* (list (make-instance 'chat-room :name "/ws/cc")
                           (make-instance 'chat-room :name "/ws/append")))

(defun find-room (request)
  (find (hunchentoot:script-name request) *chat-rooms* :test #'string= :key #'name))

(pushnew 'find-room hunchensocket:*websocket-dispatch-table*)

(defun broadcast (room message &rest args)
  (loop for peer in (hunchensocket:clients room)
        do (hunchensocket:send-text-message peer (apply #'format nil message args))))

(defmethod hunchensocket:client-connected ((room chat-room) user)
  (hunchensocket:send-text-message user "Connecting to the message bridge...")
  (attach-queue-handler (lambda (message)
                          (hunchensocket:send-text-message user
                                                           (format nil "~a~%"
                                                                   (flexi-streams:octets-to-string
                                                                    (cl-rabbit:message/body
                                                                     (cl-rabbit:envelope/message message))))))))

(defmethod hunchensocket:client-disconnected ((room chat-room) user)
  (broadcast room "~a has left ~a" (name user) (name room)))

(defmethod hunchensocket:text-message-received ((room chat-room) user message)
  (broadcast room "~a says ~a" (name user) message))  

(defvar *ws-server* (make-instance 'hunchensocket:websocket-acceptor :port 8087))

(defun start-ws-server ()
  (hunchentoot:start *ws-server*))
