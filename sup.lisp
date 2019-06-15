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
    (defparameter *client-id* (gethash "client-id" *config*))
    (defparameter *client-secret* (gethash "client-secret" *config*))
    (setf yason:*parse-json-booleans-as-symbols* t)
    (setf hunchentoot:*catch-errors-p* t)
    (push (cons "application" "json") drakma:*text-content-types*)))

(defmacro with-pg (&body body)
  `(with-connection (list "reddit" "sup" "fl33j0b" "localhost" :pooled-p t)
     ,@body))

(defparameter *reddit-user* nil)

(defparameter *listener* nil)

(defparameter *mailbox* (mailbox:make-mailbox))

(defclass link ()
  ((id :initarg :id
       :col-type :text
       :accessor link-id)
   (bulk :initarg :bulk
         :col-type :text
         :accessor link-bulk)
   (favorite :initarg :favorite
             :col-type :bool
             :accessor link-favorite
             :initform nil)
   (hidden :initarg :hidden
           :col-type :bool
           :initform nil
           :accessor link-hidden)
   (written :initarg :written
            :col-type :bigint
            :accessor link-written)
   (created-utc :initarg :created-utc
                :col-type :bigint
                :accessor link-created-utc)
   (url :initarg :url
        :col-type :text
        :accessor link-url)
   (permalink :initarg :permalink
              :col-type :text
              :accessor link-permalink)
   (author :initarg :author
           :col-type :text
           :accessor :link-author)
   (subreddit-id :initarg :subreddit-id
                 :col-type :text
                 :accessor link-subreddit-id)
   (media-only :initarg :media-only
               :col-type :bool
               :accessor link-media-only)
   (title :initarg :title
          :col-type :text
          :accessor link-title)
   (author-fullname :initarg :author-fullname
                    :col-type :text
                    :accessor link-author-fullname)
   (body :initarg :body
         :col-type (or db-null text)
         :accessor link-body)
   (selftext :initarg :selftext
             :col-type :text
             :accessor link-selftext)
   (subreddit :initarg :subreddit
              :col-type :text
              :accessor link-subreddit)
   (nsfw :initarg :nsfw
         :col-type :bool
         :accessor link-nsfw))
  (:metaclass dao-class)
  (:keys id))

(deftable link
  (!dao-def)
  (!unique-index :id))


(defclass subreddit ()
  ((id :initarg :id
       :col-type :text
       :accessor subreddit-id)
   (url :initarg :url
        :col-type :text
        :accessor subreddit-url)
   (display-name :initarg :display-name
                 :col-type :text
                 :accessor subreddit-display-name))
  (:metaclass dao-class)
  (:keys id))

(deftable subreddit
  (!dao-def)
  (!unique-index :id))


(defclass up-history ()
  ((id :initarg :id
       :col-type :text
       :accessor up-history-id)
   (scores :initarg :scores
           :accessor up-history-scores
           :col-type :integer[]))
  (:metaclass dao-class)
  (:keys id))

(deftable up-history
  (!dao-def)
  (!unique-index :id))


(defclass comment ()
  ((id :initarg :id
       :col-type :text
       :accessor comment-id)
   (score :initarg :score
          :col-type integer
          :accessor comment-score)
   (parent :initarg :parent
           :accessor comment-parent
           :col-type (or db-null text)
           :initform nil)
   (author :initarg :author
           :col-type :text
           :accessor comment-author)
   (link-id :initarg :link-id
            :col-type :text
            :accessor comment-link-id)
   (body-html :initarg :body-html
              :col-type :text
              :accessor comment-body-html))
  (:metaclass dao-class)
  (:keys id))

(deftable comment
  (!dao-def)
  (!unique-index :id))


(defgeneric make-author-link (object))

(defmethod make-author-link ((link link))
  (with-slots (author) link
    (with-html
      (:div.icon
       (:a :target (format nil "~aauthorlink" author)
         :href (format nil "/author/~a" author)
         (:i :class "s7-user-female"))))))

(defmethod make-author-link ((comment comment))
  (with-slots (author) comment
    (with-html
      (:div.icon
       (:a :target (format nil "~aauthorcomments" author)
         :href (format nil "/comments/author/~a" author)
         (:i :class "s7-note"))))))

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

(defun reddit-request (path &key retry)
  (drakma:http-request (format nil "https://oauth.reddit.com~a" path)
                       :additional-headers (list
                                            (cons "User-Agent" "supyawl/0.1 by clintm")
                                            (cons "Authorization" (format nil
                                                                          "bearer ~a"
                                                                          (access-token-access-token *reddit-user*))))))

(defun start-server (&key (port 8086))
  (unless *listener*
    (setf *listener* (make-instance 'easy-routes:easy-routes-acceptor
                                    :address "127.0.0.1"
                                    :access-log-destination nil
                                    :document-root (asdf:system-relative-pathname
                                                    :sup "static/am")
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

(defun hash-extract (name-string list-of-hashes)
  (mapcar (lambda (x)
            (gethash name-string x))
          list-of-hashes))

(defun get-reddit-subreddits ()
  (authenticate)
  (alex:when-let ((post-data (gethash "data" (yason:parse
                                              (reddit-request "/subreddits/mine")))))
    (hash-extract "data"
                  (gethash "children" post-data))))

(defun sync-subreddits ()
  (alex:if-let ((reddit-subreddits (get-reddit-subreddits)))
    (let* ((reddit-names (hash-extract "display_name" reddit-subreddits))
           (db-subreddits (with-pg (select-dao 'subreddit)))
           (db-names (mapcar #'subreddit-display-name db-subreddits)))
      
      ;; Confirm that all subscribed subreddits are in the database.
      (dolist (reddit-name reddit-names)
        (unless (member reddit-name db-names :test #'string=)
          (let ((reddit-hash (car (remove-if-not #'(lambda (x)
                                                     (string= (gethash "display_name" x) reddit-name))
                                                 reddit-subreddits))))
            (with-pg (insert-dao
                      (make-instance 'subreddit
                                     :display-name (gethash "display_name" reddit-hash)
                                     :url (gethash "url" reddit-hash)
                                     :id (gethash "id" reddit-hash))))
            (log:info "Missing ~a" (gethash "display_name" reddit-hash)))))
      
      ;; Remove db subreddits that aren't currently subscribed.
      (map nil (lambda (db-name)
                 (unless (member db-name reddit-names :test #'string=)
                   (let ((db-record (car (remove-if-not #'(lambda (x)
                                                            (string= (subreddit-display-name x) db-name))
                                                        db-subreddits))))
                     (log:info "Removing ~a" db-name)
                     (with-pg (delete-dao db-record)))))
           db-names))
    (log:info "Failed")))

(defun make-history-url (subreddit first-id)
  (format nil "https://www.reddit.com~anew.json?limit=100&before=~a"
          (subreddit-url subreddit) (format nil "t3_~a" first-id)))

(defun make-normal-url (subreddit)
  (format nil "https://www.reddit.com~a.json?limit=100"
          (subreddit-url subreddit)))

(defun make-new-url (subreddit)
  (format nil "https://www.reddit.com~anew.json?limit=100"
          (subreddit-url subreddit)))

(defmethod get-oldest-subreddit-link-id ((subreddit subreddit))
  (alex:when-let ((link (car (with-pg
                               (query (:limit
                                       (:order-by
                                        (:select '* :from 'link :where (:= 'subreddit (subreddit-display-name subreddit)))
                                        (:asc 'created_utc))
                                       1) (:dao link))))))
    (link-id link)))


(defun get-reddit (path &key retry)
  (multiple-value-bind (data code)
      (drakma:http-request (format nil "https://reddit.com~a" path))
    (alex:switch (code)
      (200 data)
      (t (and retry (get-reddit path))))))

(defmethod sync-subreddit ((subreddit subreddit))
  "Fetch the entire history of this subreddit."
  (declare (optimize (debug 3) (speed 1) (safety 3)))
  (labels ((make-request-url ()
             (format nil "~a.json" (subreddit-url subreddit)))
           (get-next-page (after-id)
             (let ((link-hash (yason:parse (get-reddit (format nil "~a?after=~a"
                                                               (make-request-url)
                                                               after-id)))))
               (dolist (link (hu:hash-get link-hash '("data" "children")))
                 (when (string= (gethash "kind" link) "t3")
                   (handle-possible-new-link (gethash "data" link))))
               (alex:when-let ((next-page (hu:hash-get link-hash '("data" "after"))))
                 (sleep 0.5)
                 (get-next-page next-page)))))
    (get-next-page (hu:hash-get (yason:parse (get-reddit (make-request-url)))
                                '("data" "after")))))


(defgeneric get-subreddit-links (subreddit &key new))

(defmethod get-subreddit-links ((subreddit subreddit) &key new historic)
  (multiple-value-bind (data code)
      (handler-case
          (let ((url (cond (new (make-new-url subreddit))
                           (historic (make-history-url subreddit
                                                       (format nil "t3_~a"
                                                               (get-oldest-subreddit-link-id subreddit))))
                           (t (make-normal-url subreddit)))))
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
                   (subreddit-display-name subreddit))))))

(defun is-repostp (link-hash)
  (< 0 (length (with-pg (select-dao 'link (:and (:!= 'id (gethash "id" link-hash))
                                                (:= 'url (gethash "url" link-hash))))))))

(defmethod send-update-graph ((link link))
  (mailbox:post-mail (to-json (alex:alist-hash-table
                               (list (cons "id" (link-id link))
                                     (cons "action" "update-graph"))))
                     *mailbox*))

(defun update-all-graphs ()
  (dolist (link (with-pg (select-dao 'link (:= 'hidden nil))))
    (send-update-graph link)))

(defun handle-possible-new-link (link-hash)
  (declare (optimize (debug 3) (speed 0) (safety 3)))
  
  (labels ((make-new-like-cache ()
             (with-pg (insert-dao (make-instance 'up-history
                                                 :id (gethash "id" link-hash)
                                                 ;; You can create it with a list, but it will come
                                                 ;; back as a simple-vector
                                                 :scores (vector (gethash "score" link-hash))))))
           (update-like-cache ()
             (alex:if-let ((up-hist (with-pg (get-dao 'up-history (gethash "id" link-hash)))))
               (progn
                 (let ((scores (make-array (length (up-history-scores up-hist))
                                           :initial-contents (up-history-scores up-hist)
                                           :adjustable t
                                           :fill-pointer t)))
                   (vector-push-extend (gethash "score" link-hash) scores)
                   (setf (up-history-scores up-hist) scores))
                 (with-pg (update-dao up-hist)))
               (make-new-like-cache))))

    ;; Reposts can get fucked.
    (when (is-repostp link-hash)
      (return-from handle-possible-new-link))

    (alex:if-let ((existing-link (with-pg (get-dao 'link (gethash "id" link-hash)))))
      (progn
        (when (link-hidden existing-link)
          (update-like-cache)
          (return-from handle-possible-new-link))
        (update-like-cache)
        (mailbox:post-mail (to-json
                            (alex:alist-hash-table
                             (list (cons "id" (gethash "id" link-hash))
                                   (cons "action" "update-graph"))))
                           *mailbox*))
      (progn
        (log:info "~a ~a" (gethash "subreddit" link-hash) (gethash "title" link-hash))
        (let ((new-link (make-instance 'link
                                       :id (gethash "id" link-hash)
                                       :hidden nil
                                       :written (get-universal-time)
                                       :created-utc (gethash "created_utc" link-hash)
                                       :url (gethash "url" link-hash)
                                       :permalink (gethash "permalink" link-hash)
                                       :author (gethash "author" link-hash)
                                       :subreddit-id (gethash "subreddit_id" link-hash)
                                       :media-only (and (equal 'yason:true
                                                               (gethash "media_only" link-hash))
                                                        t)
                                       :score (gethash "score" link-hash)
                                       :title (gethash "title" link-hash)
                                       :nsfw (and (equal 'yason:true
                                                         (gethash "over_18" link-hash))
                                                  t)
                                       :author-fullname (gethash "author_fullname" link-hash)
                                       :selftext (gethash "selftext" link-hash)
                                       :subreddit (gethash "subreddit" link-hash)
                                       :bulk (to-json link-hash)
                                       :body (gethash "selftext_html" link-hash))))
          (with-pg (insert-dao new-link))
          (update-like-cache)
          ;; (mailbox:post-mail (to-json (alex:alist-hash-table
          ;;                              (list (cons "id" (link-id new-link))
          ;;                                    (cons "action" "add-link"))))
          ;;                    *mailbox*)
          ;;(send-update-graph new-link)
          )))))

(defun scan-local-link (permalink)
  (multiple-value-bind (data code)
      (drakma:http-request (format nil "https://www.reddit.com~a.json" permalink))
    (when (equal code 200)
      (handle-possible-new-link
       (hu:hash-get (yason:parse data)
                    '(0 "data" "children" 0 "data"))))))

(defun scan-local-links (&key all)
  (let ((links (if all
                 (with-pg (query (:select 'id :from 'link)))
                 (with-pg (query (:select 'id :from 'link :where (:= 'hidden nil)))))))
    (log:info "Scanning ~a links." (length links))
    (dolist (link-id (alex:flatten links))
      (let ((link (with-pg (get-dao 'link link-id))))
        (log:info "~a ~a" (link-id link) (link-subreddit link))
        (scan-local-link (link-permalink link))
        (send-update-graph link)
        (sleep 0.5)))))

(defmethod update-subreddit ((subreddit subreddit))
  (let ((yason:*parse-json-booleans-as-symbols* nil))
    (unless (ppcre:scan "u_" (subreddit-display-name subreddit))
      (dolist (link (hash-extract "data" (get-subreddit-links subreddit)))
        (handle-possible-new-link link))
      (dolist (link (hash-extract "data" (get-subreddit-links subreddit :new t)))
        (handle-possible-new-link link)))))

(defun scan-subreddits ()
  (dolist (subreddit (with-pg (select-dao 'subreddit)))
    (update-subreddit subreddit)
    (sleep 1)))

(defun start-refresh-threads ()
  (bt:make-thread (lambda ()
                    (loop
                      (sync-subreddits)
                      (scan-subreddits)
                      (sleep 60)))
                  :name "fetchers"))

(defun stop-refresh-threads ()
  (cl-ivy:stop-thread-by-name "fetchers"))

(defmethod mark-subreddit-as-read ((subreddit subreddit))
  (with-slots (display-name) subreddit
    (dolist (link (with-pg (select-dao 'link (:and (:= 'hidden nil)
                                                   (:= 'subreddit display-name)))))
      (setf (link-hidden link) t)
      (with-pg (update-dao link)))))

(defmethod mark-subreddit-as-unread ((subreddit subreddit))
  (with-slots (display-name) subreddit
    (dolist (link (with-pg (select-dao 'link (:and (:= 'subreddit display-name)
                                                   (:= 'hidden t)))))
      (setf (link-hidden link) nil)
      (with-pg (update-dao link)))))

(defmethod display-link ((link link))
  (with-slots (id title url author subreddit created-utc permalink) link
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
                 (make-author-link link)
                 (:div.icon :id (format nil "favorite-~a" unique-id)
                            (:span.s7-download))
                 (:div.icon :id (format nil "hide-~a" unique-id)
                            (:span.s7-close-circle)))
                (:a
                  :style "font-weight:bold;"
                  :target (format nil "win-~a" (uuid:make-v4-uuid))
                  :href url (html-entities:decode-entities title))
                (:br)
                (:div
                  (:a :target (format nil "win-~a" (uuid:make-v4-uuid))
                    :href (format nil "https://www.reddit.com~a" permalink)
                    (format nil "~a in ~a on ~a"
                            author
                            subreddit
                            (local-time:to-rfc1123-timestring
                             (local-time:unix-to-timestamp
                              (rationalize created-utc))))))
                (:div :class "col-md-12")
                (:script
                  (ps:ps*
                   `(with-document-ready
                        (lambda ()
                          (-> (sel ,(format nil "#favorite-~a" unique-id))
                              (click (lambda (e)
                                       (chain (sel ,(format nil "#wx~a" unique-id))
                                              (load ,(format nil "/link/favorite/~a" id)))
                                       (-> (sel ,(format nil "#wx~a" unique-id))
                                           (toggle))
                                       (-> e (prevent-default)))))
                          (-> (sel ,(format nil "#hide-~a" unique-id))
                              (click (lambda (e)
                                       (chain (sel ,(format nil "#wx~a" unique-id))
                                              (load ,(format nil "/link/hide/~a" (hunchentoot:url-encode id))))
                                       (-> (sel ,(format nil "#wx~a" unique-id))
                                           (toggle))
                                       (-> e (prevent-default))))))))))
              (if (link-nsfw link)
                (let ((nsfw-button-id (format nil "nsfw~a" unique-id)))
                  (:button.btn-primary.btn-xs :id nsfw-button-id
                                              :style "margin-top:5px;margin-left:5px;"
                                              :onclick (ps:ps*
                                                        `(progn
                                                           (-> (sel ,(format nil "#link-~a" unique-id))
                                                               (load ,(format nil "/link/body/~a" id)))
                                                           (-> (sel ,(format nil "#~a" nsfw-button-id)) (hide))))
                                              "NSFW"))
                (:script
                  (ps:ps*
                   `(with-document-ready
                        (lambda ()
                          (-> (sel ,(format nil "#link-~a" unique-id))
                              (load ,(format nil "/link/body/~a" id)))
                          (update-graph ,id))))))
              (:div.panel-body :id (format nil "graph~a" id)
                               :style "height:30px;margin-bottom:5px;")
              (:div.panel-body :class "link-body"
                               :style "padding: 10px 15px 15px;"
                               :id (format nil "link-~a" unique-id))
              (:div :class "panel-heading"
                (:div
                  :class "icon"
                  :onclick (ps:ps* `(if (string= (-> (sel ,(format nil "#comments-~a" id))
                                                     (css "display"))
                                                 "none")
                                      (progn
                                        (-> (sel ,(format nil "#comments-~a" id))
                                            (toggle))
                                        (-> (sel ,(format nil "#comments-~a" id))
                                            (load ,(format nil "/comments/~a" id))))
                                      (-> (sel ,(format nil "#comments-~a" id))
                                          (toggle))))
                  
                  (:span :class "s7-download")))
              (:div.panel-body :id (format nil "comments-~a" id)
                               :style "display:none;"
                               "Loading...")))))))))

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

(defun mark-everything-unread ()
  (dolist (subreddit (with-pg (select-dao 'subreddit)))
    (mark-subreddit-as-unread (subreddit-display-name subreddit))))

(defun mark-everything-read ()
  (dolist (subreddit (with-pg (select-dao 'subreddit)))
    (mark-subreddit-as-read (subreddit-display-name subreddit))))

(defroute get-votes ("/votes/:id") ()
  (setf (hunchentoot:content-type*) "application/json")
  (alex:if-let ((up-history (with-pg (get-dao 'up-history id))))
    (with-output-to-string (sink)
      (yason:encode (up-history-scores up-history) sink))
    "[]"))

(defroute display-single-link ("/link/:id") ()
  (with-page ()
    (display-link (with-pg (get-dao 'link id)))))

(defmacro links-page (&rest body)
  (alex:with-gensyms (subreddits)
    `(let ((,subreddits (with-pg (query (:order-by
                                        (:select '* :from 'subreddit)
                                        (:asc 'display_name))
                                       (:dao subreddit)))))
       (with-page ()
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
                 (dolist (subreddit ,subreddits)
                   (with-slots (display-name) subreddit
                     (:a :class "btn btn-primary btn-xs"
                       :href (format nil "/subreddit/~a" display-name)
                       display-name)))))))
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
                                                                      :line-width 1.7)))))))))))))))

(defun display-links (links)
  (links-page
   (:div :class "col-md-12" :id "links"
     (map nil #'display-link links))))


(defroute show-author ("/author/:author") ()
  (display-links (with-pg (select-dao 'link (:= 'author author)))))

(defroute display-subreddit ("/subreddit/:subid") ()
  (display-links
   (with-pg (query (:order-by (:select '* :from 'link
                                :where (:= 'subreddit subid))
                              (:desc 'created-utc))
                   (:dao link)))))

(defroute favorites ("/favorites") ()
  (display-links (with-pg (select-dao 'link (:= 'favorite t)))))

(defroute index ("/") ()
  (hunchentoot:redirect "/links"))

(defroute hide-link ("/link/hide/:id") ()
  (alex:when-let ((link (with-pg (get-dao 'link id))))
    (setf (link-hidden link) t)
    (with-pg (update-dao link)))
  "ok")

(defroute make-favorite ("/link/favorite/:id") ()
  (alex:when-let ((link (with-pg (get-dao 'link id))))
    (if (link-hidden link)
      (progn
        (setf (link-hidden link) nil)
        (setf (link-favorite link) nil)
        (with-pg (update-dao link)))
      (progn
        (setf (link-hidden link) t)
        (setf (link-favorite link) t)
        (with-pg (update-dao link)))))
  "ok")

(defmethod display-comment ((comment comment))
  (with-slots (body) comment
    (with-html
      (:p
        (:span.pull-right
         ("~a" (comment-score comment)))

        (:div.pull-left :style "font-size:10px;" (make-author-link comment))
        
        (:a.pull-left :href (format nil "https://reddit.com/u/~a" (comment-author comment))
          (comment-author comment))
        (:br)
        (:span :style "font-size:14px;"
          (alex:if-let ((html-body (comment-body-html comment)))
            (:raw (html-entities:decode-entities html-body))
            (with-output-to-string (sink)
              (ignore-errors (cl-markdown:markdown body :stream *html*)))))
        (let ((replies (with-pg (select-dao 'comment (:= 'parent (format nil "t1_~a"
                                                                         (comment-id comment)))))))
          (dolist (reply (or replies nil))
            (:div :style "border-left:1px solid #eee;padding-left:10px;"
              (display-comment reply))))))))

(defroute authors-comments ("/comments/author/:author") ()
  (with-page ()
    (dolist (comment (with-pg (select-dao 'comment (:= 'author author))))
      (:div :class "col-md-6 col-md-offset-3" :style "background:white;"
        (display-comment comment)))))

(defmethod update-link-comments ((link link))
  (declare (optimize (debug 3) (speed 1) (safety 3)))
  (labels ((add-or-update-comment (comment-hash)
             (alex:when-let ((db-comment (with-pg (get-dao 'comment (gethash "id" comment-hash)))))
               (with-pg (delete-dao db-comment)))
             (with-pg (insert-dao
                       (make-instance 'comment
                                      :id (gethash "id" comment-hash)
                                      :score (gethash "score" comment-hash)
                                      :parent (gethash "parent_id" comment-hash)
                                      :link-id (gethash "link_id" comment-hash)
                                      :author (gethash "author" comment-hash)
                                      :body-html (or (gethash "body_html" comment-hash)
                                                     (gethash "body_text" comment-hash)
                                                     (gethash "body" comment-hash)))))
             (dolist (reply (ignore-errors (hu:hash-get comment-hash '("replies" "data" "children"))))
               (when (string= "t1" (gethash "kind" reply))
                 (add-or-update-comment (gethash "data" reply))))))

    (multiple-value-bind (data code) (drakma:http-request (format nil "https://www.reddit.com~a.json"
                                                                  (link-permalink link))
                                                          :external-format-in :utf8
                                                          :external-format-out :utf8)
      (alex:switch (code)
        (200 (dolist (comment-hash (yason:parse data))
               (dolist (listing (ignore-errors (hu:hash-get comment-hash '("data" "children"))))
                 (when (string= (gethash "kind" listing) "t1")
                   (add-or-update-comment (gethash "data" listing))))))
        (t (log:info "Fetching comments failed: ~a" code))))))



(defroute show-comments ("/comments/:id") ()

  (update-link-comments (with-pg (get-dao 'link id)))
  ;; (let ((link (car (our-couch-query (list (cons "id" id))))))
  ;;   (comments-handle-link (gethash "permalink" link)))

  (let ((comments (with-pg
                    (query (:order-by (:select '* :from 'comment
                                        :where (:= 'parent (format nil "t3_~a" id)))
                                      (:desc 'score))
                           (:dao comment)))))
    (with-html-string
      (map nil #'display-comment comments)))
  ;; (let ((comments (our-couch-query
  ;;                  (list (cons "type" "comment")
  ;;                        (cons "link_id" (format nil "t3_~a" id)))
  ;;                  :sort (list (alex:alist-hash-table
  ;;                               (list (cons "ups" "desc")))))))
  ;;   (with-html-string (map 'nil #'display-comment comments)))
  )

(defroute show-link-body ("/link/body/:id") ()
  (alex:when-let (link (with-pg (get-dao 'link id)))
    (let ((doc-hash (yason:parse (link-bulk link))))
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
                                      (:script :src "//s.imgur.com/min/embed.js"))))))))))

(defroute make-history ("/graph/history/:id") ()
  (setf (hunchentoot:content-type*) "application/json")
  (alex:if-let ((history (with-pg (get-dao 'up-history id))))
    (with-output-to-string (sink)
      (yason:encode (up-history-scores history) sink))
    "[]"))

(defroute make-graph ("/graph/:id") ()
  (with-html-string
    (:div :class "col-md-3"
      (:div :id (format nil "graph~a" id)))))












(defroute ajax-link ("/singlelink/:id") ()
  (with-html-string (display-link (with-pg (get-dao 'link id)))))

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
                            (add-link (@ env id))
                            (update-graph (@ env id)))
                          (when (string= (@ env action) "update-graph")
                            (update-graph (@ env id)))))))))
          
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
  (hunchensocket:send-text-message (car (hunchensocket:clients room))
                                   (apply #'format nil message args)))

(defun new-articles-to-live ()
  (log:info "Sending unread articles to client.")
  (dolist (link (with-pg (query (:limit (:select '* :from 'link
                                          :where (:= 'hidden nil))
                                        50)
                                (:dao link))))
    (mailbox:post-mail (to-json
                        (alex:alist-hash-table
                         (list (cons "action" "add-link")
                               (cons "id" (link-id link)))))
                       *mailbox*)
    (sleep 0.2))
  (log:info "done"))

(defmethod hunchensocket:client-connected ((room chat-room) user)
  (declare (ignore user))
  (new-articles-to-live))

(defmethod hunchensocket:client-disconnected ((room chat-room) user)
  (broadcast room "~a has left ~a" (user-name user) (chat-room-name room)))

(defmethod hunchensocket:text-message-received ((room chat-room) user message)
  (declare (ignore user message)))

(defvar *ws-server* nil)

(defun start-ws-reader-thread ()
  (bt:make-thread (lambda ()
                    (loop
                      (alex:when-let ((message (mailbox:read-mail *mailbox*)))
                        (ignore-errors (broadcast (car *chat-rooms*) message)))))
                   :name "ws broadcast"))

(defun start-ws-server ()
  (unless *ws-server*
    (setf *ws-server* (make-instance 'hunchensocket:websocket-acceptor :port 8088))
    (hunchentoot:start *ws-server*)))

(defun cleanup ()
  (with-pg
    (let ((subreddits (alex:flatten (query (:select 'display-name :from 'subreddit)))))
      (dolist (link (query (:select 'id 'subreddit :from 'link)))
        (destructuring-bind (id subreddit) link
          (unless (member subreddit subreddits :test #'string=)
            (log:info "~a ~a" id subreddit)
            (delete-dao (get-dao 'link id))))))

    (dolist (comment (select-dao 'comment))
      (unless (get-dao 'link (ppcre:regex-replace "^t3_" (comment-link-id comment) ""))
        (delete-dao comment)))))


(defun test ()
  (declare (optimize (debug 3) (speed 1) (safety 3)))
  (dolist (subreddit (with-pg (select-dao 'subreddit)))
    (log:info "~a" (subreddit-display-name subreddit))
    (dolist (new-link (hash-extract "data"
                                    (get-subreddit-links
                                     *tt*
                                     :newer-than )))
      (handle-possible-new-link new-link))
    (sleep 1)))


(defun download-entire-internet ()
  (dolist (subreddit (with-pg (select-dao 'subreddit)))
    (sync-subreddit subreddit)))


(defun mark-everything-older-than-a-month-as-read ()
  (local-time:unix-to-timestamp (link-created-utc (car (with-pg (query (:select '* :from 'link :where
                                                                         (:>= (- (get-universal-time) (* 30 24 60 60))
                                                                              'created_utc))
                                                                       (:dao link)))))))
