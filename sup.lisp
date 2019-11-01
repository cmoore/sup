;;; -*- mode: Lisp; Syntax: common-lisp; Package: sup; -*-

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
    (defparameter *subreddits-cache* nil)
    (setf yason:*parse-json-booleans-as-symbols* t)
    (setf hunchentoot:*catch-errors-p* t)
    (push (cons "application" "json") drakma:*text-content-types*)))

(defmacro with-pg (&body body)
  `(with-connection (list "reddit" "cmoore" "fl33j0b" "localhost" :pooled-p t)
       ,@body))

(defun update-subreddits-cache ()
  (setf *subreddits-cache* (with-pg (query (:order-by
                                            (:select '* :from 'subreddit)
                                            (:asc 'display_name))
                                           (:dao subreddit))))
  nil)


(defun relabel-links ()
  (with-pg
    (dolist (link-id (with-pg (query (:select 'id :from 'link
                                       :where (:and (:= nil 'shadow))))))
      (let* ((the-link (get-dao 'link (car link-id)))
             (link-json (yason:parse (link-bulk the-link))))
        (alex:when-let ((ltext (gethash "link_flair_text" link-json nil)))
          (when (not (string= (link-label-text the-link) ltext))
            (setf (link-label-text the-link) ltext)
            (alex:when-let ((lb (gethash "link_flair_background_color" link-json nil)))
              (setf (link-label-background-color the-link) lb))
            (alex:when-let ((lc (gethash "link_flair_text_color" link-json nil)))
              (setf (link-label-color the-link) lc))
            (update-dao the-link)))))))



(defparameter *reddit-user* nil)

(defparameter *listener* nil)


(defparameter *mailbox* (sb-concurrency:make-mailbox :name "New articles mailbox"))
(defparameter *fetchbox* (sb-concurrency:make-mailbox :name "Image fetch box"))

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
         :accessor link-nsfw)
   (shadow :initarg :shadow
           :col-type :bool
           :col-default nil
           :accessor link-shadow)
   (store :initarg :static
           :accessor link-store
          :col-type (or db-null text))
   (remotefile :initarg :remotefile
               :accessor link-remote-file
               :col-type (or db-null text))
   (label-text :initarg :label-text
               :accessor link-label-text
               :col-type (or db-null text))
   (label-background-color :initarg :label-background-color
                           :accessor link-label-background-color
                           :col-type (or db-null text))
   (label-color :initarg :label-color
                :accessor link-label-color
                :col-type (or db-null text)))
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


(defclass comment-votes ()
  ((id :initarg :id
       :col-type :text
       :accessor comment-votes-id)
   (scores :initarg :scores
           :col-type :integer[]
           :accessor comment-votes-scores))
  (:metaclass dao-class)
  (:keys id))

(deftable comment-votes
  (!dao-def)
  (!unique-index :id))


(defclass comment ()
  ((id :initarg :id
       :col-type :text
       :accessor comment-id)
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
   (flair-text :initarg :flair-text
               :col-type (or db-null text)
               :accessor comment-flair-text)
   (score :initarg :score
          :col-type integer
          :accessor comment-score)
   (body-html :initarg :body-html
              :col-type :text
              :accessor comment-body-html))
  (:metaclass dao-class)
  (:keys id))

(deftable comment
  (!dao-def)
  (!unique-index :id))


(defclass seen ()
  ((id :initarg :id
       :col-type :text
       :accessor seen-id))
  (:metaclass dao-class)
  (:keys id))

(deftable seen
  (!dao-def)
  (!unique-index :id))


(defun add-seen (id)
  (handler-case (with-pg (insert-dao (make-instance 'seen :id id)))
    (error (condition)
      (declare (ignore condition))
      nil)))

(defun has-seen-p (id)
  (with-pg (get-dao 'seen id)))


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


(defun raw-get-reddit (url &key (retry nil))
  (multiple-value-bind (data code)
      (handler-case
          (drakma:http-request url
                               :additional-headers (list
                                                    (cons "User-Agent" "supyawl:0.2 (by /u/clintm")
                                                    (cons "X-BULL" "Ferdinand")
                                                    (cons "X-BULL-FIGHTS" "No, he's a peaceful bull."))
                               :external-format-in :utf-8
                               :external-format-out :utf-8)
        (sb-bsd-sockets:interrupted-error (condition)
          (log:info "That weird ass interrupted system call error: ~a"
                    condition)
          (raw-get-reddit url))
        (drakma::drakma-simple-error (condition)
          (log:info "~a" condition)
          (raw-get-reddit url))
        (usocket:timeout-error (condition)
          (log:info "TIMED OUT - retrying: ~a" condition)
          (raw-get-reddit url)))
    (alex:switch (code)
      (200 (values data code))
      (503 (values nil code))
      (t (when retry
           (sleep 1)
           (raw-get-reddit url))))))

(defun get-reddit (path &key (retry nil))
  (raw-get-reddit (format nil "https://reddit.com~a" path) :retry retry))

(defun authenticate ()
  (let ((config (yason:parse (alex:read-file-into-string
                              (asdf:system-relative-pathname :sup "config.json")))))
    (handler-case 
        (multiple-value-bind (result code)
            (drakma:http-request "https://www.reddit.com/api/v1/access_token"
                                 :basic-authorization (list *client-id* *client-secret*)
                                 :additional-headers (list (cons "User-Agent" "supyawl:0.2 (by /u/clintm"))
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
                             (json-mop:json-to-clos result 'access-token)))))
      (usocket:ns-host-not-found-error (condition)
        (declare (ignore condition))
        nil))))

(defun start-server (&key (port 8086))
  (unless *listener*
    (setf *listener* (make-instance 'easy-routes:easy-routes-acceptor
                                    :access-log-destination nil
                                    :document-root (asdf:system-relative-pathname
                                                    :sup "static/am")
                                    :port port)))
  (hunchentoot:start *listener*)
  (update-subreddits-cache))

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
  (labels ((reddit-request (path)
             (drakma:http-request (format nil "https://oauth.reddit.com~a" path)
                                  :additional-headers (list
                                                       (cons "User-Agent" "supyawl:0.2 (by /u/clintm")
                                                       (cons "Authorization" (format nil
                                                                                     "bearer ~a"
                                                                                     (access-token-access-token *reddit-user*)))))))
    (authenticate)
    (alex:when-let ((post-data (gethash "data" (yason:parse
                                                (reddit-request "/subreddits/mine")))))
      (hash-extract "data"
                    (gethash "children" post-data)))))

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
            (update-subreddits-cache)
            (log:info "Missing ~a" (gethash "display_name" reddit-hash)))))
      
      ;; Remove db subreddits that aren't currently subscribed.
      (dolist (name db-names)
        (unless (member name reddit-names :test #'string=)
          (let ((db-record (car (remove-if-not #'(lambda (x)
                                                   (string= (subreddit-display-name x) name))
                                               db-subreddits))))
            (log:info "Removing ~a" name)
            (with-pg (delete-dao db-record))
            (update-subreddits-cache)))))
    (log:info "Failed")))

(defun make-history-url (subreddit first-id)
  (format nil "https://www.reddit.com~anew.json?limit=100&before=~a"
          (subreddit-url subreddit) (format nil "t3_~a" first-id)))

(defmethod get-latest-link-for-subreddit ((subreddit subreddit))
  (car (alex:flatten
        (with-pg (query (:limit (:order-by (:select 'id :from 'link
                                             :where (:= 'subreddit (subreddit-display-name subreddit)))
                                           (:asc 'created_utc))
                                1))))))

(defun make-normal-url (subreddit)
  (format nil "~a.json?limit=100" (subreddit-url subreddit)))

(defun make-new-url (subreddit)
  (format nil "~anew/.json?limit=100" (subreddit-url subreddit)))


(defmethod sync-subreddit ((subreddit subreddit))
  "Fetch the entire history of this subreddit."
  (declare (optimize (debug 3) (speed 1) (safety 3)))
  (labels ((make-request-url ()
             (format nil "~a.json" (subreddit-url subreddit)))
           (get-next-page (after-id)
             (let* ((new-url (format nil "~a?show=all&limit=100&after=~a" (make-request-url) after-id))
                    (link-hash (yason:parse (get-reddit new-url))))
               (log:info new-url)
               (dolist (link (hu:hash-get link-hash '("data" "children")))
                 (when (string= (gethash "kind" link) "t3")
                   (handle-possible-new-link (gethash "data" link))))
               (alex:when-let ((next-page (hu:hash-get link-hash '("data" "after"))))
                 (get-next-page next-page)))))
    (get-next-page (hu:hash-get (yason:parse (get-reddit (make-request-url)))
                                '("data" "after")))))

(defgeneric get-subreddit-links (subreddit &key new))

(defmethod get-subreddit-links ((subreddit subreddit) &key new)
  ;;(log:info "~a" (subreddit-display-name subreddit))
  (labels ((safe-parse (blob)
             (handler-case (yason:parse blob)
               (end-of-file (condition)
                 (log:info "END OF FILE?? WTF: ~a" condition)
                 nil))))
    (multiple-value-bind (data code)
        (handler-case
            (let ((path (cond (new (make-new-url subreddit))
                              (t (make-normal-url subreddit)))))
              (get-reddit path))
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
               (string (alex:if-let ((json-data (safe-parse data)))
                         (hu:hash-get json-data '("data" "children"))
                         (log:info "Links fetch was null? ~a ~a" code data)))
               (list (alex:flatten data))))
        (t (log:info "Connecting to reddit failed with: ~a for ~a"
                     code (subreddit-display-name subreddit)))))))

(defun is-repostp (link-hash)
  (< 0 (length (with-pg (select-dao 'link (:and (:!= 'id (gethash "id" link-hash))
                                                (:= 'url (gethash "url" link-hash))))))))

(defmethod send-update-graph ((link link))
  (sb-concurrency:send-message *mailbox*
                               (to-json (alex:alist-hash-table
                                            (list (cons "id" (link-id link))
                                                  (cons "action" "update-graph"))))))

(defun update-all-graphs ()
  (dolist (link (with-pg (select-dao 'link (:= 'hidden nil))))
    (send-update-graph link)))


(defgeneric update-link-comments (link &key refresh))

(defmethod update-link-comments ((link link) &key (refresh t))
  (declare (optimize (debug 3) (speed 1) (safety 3)))
  
  (unless (and (< 0 (length (with-pg (query (:limit (:select 'id :from 'comment
                                                      :where (:= 'parent (format nil "t3_~a" (link-id link))))
                                                    1)))))
               (not refresh))
    (multiple-value-bind (data code)
        (handler-case (get-reddit (format nil "~acomments.json" (link-permalink link)) :retry nil)
          (flexi-streams:external-format-encoding-error (condition)
            (log:info "ENCODING ERROR: ~a" condition)
            (values 201 "NOTHING")))
      (alex:switch (code)
        (200 (dolist (comment-hash (yason:parse data))
               (dolist (listing (ignore-errors (hu:hash-get comment-hash '("data" "children"))))
                 (when (string= (gethash "kind" listing) "t1")
                   (add-or-update-comment (gethash "data" listing))))))
        (t nil)))))

(defmethod update-comment-votes ((comment comment)
                                 (id string)
                                 (count integer))
  (alex:if-let ((maybe-votes (with-pg (get-dao 'comment-votes id))))
    (progn
      (let ((votes (make-array (length (comment-votes-scores maybe-votes))
                               :initial-contents (comment-votes-scores maybe-votes)
                               :adjustable t
                               :fill-pointer t)))
        (vector-push-extend count votes)
        (setf (comment-votes-scores maybe-votes) votes)
        (with-pg (update-dao maybe-votes))))
    (with-pg (insert-dao (make-instance 'comment-votes
                                        :id id
                                        :scores (vector count))))))


(defun handle-possible-new-link (link-hash &key (update t))
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

    (alex:if-let ((existing-link (with-pg
                                   (get-dao 'link
                                            (gethash "id" link-hash)))))
      (progn
        (when update
          (alex:when-let ((ltext (gethash "link_flair_text" link-hash nil)))
            (when (not (string= ltext
                                (link-label-text existing-link)))
              (setf (link-label-text existing-link) ltext)
              (alex:when-let ((lb (gethash "link_flair_background_color" link-hash nil)))
                (setf (link-label-background-color existing-link) lb))
              (alex:when-let ((lc (gethash "link_flair_text_color" link-hash nil)))
                (setf (link-label-color existing-link) lc))
              (unless (link-hidden existing-link)
                (sb-concurrency:send-message *mailbox*
                                             (to-json
                                              (alex:alist-hash-table
                                               (list (cons "id" (gethash "id" link-hash))
                                                     (cons "action" "update-link")))))))
            (with-pg (update-dao existing-link)))
          (when (link-hidden existing-link)
            (return-from handle-possible-new-link))
          (update-like-cache))
        (sb-concurrency:send-message *mailbox*
                                     (to-json
                                      (alex:alist-hash-table
                                       (list (cons "id" (gethash "id" link-hash))
                                             (cons "action" "update-graph"))))))
      
      (unless (has-seen-p (gethash "id" link-hash))
        (log:info "~a ~a"
                  (gethash "subreddit" link-hash)
                  (gethash "title" link-hash))
        (let ((new-link (make-instance 'link
                                       :id (gethash "id" link-hash)
                                       :hidden nil
                                       :written (get-universal-time)
                                       :created-utc (gethash "created_utc" link-hash)
                                       :url (gethash "url" link-hash)
                                       :permalink (gethash "permalink" link-hash)
                                       :author (gethash "author" link-hash)
                                       :subreddit-id (gethash "subreddit_id" link-hash)
                                       :media-only (equal 'yason:true (gethash "media_only" link-hash))
                                       :score (gethash "score" link-hash)
                                       :title (gethash "title" link-hash)
                                       :nsfw (equal 'yason:true (gethash "over_18" link-hash))
                                       :author-fullname (gethash "author_fullname" link-hash)
                                       :selftext (gethash "selftext" link-hash)
                                       :subreddit (gethash "subreddit" link-hash)
                                       :bulk (to-json link-hash)
                                       :body (gethash "selftext_html" link-hash)
                                       :label-text (gethash "label_flair_text" link-hash nil)
                                                    
                                       :label-background-color (gethash "link_flair_background_color" link-hash nil)
                                       :label-color (gethash "link_flair_text_color"
                                                             link-hash nil))))
          (add-seen (gethash "id" link-hash))
          (with-pg (insert-dao new-link))
          (when update
            (update-like-cache))
          (sb-concurrency:send-message *mailbox*
                                       (to-json (alex:alist-hash-table
                                                 (list (cons "id" (link-id new-link))
                                                       (cons "action" "add-link")))))
          (send-update-graph new-link))))))


(defmethod update-subreddit ((subreddit subreddit))
  (let ((yason:*parse-json-booleans-as-symbols* nil))
    (unless (ppcre:scan "u_" (subreddit-display-name subreddit))
      (dolist (link (hash-extract "data" (get-subreddit-links subreddit)))
        (handle-possible-new-link link))
      (dolist (link (hash-extract "data" (get-subreddit-links subreddit :new t)))
        (handle-possible-new-link link :update nil))
      )))

(defun scan-subreddits ()
  (dolist (subreddit (with-pg (select-dao 'subreddit)))
    (update-subreddit subreddit)))

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

(defgeneric display-link (link &key public))

(defmethod display-link ((link link) &key (public))
  (with-slots (id title url author subreddit created-utc permalink) link
    (with-html
      (let ((unique-id (format nil "~a" (link-id link))))
        (:div :class "col-md-4" :id (format nil "wx~a" (link-id link))
              (:div.panel.panel-default.panel-borders.panel-heading-fullwidth
               :style "border:1px solid rgb(7,7,7);"
               (:div :class "panel-heading" :style "font-size:14px;padding: 15px 15px 10px;"
                     (unless public
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
                        (:a :style "font-weight:bold;"
                            :target (format nil "win-~a" unique-id)
                            :href (format nil "/link/~a" id)
                            (:div.icon (:span.s7-id)))
                
                        (:div.icon (:a :id (format nil "shadow-~a" unique-id)
                                       (:i :class "s7-trash")))
                        (:script
                         (ps:ps*
                          `(-> (sel ,(format nil "#shadow-~a" unique-id))
                               (click (lambda (e)
                                        (-> (sel ,(format nil "#wx~a" unique-id)) (toggle))
                                        (-> (sel ,(format nil "#wx~a" unique-id)) (load ,(format nil "/shadow/~a" id)))
                                        (-> e (prevent-default)))))))
                
                        (:div.icon :id (format nil "favorite-~a" unique-id)
                                   (:span.s7-download))
                        (:div.icon :id (format nil "hide-~a" unique-id)
                                   (:span.s7-close-circle))
                        (when (and (link-label-text link)
                                   (not (eq :null (link-label-text link)))
                                   (not (string= "false" (link-label-text link)))
                                   (not (eq :db-null (link-label-text link))))
                          (:br)
                          (:span :class "label pull-right"
                                 :style (format nil "font-size:8px;background-color:~a;color:~a"
                                                (link-label-background-color link)
                                                "white")
                                 (ppcre:regex-replace-all "&amp;"
                                                          (link-label-text link)
                                                          "&")))))
                     (:a :target (format nil "win-~a" (uuid:make-v4-uuid))
                         :href url (html-entities:decode-entities title))
                     (:br)
                     (:div
                      (:a :target (format nil "win-~a" (uuid:make-v4-uuid))
                          :href (format nil "https://www.reddit.com~a" permalink)
                          (format nil "~a in ~a on ~a"
                                  author
                                  subreddit
                                  (with-output-to-string (sink)
                                    (local-time:format-timestring sink
                                                                  (local-time:unix-to-timestamp
                                                                   (rationalize created-utc))
                                                                  :format (list :month "/" :day "/" :year " " :hour ":" :min ":" :sec))))))
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
                                           (let ((selector ,(format nil "#wx~a" unique-id)))
                                             (chain (sel selector)
                                                    (load ,(format nil "/link/hide/~a" (hunchentoot:url-encode id))))
                                             (-> (sel selector) (remove))
                                             (-> e (prevent-default)))))))))))
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
                      :class "icon comment-trigger"
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
                                "Loading...")))))))

(defun fetcher-is-running-p ()
  (member "fetchers" (mapcar #'bt:thread-name
                             (bt:all-threads))
          :test #'string=))

(defun mark-everything-unread ()
  (dolist (subreddit (with-pg (select-dao 'subreddit)))
    (mark-subreddit-as-unread (subreddit-display-name subreddit))))

(defun mark-everything-read ()
  (dolist (subreddit (with-pg (select-dao 'subreddit)))
    (mark-subreddit-as-read subreddit)))

(defparameter *display-offset* nil)

(defmacro links-page (&rest body)
  `(with-page ()
     (:div :class "row"
       (:div :class "col-md-12"
         (:form :method :post :action "/search"
           (:div :class "col-md-3"
             (:div :class "btn-group btn-space"
               (:a :class "btn btn-default btn-xs"
                 :type "button" :href "/favorites" "Favorites")
               (:a :class "btn btn-default btn-xs" :type "button" :href "/live" "Live")
               (:a.btn.btn-default.btn-xs :type "button" :href "/mine" :target "mineminemine" "Mine")
               
               (:a :class "btn btn-default btn-xs" :id "hidebutton" :type "button" :href "#" "C")
               (:script (ps:ps (-> (sel "#hidebutton")
                                   (click (lambda (e)
                                            (-> (sel ".link-body") (hide))
                                            (-> e (prevent-default)))))))
               
               (:a :class "btn btn-default btn-xs" :id "expandcomments" :type "button" :href "#" "E")
               (:script (ps:ps (-> (sel "#expandcomments")
                                   (click (lambda (e)
                                            (-> (sel ".comment-trigger") (click))
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
             (dolist (subreddit *subreddits-cache*)
               (with-slots (display-name id) subreddit
                 (:a
                   :target (format nil "wx~a" id)
                   :class "btn btn-primary btn-xs"
                   :href (format nil "/subreddit/~a/0" display-name)
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
                                                                  :line-width 1.7)))))))))))))

(defun display-links (links)
  (let* ((subreddit (cadr (ppcre:split "/" (format nil "~a" (hunchentoot:request-pathname)))))
         (next-page-link (format nil "/subreddit/~a/~a" subreddit *display-offset*)))
    (links-page
     (:div :class "row"
       (:div.col-md-2
        (:a :href next-page-link
          (:button :class "btn-primary btn-sm" "Next"))))
     (:div :class "row"
       (dolist (link links)
         (display-link link)))
     (:div :class "row"
       (:div.col-md-2
        (:a :href next-page-link
          (:button :class "btn-primary btn-sm" "Next")))))))

(defmethod display-comment ((comment comment))
  (with-slots (id body flair-text) comment
    (with-html
      (:p
        (:span.pull-right ("~a" (comment-score comment)))
        (:div.pull-left :style "font-size:10px;" (make-author-link comment))
        (:span.pull-left
         (:a.pull-left :href (format nil "https://reddit.com/u/~a" (comment-author comment))
                       (comment-author comment))
         (unless (string= flair-text "false")
           (:span.text-warning :style "padding-left:4px;padding-right:4px;margin-left:5px;background-color:#444444;"
                               (comment-flair-text comment))))
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

(defun add-or-update-comment (comment-hash)
  (alex:when-let ((db-comment (with-pg (get-dao 'comment (gethash "id" comment-hash)))))
    (with-pg (delete-dao db-comment)))
  (let ((new-comment (make-instance 'comment
                                    :id (gethash "id" comment-hash)
                                    :parent (gethash "parent_id" comment-hash)
                                    :score (gethash "score" comment-hash)
                                    :link-id (gethash "link_id" comment-hash)
                                    :flair-text (gethash "author_flair_text" comment-hash)
                                    :author (gethash "author" comment-hash)
                                    :body-html (or (gethash "body_html" comment-hash)
                                                   (gethash "body_text" comment-hash)
                                                   (gethash "body" comment-hash)))))
    (with-pg (insert-dao new-comment))
    (update-comment-votes new-comment
                          (gethash "id" comment-hash)
                          (gethash "score" comment-hash)))
  (dolist (reply (ignore-errors (hu:hash-get comment-hash '("replies" "data" "children"))))
    (when (string= "t1" (gethash "kind" reply))
      (add-or-update-comment (gethash "data" reply)))))

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
  (find (hunchentoot:script-name request) *chat-rooms* :test #'string= :key #'chat-room-name))

(pushnew 'find-room hunchensocket:*websocket-dispatch-table*)

(defgeneric send-message (client message &rest args))

(defmethod send-message ((user user) (message string) &rest args)
  (handler-case
      (hunchensocket:send-text-message user (apply #'format nil message args))
    (error (condition)
      (log:info "ERROR sending websocket message: ~a" condition)
      nil)))

(defun broadcast (room message &rest args)
  (dolist (client (hunchensocket:clients room))
    (send-message client message args)))

(defun new-articles-to-live ()
  (dolist (link (with-pg (query (:limit (:order-by
                                         (:select '* :from 'link :where (:= 'hidden nil))
                                         (:asc 'created_utc)) 200)
                                (:dao link))))
    (sb-concurrency:send-message *mailbox*
                                 (to-json
                                  (alex:alist-hash-table
                                   (list (cons "action" "add-link")
                                         (cons "id" (link-id link))))))))

(defmethod hunchensocket:client-connected ((room chat-room) user)
  (declare (ignore user))
  (new-articles-to-live))

(defmethod hunchensocket:client-disconnected ((room chat-room) user)
  nil)

(defmethod hunchensocket:text-message-received ((room chat-room) user message)
  (declare (ignore user message)))

(defvar *ws-server* nil)

(defun start-ws-reader-thread ()
  (bt:make-thread (lambda ()
                    (loop
                      (alex:if-let ((message (sb-concurrency:receive-message *mailbox*)))
                        (handler-case (broadcast (car *chat-rooms*) message)
                          (error (condition)
                            (log:info "Error broadcasting message: ~a" condition)))
                        (sleep 1))))
                  :name "ws broadcast"))

(defun start-ws-server ()
  (unless *ws-server*
    (setf *ws-server* (make-instance 'hunchensocket:websocket-acceptor :port 8088))
    (hunchentoot:start *ws-server*)))

(defun cleanup ()
  (labels ((remove-comment-by-parent (parent)
             (dolist (comment-id (alex:flatten (with-pg (query (:select 'id :from 'comment
                                                                 :where (:= 'parent parent))))))
               (remove-comment-by-parent (format nil "t1_~a" comment-id))
               (with-pg (execute "delete from comment where id = $1"
                                 comment-id)))))
    (let ((subreddits (alex:flatten (with-pg (query (:select 'display-name :from 'subreddit))))))
      (dolist (link (with-pg (query (:select 'id 'subreddit :from 'link))))
        (destructuring-bind (id subreddit) link
          (unless (member subreddit subreddits :test #'string=)
            (with-pg (delete-dao (get-dao 'link id)))))))

    
    (dolist (comment-parent (alex:flatten (with-pg (query (:select 'parent :from 'comment)))))
      (when (ppcre:scan "^t3_" comment-parent)
        (unless (car (alex:flatten
                      (with-pg (query (:select 'id :from 'link
                                        :where (:= 'id (ppcre:regex-replace "^t3_" comment-parent "")))))))
          (remove-comment-by-parent comment-parent))))
    
    (dolist (vote-id (alex:flatten (with-pg (query (:select 'id :from 'up-history)))))
      (unless (car (with-pg (query (:select 'id :from 'link :where (:= 'id vote-id)))))
        (with-pg (delete-dao (get-dao 'up-history vote-id)))))

    (dolist (vote-info (with-pg (query (:select 'id 'scores :from 'up_history))))
      (destructuring-bind (id votes) vote-info
        (when (< 50 (length votes))
          (let ((new-scores (subseq votes (- (length votes) 50))))
            (with-pg (execute "update up_history set scores = $1 where id = $2"
                              new-scores id))))))))

(defun download-entire-internet ()
  (dolist (subreddit (with-pg (select-dao 'subreddit)))
    (sync-subreddit subreddit)))

(defun mark-everything-older-than-a-month-as-read ()
  (dolist (linkid (alex:flatten
                   (with-pg (query (:order-by (:select 'id :from 'link :where
                                                (:and (:= 'hidden nil)
                                                      (:>= (- (cl-ivy:epoch-time) (* 2 24 60 60))
                                                           'created_utc)))
                                              (:desc 'created_utc))))))
    (let ((the-link (with-pg (get-dao 'link linkid))))
      (setf (link-hidden the-link) t)
      (with-pg (update-dao the-link)))))

(defun start-comment-update-thread ()
  (bt:make-thread (lambda ()
                    (get-all-the-damned-comments))
                  :name "comment fetcher"))



(defroute stop-threads ("/stop-fetchers") ()
  (stop-refresh-threads)
  (hunchentoot:redirect "/live"))

(defroute start-threads ("/start-fetchers") ()
  (start-refresh-threads)
  (hunchentoot:redirect "/live"))

(defroute get-votes ("/votes/:id") ()
  (setf (hunchentoot:content-type*) "application/json")
  (alex:if-let ((up-history (with-pg (get-dao 'up-history id))))
    (with-output-to-string (sink)
      (yason:encode (up-history-scores up-history) sink))
    "[]"))

(defroute show-author ("/author/:author") ()
  (display-links (with-pg (query (:order-by
                                  (:select '* :from 'link :where (:= 'author author))
                                  (:desc 'created-utc))
                                 (:dao link)))))

(defroute display-single-link ("/link/:id") ()
  (with-page ()
    (display-link (with-pg (get-dao 'link id)) :public t)))

(defroute display-subreddit ("/subreddit/:subid/:offset" :method :get) ()
  (let* ((offset-i (parse-integer offset))
         (*display-offset* (+ offset-i 1000)))
    (display-links
     (with-pg (query (:order-by (:limit (:select '* :from 'link
                                          :where (:and (:= 'subreddit subid)
                                                       (:= 'shadow nil)))
                                        1000 offset-i)
                                (:desc 'created-utc))
                     (:dao link))))))

(defroute favorites ("/favorites") ()
  (display-links (with-pg (query (:order-by (:select '* :from 'link
                                              :where (:= 'favorite t))
                                            (:desc 'created_utc))
                                 (:dao link)))))

(defroute shadow-link ("/shadow/:id") ()
  (alex:when-let ((link (with-pg (get-dao 'link id))))
    (setf (link-shadow link) t)
    (setf (link-hidden link) t)
    (with-pg (update-dao link))
    (with-html-string (:div "ok"))))

(defroute index ("/") ()
  (hunchentoot:redirect "/live"))

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

(defroute show-comments ("/comments/:id") ()
  (update-link-comments (with-pg (get-dao 'link id)))
  (alex:if-let ((comments (with-pg
                    (query (:order-by (:select '* :from 'comment
                                        :where (:= 'parent (format nil "t3_~a" id)))
                                      (:desc 'score))
                           (:dao comment)))))
    (with-html-string
      (dolist (comment comments)
        (display-comment comment)))
    (with-html-string
      (:div "No comments yet."))))

(defroute show-link-body ("/link/body/:id") ()
  (labels (
           ;; (drop-leading-spaces (string)
           ;;   (ppcre:regex-replace "^\\s+" string ""))
           ;; (drop-trailing-spaces (string)
           ;;   (ppcre:regex-replace "\\s+$" string ""))
           ;; (trim-spaces (string)
           ;;   (drop-leading-spaces (drop-trailing-spaces string)))
           (has-image-suffix (string)
             (ppcre:scan ".jpg|.png$|.gif$|.JPG$|.PNG$|.GIF$" string))
           (render-link (link doc-hash)
             (with-html-string
               (:div.row
                (:div.col-md-12
                 (let ((crossposted-reddit-video
                         (hash-get doc-hash
                                   '("crosspost_parent_list" 0 "secure_media" "reddit_video" "fallback_url")))
                       
                       (crossposted-media-content
                         (hash-get doc-hash '("crosspost_parent_list" 0 "media_embed" "content")))
                       
                       (reddit-preview-of-imgur-gif
                         (hash-get doc-hash '("preview" "reddit_video_preview" "fallback_url")))
                       
                       (is-reddit-video
                         (hash-get doc-hash '("secure_media" "reddit_video" "fallback_url")))

                       (url-is-mp4 (ppcre:scan ".mp4" (gethash "url" doc-hash)))
                       
                       (is-imgur-gifv (ppcre:scan ".gifv$" (gethash "url" doc-hash)))
                       
                       (is-embedded-image (hash-get doc-hash '("preview" "images")))
                       
                       (has-selftext (gethash "selftext" doc-hash nil))
                       
                       (has-selftext-html (gethash "selftext_html" doc-hash nil))
                       
                       (has-oembed-media (hash-get doc-hash
                                                   '("secure_media" "oembed" "html")))
                       
                       (url-is-imgur-image (alex:when-let ((url (gethash "url" doc-hash)))
                                             (when (ppcre:scan "imgur.com" url)
                                               (ppcre:regex-replace "https?://i?.?imgur.com/" url ""))))
                       
                       (url-is-video (alex:when-let ((url (gethash "url" doc-hash)))
                                       (when (ppcre:scan ".mp4|.MP4" url)
                                         url)))
                       
                       (url-is-image (alex:when-let ((url (gethash "url" doc-hash)))
                                       (when (has-image-suffix url)
                                         url)))
                       
                       (has-crosspost-parent-media (let ((crosspost-parent-list
                                                           (hash-get doc-hash '("crosspost_parent_list"))))
                                                     (when (listp crosspost-parent-list)
                                                       (hash-get (car crosspost-parent-list)
                                                                 '("preview" "reddit_video_preview" "fallback_url"))))))
                   
                   (cond

                         
                     (crossposted-reddit-video (link-add-to-queue link crossposted-reddit-video)
                                               (:video :preload "auto" :class "img-responsive" :controls 1
                                                 (:source :src crossposted-reddit-video))
                                               (:div "crossposted-reddit-video"))
                         
                     (reddit-preview-of-imgur-gif (link-add-to-queue link reddit-preview-of-imgur-gif)
                                                  (:video :preload "auto" :class "img-responsive" :controls 1
                                                    (:source :src reddit-preview-of-imgur-gif))
                                                  (:div "reddit-preview-of-imgur-gif"))
                         
                     (has-oembed-media (:raw (html-entities:decode-entities has-oembed-media))
                                       (:div "has-oembed-media"))
                     
                     (url-is-mp4 (link-add-to-queue link (gethash "url" doc-hash)) 
                                 (:video :preload "auto" :class "img-responsive" :controls "1"
                                   (:source :src (gethash "url" doc-hash)))
                                 (:div "url-is-mp4"))
                     
                     (is-imgur-gifv (let ((thing (ppcre:regex-replace ".gifv" (gethash "url" doc-hash) ".mp4")))
                                      (link-add-to-queue link thing)
                                      (:video :preload "auto" :class "img-responsive" :controls "1"
                                        (:source :src thing))
                                      (:div "is-imgur-gifv")))
                     (is-reddit-video (link-add-to-queue link is-reddit-video)
                                      (:video :preload "auto" :class "img-responsive" :controls 1
                                        (:source :src is-reddit-video))
                                      (:div "is-reddit-video"))
                         
                     ;; Matches if the post is a crosspost and the original
                     ;; has a video hosted at reddit
                     (crossposted-media-content (:raw (html-entities:decode-entities
                                                       crossposted-media-content))
                                                (:div "crossposted-media-content"))
                         
                     (has-crosspost-parent-media (:video :preload "auto" :class "img-responsive" :controls 1
                                                   (:source :src has-crosspost-parent-media)))
                         
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
                                                   (cond (has-mp4 (link-add-to-queue link has-mp4)
                                                                  (:video :preload "auto" :class "img-responsive"
                                                                    :controls 1
                                                                    (:source :src (html-entities:decode-entities has-mp4))))
                                                         (has-gif (link-add-to-queue link has-gif)
                                                                  (:img :class "img-responsive"
                                                                    :src (html-entities:decode-entities has-gif)))
                                                         (has-still-image (link-add-to-queue link has-still-image)
                                                                          (:img :class "img-responsive"
                                                                            :src (html-entities:decode-entities has-still-image))))))))
                                        (:div "is-embedded-image"))

                     (url-is-imgur-image (:blockquote.imgur-embed-pub :data-id url-is-imgur-image)
                                         (:script :src "//s.imgur.com/min/embed.js")
                                         (:div "url-is-imgur-image"))

                         
                     (url-is-video (let ((video (html-entities:decode-entities url-is-video)))
                                     (link-add-to-queue link video)
                                     (:video :class "img-responsive" :preload "auto" :controls "1"
                                       (:source :src (html-entities:decode-entities url-is-video)))
                                     (:div "url-is-video")))
                     (url-is-image (link-add-to-queue link url-is-image)
                                   (:img.img-responsive :src url-is-image)
                                   (:div "url-is-image"))
                         
                         
                         
                     (has-selftext-html (:raw
                                         (with-output-to-string (sink)
                                           (let ((decoded (html-entities:decode-entities has-selftext-html)))
                                             (handler-case (markdown:markdown decoded :stream sink)
                                               (error (condition)
                                                 (declare (ignore condition))
                                                 (format sink "~a" decoded)))))))
                         
                     (has-selftext (:raw (html-entities:decode-entities has-selftext))
                                   (alex:when-let ((the-url (gethash "url" doc-hash nil)))
                                     (:a :target (format nil "~a" (get-universal-time)) :href the-url the-url))
                                   (:div "selftext"))
                         
                         
                         
                     (t (:div "What the hell is this?")))))))))

    (alex:when-let (link (with-pg (get-dao 'link id)))
      (let ((doc-hash (yason:parse (link-bulk link))))
        (alex:if-let ((has-crosspost-parent (car (gethash "crosspost_parent_list" doc-hash nil))))
          (render-link link has-crosspost-parent)
          (render-link link doc-hash))))))

(defroute comment-history ("/graph/comment/history/:id") ()
  (setf (hunchentoot:content-type*) "application/json")
  (alex:if-let ((history (with-pg (get-dao 'comment-votes id))))
    (with-output-to-string (sink)
      (yason:encode (comment-votes-scores history) sink))
    "[]"))

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

(defroute authors-comments ("/comments/author/:author") ()
  (with-page ()
    (dolist (comment (with-pg (select-dao 'comment (:= 'author author))))
      (:div :class "col-md-6 col-md-offset-3" :style "padding-top:10px;padding-bottom:10px;margin-bottom:10px;background:white;"
        (display-comment comment)))))

(defroute mine ("/mine") ()
  (display-links
   (sort (mapcar (lambda (realid)
                   (with-pg (get-dao 'link (ppcre:regex-replace "^t3_" realid ""))))
                 (remove-duplicates
                  (remove-if-not (lambda (thing)
                                   (ppcre:scan "^t3_" thing))
                                 (alex:flatten (with-pg (query (:select 'parent :from 'comment :where (:= 'author "clintm"))))))))
         #'> :key #'link-created-utc)))

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
            (setf ws (new (-web-socket "ws://hypatia.ivy.io/ws/news")))
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
                          (when (string= (@ env action) "update-link")
                            (let ((the-id (@ env id) "id"))
                              (-> (sel (+ "#wx" the-id)) (remove))
                              (add-link (@ env id))))
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

(defun get-links-by-text (searchtext)
  (with-pg
    (query (:select '* :from 'link
             :where (:and (:= 'shadow nil)
                          (:raw (format nil "to_tsvector('english',title) @@ to_tsquery('english','~a')"
                                        searchtext))))
           (:dao link))))

(defroute search-h ("/search" :method :post) (pattern)
  (log:info "Searching... ~a" pattern)
  (links-page
   (dolist (link (get-links-by-text pattern))
     (display-link link))))


(defparameter *test-mailbox* (sb-concurrency:make-mailbox :name "testing"))

(defun start-test-worker (name)
  (bt:make-thread (lambda ()
                    (loop
                      (let ((thing (sb-concurrency:receive-message *test-mailbox*)))
                        (log:info "~a got ~a" name thing)
                        (sleep 1))))
                  :name (format nil "~a" name)))

(defun test-inject-messages ()
  (dotimes (count 50)
    (sb-concurrency:send-message *test-mailbox* (format nil "~a" count))))


(defgeneric link-add-to-queue (link url))

(defmethod link-add-to-queue ((link link) (url string))
  ;;(sb-concurrency:send-message *fetchbox* (list (link-id link) url))
  )



(defun start-link-cache-worker ()
  (loop
    (destructuring-bind (id url) (sb-concurrency:receive-message *fetchbox*)
      (let ((link (with-pg (get-dao 'link id))))
        (setf (link-remote-file link) url)
        (link-create-cache link)))))


(defmethod convert-remote-file ((link link))
  (let ((oori (puri:parse-uri (link-remote-file link))))
    (when (or (ppcre:scan "preview.redd.it" (puri:uri-host oori)))
      (setf (puri:uri-host oori) "i.redd.it")
      (setf (puri:uri-query oori) nil)
      (let ((new-url (with-output-to-string (sink)
                       (puri:render-uri oori sink))))
        ;; (log:info "Was: ~a Now: ~a"
        ;;           (link-remote-file link)
        ;;           new-url)
        (setf (link-remote-file link) new-url)
        (with-pg (update-dao link))))))



(defmethod save-to-local-file ((link link))
  (let* ((uri (puri:parse-uri (link-remote-file link)))
         (filename (ppcre:regex-replace "^/" (puri:uri-path uri) ""))
         (full-pathname (asdf:system-relative-pathname :sup
                                                       (format nil "static/store/~a"
                                                               filename))))
    (unless (probe-file full-pathname)
      (let ((image-data (raw-get-reddit (link-remote-file link))))
        (when image-data
          (handler-case (progn
                          (alex:write-byte-vector-into-file image-data full-pathname)
                          (setf (link-store link) (format nil "/store/~a" filename))
                          (log:info "saved"))
            (sb-int:simple-file-error (condition)
              (log:info "Caught ~a" condition)
              nil)))))))


(defgeneric link-create-cache (link))
(defmethod link-create-cache ((link link))
  (convert-remote-file link)
  (save-to-local-file link)
  (with-pg (update-dao link)))

(defroute testing ("/staticles") ()
  (let ((links (with-pg (query
                         (:select 'id 'remotefile :from 'link :where (:and (:not-null 'remotefile)
                                                                           (:= 'shadow nil)))))))
    (links-page
     (dolist (link links)
       (destructuring-bind (link-id link-remote) link
         (:div :class "col-md-2"
           (:a :target "whadda" :href (format nil "/link/~a" link-id)
             (:img.img-responsive :src (fixit link-remote)))))))))
