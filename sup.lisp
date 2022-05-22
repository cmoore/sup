
;;
;; 🆑 or gtfo
;;

(defpackage :sup
  (:use #:cl
        #:cl-ivy
        #:easy-routes
        #:spinneret
        #:cl-hash-util
        #:postmodern)
  (:import-from :alexandria #:when-let #:when-let*
                #:if-let #:read-file-into-string
                #:switch #:flatten
                #:alist-hash-table)
  (:import-from :parenscript #:defpsmacro)
  (:export #:interface-main
           #:start))

(in-package :sup)

(defvar *config* nil)

(defun write-config (filename)
  (with-open-file (out filename
                       :direction :output
                       :if-exists :supersede)
    (print *config* out)))

(defun read-config (&key (filename (asdf:system-relative-pathname "sup" "config.sexp")))
  (setf *config*
        (with-open-file (in filename :direction :input)
          (let ((*read-eval* nil)
                (*package* #.*package*))
            (read in nil)))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (local-time:reread-timezone-repository)
  (setf local-time:*default-timezone*
        (local-time:find-timezone-by-location-name "America/Los_Angeles"))
  (setf hunchentoot:*catch-errors-p* t)
  (push (cons "application" "json") drakma:*text-content-types*))


(defmacro with-time-info (name &rest body)
  (alexandria:with-gensyms (time return-value)
    `(let ((,time (get-universal-time))
           (,return-value (progn ,@body)))
       (log:info "~a took: ~a seconds" ,name (- (get-universal-time) ,time))
       ,return-value)))


(defmacro with-pg (&body body)
  `(destructuring-bind (&key database-name database-login
                          database-password database-host &allow-other-keys)
       *config*
     (with-connection (list database-name database-login database-password database-host :pooled-p t)
       ,@body)))

(defun update-subreddits-cache ()
  (setf *subreddits-cache* (with-pg (query (:order-by
                                            (:select '* :from 'subreddit)
                                            (:asc 'display_name))
                                           (:dao subreddit))))
  nil)

(defun clm-show-data-vars ()
  (let ((package (find-package *package*))
        data-symbols)
    (do-all-symbols (symbol (find-package package))
      (when (and (eql (symbol-package symbol) package)
                 (boundp symbol))
        (push symbol data-symbols)))
    data-symbols))


(defvar *subreddits-cache* nil)

(defvar *reddit-access-token* nil)

(defvar *listener* nil)

(defvar *interface-queue* (lparallel.queue:make-queue :fixed-capacity 10000))


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
            :col-type :numeric
            :accessor link-written)
   (created-utc :initarg :created-utc
                :col-type :numeric
                :accessor link-created-utc)
   (url :initarg :url
        :col-type :text
        :accessor link-url)
   (permalink :initarg :permalink
              :col-type :text
              :accessor link-permalink)
   (author :initarg :author
           :col-type :text
           :accessor link-author)
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
   (ts-index :col-type db-null
             :default nil))
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
              :accessor comment-body-html)
   (bulk :initarg :bulk
         :col-type (or db-null text)
         :accessor comment-bulk))
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

(defclass media ()
  ((id :col-type :serial
       :accessor media-id)
   (link-id :col-type :text
            :accessor media-link-id
            :initarg :link-id)
   (url :col-type :text
        :initarg :url
        :accessor media-url)
   (hash :col-type :text
         :initarg :hash
         :accessor media-hash))
  (:metaclass dao-class)
  (:keys id))

(deftable media
  (!dao-def)
  (!unique-index :url))


(defun add-seen (id)
  (handler-case (with-pg (insert-dao (make-instance 'seen :id id)))
    (error (condition)
      (log:info condition))))

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

(defun raw-http-request (url)
  (let ((tries 0))
    (block retry-it
      (tagbody retry
         (when (> tries 10)
           (log:info "RETRIED TOO MUCH!  BAILING!")
           (return-from raw-http-request nil))
         (setf tries (+ tries 1))
         (handler-bind ((error #'(lambda (condition)
                                   (declare (ignore condition))
                                   (invoke-restart (find-restart 'retry)))))
           (restart-case
               (return-from retry-it
                 (multiple-value-bind (data code)
                     (drakma:http-request url
                                          :additional-headers
                                          '(("User-Agent" . "supyawl:0.2 (by /u/clintm")
                                            ("X-BULL" . "Ferdinand")
                                            ("X-BULL-FIGHTS" . "No, he's a peaceful bull."))
                                          :external-format-in :utf-8
                                          :external-format-out :utf-8)
                   (switch (code)
                     (200 (values data code))
                     (503 (values nil code))
                     (t (progn
                          (log:warn "Result code: ~a" code)
                          (values nil nil))))))
             (retry ()
               (sleep 1)
               (log:warn "Retrying: ~a" url)
               (go retry))))))))

(defun get-reddit (path)
  (log:debug path)
  (raw-http-request (format nil "https://reddit.com~a" path)))

(defmacro destructure-jsown-object (bind-list object &body body)
  "Ala DESTRUCTURING-BIND, decompose a JSOWN object.

  Binds the symbols in BIND-LIST inside BODY to their respective values in the JSOWN object.

  BIND-LIST is a list of either a symbol, a symbol and string, or a
symbol a string and T where the string is the name of the key in the
JSOWN object.  Passing T as the third option indicates that the value
can be NIL (ie. the value is picked out using JSOWN:VAL-SAFE).
Otherwise, JSOWN:VAL is used and will signal an error if a key is not
found.

  OBJECT is a regular JSOWN :OBJ
"

  ;; (destructure-jsown-object ((first-name "first_name")
  ;;                            (last-name "last_name" :safe)
  ;;                            date)
  ;;     '(:obj
  ;;       ("first_name" . "bob")
  ;;       ("date" . "2022-04-24"))
  ;;   (list first-name last-name date)) => '("bob" nil "2022-04-24")

  `(let (,@(mapcar (lambda (bind)
                     (if (listp bind)
                         (destructuring-bind (symbol &optional key safe)
                             bind
                           (if (and symbol key safe)
                               `(,symbol (jsown:val-safe ,object ,key))
                               `(,symbol (jsown:val ,object ,key))))
                         `(,bind (jsown:val ,object ,(string-downcase
                                                      (symbol-name bind))))))
                   bind-list))
     ,@body))


(defmacro distill-jsown-object-list (list-of-jsown-objects &rest path)
  "Given a list of JSOWN objects, provide a path for JSOWN:FILTER down each to get a value."
  (let ((current (gensym))
        (results (gensym)))
    `(let (,results)
       (dolist (,current ,list-of-jsown-objects)
         (push (jsown:filter ,current ,@path)
               ,results))
       ,results)))

(defun authenticate ()
  (destructuring-bind (&key client-id client-secret reddit-login reddit-password &allow-other-keys)
      *config*
    (handler-case
        (multiple-value-bind (result code)
            (drakma:http-request "https://www.reddit.com/api/v1/access_token"
                                 :basic-authorization (list client-id client-secret)
                                 :additional-headers (list (cons "User-Agent" "supyawl:0.2 (by /u/clintm"))
                                 :accept "application/json"
                                 :content-type "application/json"
                                 :parameters (list (cons "grant_type" "password")
                                                   (cons "username" reddit-login)
                                                   (cons "password" reddit-password))
                                 :method :post
                                 :preserve-uri t)
          (switch (code)
            ((or 503 504) (error "Bad auth response?"))
            (otherwise (destructure-jsown-object ((access-token "access_token"))
                           (jsown:parse result)
                         (setf *reddit-access-token* access-token)
                         ;; Don't possibly bleed our access token out to
                         ;; the world if I'm streaming working on this.
                         ;; I mean, you never know...
                         nil))))
      (usocket:ns-host-not-found-error (condition)
        (log:info "Host not found error: ~a" condition))
      (usocket:timeout-error (condition)
        (declare (ignore condition))
        (log:info "Timed out")))))


(defun start-server (&key (port 9000))
  (unless *listener*
    (setf *listener* (make-instance 'easy-routes:easy-routes-acceptor
                                    :access-log-destination nil
                                    :document-root (asdf:system-relative-pathname
                                                    :sup "static/am")
                                    :port port
                                    :address "0.0.0.0")))
  (hunchentoot:start *listener*)
  (update-subreddits-cache))


(defpsmacro with-document-ready (&body body)
  `(progn
     ((ps:@ ($ document) ready) ,@body)))

(defpsmacro jq (selector &body body)
  `(-> (sel ,selector)
       ,@body))

(defpsmacro sel (name)
  `($ ,name))

(defpsmacro $. (name)
  `(ps:@ (sel ,name)))

(defpsmacro -> (&body body)
  `(ps:chain ,@body))

(defpsmacro map (func list)
  `(do ((i 0 (incf i)))
       ((>= i (ps:@ ,list length)))
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
              `(:title "Sup"))
         (:meta
           :name "viewport"
           :content "width=device-width, initial-scale=1.0, maximum-scale=1.0, user-scalable=no")
         (:link :rel "stylesheet"
           :type "text/css" :href "/am/lib/stroke-7/style.css")
         (:link :rel "stylesheet"
           :type "text/css"
           :href "/am/lib/jquery.nanoscroller/css/nanoscroller.css")
         (:link :rel "stylesheet"
           :type "text/css"
           :href "/am/lib/jquery.magnific-popup/magnific-popup.css")
         (:link :rel "stylesheet" :type "text/css" :href "/am/css/style.css")
         (:script :src "/am/lib/jquery/jquery.min.js")
         (:script :src "https://cdn.jsdelivr.net/npm/lozad/dist/lozad.min.js")
         (:style :type "text/css"
           (:raw (css-lite:css
                   ((".am-wrapper") (:padding-top "0px !important"))
                   ((".md > p") (:margin "0px"
                                 :padding "0px"))
                   ((".youtube-container") (:position "relative"
                                            :padding-bottom "56.25%"
                                            :padding-top "30px"
                                            :height "0"
                                            :overflow "hidden"))
                   ((".youtube-container iframe, .youtube-container object, .youtube-container embed")
                    (:position "absolute"
                     :top "0"
                     :left "0"
                     :width "100%"
                     :height "100%"))))))

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
         (:script :src "/am/lib/bootstrap/dist/js/bootstrap.min.js")
         (:script :src "/am/lib/jquery-ui/jquery-ui.min.js")
         (:script :src "/am/lib/jquery.sparkline/jquery.sparkline.min.js")
         (:script :src "/am/lib/jquery.magnific-popup/jquery.magnific-popup.js")
         (:script :src "/am/lib/masonry/masonry.pkgd.min.js")
         (:script :src "/am/js/main.min.js")
         (:script :src "/am/js/app-page-gallery.js")
         (:script
           (:raw
            (ps:ps
              (defvar observer (lozad))
              (defun update-post (id)
                (-> (sel (+ "#awards-" id))
                    (load (+ "/awards/" id))))

              (defun update-graph (link-id)
                (-> $ (ajax (ps:create :type "get"
                                       :url (concatenate 'string "/graph/history/" link-id)
                                       :success (lambda (obj)
                                                  (-> (sel (concatenate 'string "#graph" link-id))
                                                      (sparkline obj
                                                                 (ps:create :width "100%"
                                                                            :height "35"
                                                                            :line-width 1.7))))))))))))
       (:script
         (:raw
          (ps:ps
            (with-document-ready
                (lambda ()
                  (-> -app (init))
                  (-> window (add-event-listener
                              "load"
                              (lambda ()
                                (-> -app (page-gallery)))))
                  (-> window (add-event-listener
                              "DOMContentLoaded"
                              (lambda ()
                                (-> -app (page-gallery)))))))))))))

(defun get-reddit-subreddits ()
  (labels ((reddit-request (path)
             (drakma:http-request (format nil "https://oauth.reddit.com~a" path)
                                  :additional-headers
                                  `(("User-Agent" . "supyawl:0.2 (by /u/clintm")
                                    ("Authorization" . ,(format nil "bearer ~a" *reddit-access-token*))))))
    (authenticate)
    (when-let ((post-data (handler-case (jsown:val (jsown:parse
                                                    (reddit-request "/subreddits/mine"))
                                                   "data")
                            (error (condition)
                              (log:info "Failure fetching subscribed subreddits: ~a"
                                        condition)))))
      (jsown:val post-data "children"))))

(defun sync-subreddits ()
  (if-let ((reddit-subreddits (get-reddit-subreddits)))
    (let* ((reddit-names (distill-jsown-object-list reddit-subreddits "data" "display_name"))
           (db-subreddits (with-pg (select-dao 'subreddit)))
           (db-names (mapcar #'subreddit-display-name db-subreddits))
           (needs-cleanup nil))

      ;; Confirm that all subscribed subreddits are in the database.
      (dolist (reddit-name reddit-names)
        (unless (member reddit-name db-names :test #'string=)
          (let* ((reddit-doc (jsown:val (car (remove-if-not #'(lambda (x)
                                                                (string= (jsown:filter x "data" "display_name") reddit-name))
                                                            reddit-subreddits))
                                        "data")))
            (destructure-jsown-object ((display-name "display_name")
                                       url id)
                reddit-doc
              (let ((new-subreddit
                      (with-pg (insert-dao
                                (make-instance 'subreddit
                                               :display-name display-name
                                               :url url
                                               :id id)))))
                (update-subreddits-cache)
                (log:info "Missing ~a" display-name)
                (sync-subreddit new-subreddit))))))

      ;; Remove db subreddits that aren't currently subscribed.
      (dolist (name db-names)
        (unless (member name reddit-names :test #'string=)
          (let ((db-record (car (remove-if-not #'(lambda (x)
                                                   (string= (subreddit-display-name x) name))
                                               db-subreddits))))
            (log:info "Removing ~a" name)
            (hide-subreddit name)
            (with-pg (delete-dao db-record))
            (setf needs-cleanup t)
            (update-subreddits-cache))))
      (when needs-cleanup
        (cleanup)))
    (log:info "Failed")))

(defmethod make-request-url ((subreddit string) &key type after)
  (make-request-url (car (with-pg (select-dao 'subreddit (:= 'display_name subreddit))))
                    :type type
                    :after after))

(defmethod make-request-url ((subreddit subreddit) &key type after)
  (let ((type (alexandria:switch (type)
                (:new "new/")
                (:controversial "controversial/")
                (:top "top/")
                (:otherwise "/"))))
    (format nil "~a~@[~a~].json?limit=100~@[&after=~a~]"
            (subreddit-url subreddit) type after)))

(defgeneric sync-subreddit (subreddit &key reset &allow-other-keys))

(defmethod sync-subreddit ((subreddit subreddit) &key reset)
  "Fetch the entire history of this subreddit."
  (labels ((handle-this-block (jsown-block)
             (dolist (link (jsown:filter jsown-block "data" "children"))
               (destructure-jsown-object (data kind)
                   link
                 (when (string= kind "t3")
                   (when reset
                     (with-pg
                       (when-let ((seen-record (get-dao 'seen (jsown:val data "id"))))
                         (delete-dao seen-record))))
                   (handle-possible-new-link data)))))
           (get-next-page (after-id type)
             (let ((new-url (make-request-url subreddit :type type :after after-id)))
               (when-let ((link (ignore-errors (jsown:parse (get-reddit new-url)))))
                 (handle-this-block link)
                 (when-let ((next-page (jsown:filter link "data" "after")))
                   (get-next-page next-page type))))))
    (dolist (type '(nil :top :controversial :new))
      (let ((parsed (jsown:parse (get-reddit (make-request-url subreddit :type type)))))
        (handle-this-block parsed)
        (get-next-page (jsown:filter parsed "data" "after")
                       type)))))

(defmethod sync-subreddit ((subreddit-name string) &key reset)
  (let ((subreddit (car (with-pg (query (:select '* :from 'subreddit
                                          :where (:= 'display_name subreddit-name))
                                        (:dao subreddit))))))
    (sync-subreddit subreddit :reset reset)))


(defmethod get-subreddit-links ((subreddit subreddit) &key type)
  (labels ((safe-parse (blob)
             (handler-case
                 (jsown:parse blob)
               (end-of-file (condition)
                 (log:info "END OF FILE?? WTF: ~a" condition)
                 nil))))
    (multiple-value-bind (data code)
        (let ((path (make-request-url subreddit :type type)))
          (get-reddit path))
      (switch (code)
        (200 (if-let ((json-data (safe-parse data)))
               (jsown:filter json-data "data" "children")
               (log:info "Links fetch was null? ~a ~a" code (subreddit-display-name subreddit))))
        (t (log:info "Connecting to reddit failed with: ~a for ~a"
                     code (subreddit-display-name subreddit))
           (sleep 1))))))

(defmethod send-update-graph ((link link))
  (lparallel.queue:push-queue
   (jsown:to-json `(:obj ("id" . ,(link-id link))
                         ("action" . "update-graph")))
   *interface-queue*))

(defun update-all-graphs ()
  (dolist (link (with-pg (select-dao 'link (:= 'hidden nil))))
    (send-update-graph link)))

(defmethod update-link-comments ((link link) &key (refresh t))
  (unless (and (< 0 (length (with-pg (query (:limit (:select 'id :from 'comment
                                                      :where (:= 'parent (format nil "t3_~a" (link-id link))))
                                                    1)))))
               (not refresh))
    (multiple-value-bind (data code)
        (handler-case
            (get-reddit (format nil "~acomments.json" (link-permalink link)))
          (flexi-streams:external-format-encoding-error (condition)
            (log:info "ENCODING ERROR: ~a" condition)
            (values 201 "NOTHING")))
      (switch (code)
        (200 (dolist (comment-hash (jsown:parse data))
               (dolist (listing (ignore-errors (jsown:filter comment-hash "data" "children")))
                 (destructure-jsown-object (kind data)
                     listing
                   (when (string= kind "t1")
                     (add-or-update-comment data))))))
        (t nil)))))

(defmethod update-link-comments ((link-id string) &key (refresh t) verbose)
  (update-link-comments (with-pg (get-dao 'link link-id))
                        :refresh refresh
                        :verbose verbose))

(defmethod update-comment-votes ((comment comment)
                                 (id string)
                                 (count integer))
  (if-let ((maybe-votes (with-pg (get-dao 'comment-votes id))))
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

(defparameter *media-mailbox*
  (sb-concurrency:make-mailbox :name "Media Mailbox"))

(defun add-to-media-queue (url link-id)
  (sb-concurrency:send-message *media-mailbox* (list url link-id)))

(defun fetch-media (url)
  (handler-case
      (multiple-value-bind (data status)
          (drakma:http-request url)
        (cond ((= status 200) data)
              (t nil)))
    (usocket:unknown-error (condition)
      (declare (ignore condition))
      nil)
    (usocket:timeout-error (condition)
      (log:warn "fetch-media: timeout: ~a" condition))))

(defun media-path (media)
  (with-slots (url)
      media
    (asdf:system-relative-pathname "sup"
                                   (format nil "images/~a.~a"
                                           (ivy:make-hash url)
                                           (get-image-url-suffix url)))))

(defun media-exists-p (media)
  (probe-file (media-path media)))

;; TODO: Why are these here?  I can't remember.
(defmethod write-to-file ((media media) (data vector))
  nil)

(defmethod write-to-file ((media media) (data string))
  nil)

(defmethod write-to-file ((media media)
                          (image-data #.(class-of (make-array 0 :element-type '(unsigned-byte 8)))))
  (handler-case
      (progn
        (unless (probe-file (media-path media))
          (alexandria:write-byte-vector-into-file image-data (media-path media))
          (log:debug "Wrote ~a" (media-path media))))
    (error (condition)
      (log:info "Error writing media for ~a: ~a"
                (media-link-id media) condition))))

(defmethod media-for-link ((link link))
  (with-pg (query (:select '* :from 'media
                    :where (:= 'link-id (link-id link)))
                  (:dao media))))

(defmethod media-for-link ((link-id string))
  (media-for-link (with-pg (get-dao 'link link-id))))

(defun handle-media-envelope (envelope)
  (handler-case
      (destructuring-bind (url linkid)
          envelope
        (if-let ((media (car (with-pg (select-dao 'media (:= 'hash (cl-ivy:make-hash url)))))))
          (unless (media-exists-p media)
            (when-let ((data (fetch-media url)))
              (write-to-file media data)))
          (when-let ((image (fetch-media (or (ignore-errors (html-entities:decode-entities url))
                                             url))))
            (let ((media (make-instance 'media
                                        :url url
                                        :link-id linkid
                                        :hash (cl-ivy:make-hash url))))
              (handler-case
                  (progn
                    (with-pg (insert-dao media))
                    (unless (media-exists-p media)
                      (write-to-file media image)))
                (cl-postgres-error:unique-violation (condition)
                  (declare (ignore condition))
                  nil))))))
    (error (condition)
      (log:info "media download failed: ~a" condition))))

(defun media-download-worker ()
  (handle-media-envelope (sb-concurrency:receive-message *media-mailbox*)))

(defun start-media-worker ()
  (bt:make-thread (lambda ()
                    (loop (media-download-worker)))
                  :name "media download worker"))

(defun find-media (doc-obj link-id)
  (destructure-jsown-object (url
                             (images "images" :safe)
                             (media-metadata "media_metadata" :safe)
                             (crosspost-parent-list "crosspost_parent_list" :safe))
      doc-obj


    (when crosspost-parent-list
      (let ((result (mapcar (lambda (entry)
                              (find-media entry link-id))
                            crosspost-parent-list)))
        (return-from find-media result)))

    (when url
      (when (ppcre:scan ".jpg|.png$|.gif$|.JPG$|.PNG$|.GIF$" url)
        (add-to-media-queue url link-id)
        (return-from find-media)))

    (when media-metadata
      (dolist (key (jsown:keywords media-metadata))
        ;;(log:info key)
        (let ((thingy (jsown:val media-metadata key)))
          (when-let ((fsu (ignore-errors (jsown:filter thingy "s" "u"))))
            (add-to-media-queue fsu link-id)))))

    (when-let ((images (and (not (jsown:val-safe images "preview"))
                            media-metadata)))
      (let (fnd)
        (dolist (image-key (jsown:keywords images))
          (setf fnd 1)
          (let ((image (jsown:val images image-key)))
            (when-let* ((image-url (ignore-errors (jsown:filter image "s" "u"))))
              (add-to-media-queue image-url link-id))))
        (when fnd
          (return-from find-media :preview))))


    (alexandria:if-let ((preview-images (ignore-errors (jsown:filter doc-obj "preview" "images"))))
      (progn
        (dolist (preview preview-images)
          (when-let ((url (jsown:filter preview "source" "url")))
            (add-to-media-queue url link-id))))
      (when (ignore-errors (jsown:filter doc-obj "secure_media" "type"))
        (dolist (preview (ignore-errors
                          (jsown:filter doc-obj "preview" "images")))
          (let ((url (jsown:filter preview "source" "url")))
            (add-to-media-queue url link-id)))
        (return-from find-media)))))

(defun scan-media-for-id (id)
  (let* ((link (with-pg (get-dao 'link id)))
         (json-string (link-bulk link)))
    (unless (eq json-string :null)
      (find-media (jsown:parse (link-bulk link)) id))))

(defun load-em-up ()
  (with-lparallel (:workers 4)
    (lparallel:pmapcar #'scan-media-for-id
                       (with-pg (query (:order-by (:select 'id :from 'link)
                                                  (:asc 'created_utc))
                                       :column)))))

(defmethod rescan-link ((link-id string))
  (rescan-link (with-pg (get-dao 'link link-id))))

(defmethod rescan-link ((link link))
  (find-media (jsown:parse (link-bulk link)) (link-id link)))

(defun rescan-media ()
  (dolist (link-id (with-pg (query (:select 'id :from 'link))))
    (rescan-link link-id)))

(defroute handle-media ("/media/:id") ()
  (let* ((media (with-pg (get-dao 'media id)))
         (filename (media-path media)))
    (if (probe-file filename)
        (let* ((image-data (alexandria:read-file-into-byte-vector
                            filename))
               (image-type (get-image-url-suffix (media-url media)))
               (content-type (switch (image-type :test #'string=)
                               ("jpg" "image/jpg")
                               ("png" "image/png")
                               ("gif" "image/gif")
                               (otherwise (log:info "WHAT IS THIS: ~a" image-type)
                                          "image/jpg"))))
          (setf (hunchentoot:content-type*) content-type)
          image-data)
        nil)))

(defun make-new-like-cache (link-obj)
  (destructure-jsown-object (id score)
      link-obj
    (with-pg (insert-dao (make-instance
                          'up-history
                          :id id
                          ;; You can create it with a list,
                          ;; but it will come back as a
                          ;; simple-vector
                          :scores (vector score))))))

(defun update-like-cache (link-obj)
  (destructure-jsown-object (id score)
      link-obj
    (if-let ((up-hist (with-pg (get-dao 'up-history id))))
      (progn
        (let ((scores (make-array (length (up-history-scores up-hist))
                                  :initial-contents (up-history-scores up-hist)
                                  :adjustable t
                                  :fill-pointer t)))
          (vector-push-extend score scores)
          (setf (up-history-scores up-hist) scores))
        (with-pg (update-dao up-hist)))
      (make-new-like-cache link-obj))))

(defun handle-possible-new-link (link-obj)
  (destructure-jsown-object (id title subreddit score permalink selftext author url
                                (label-flair-text "label_flair_text" :safe)
                                (link-flair-background-color "link_flair_background_color")
                                (link-flair-text-color "link_flair_text_color")
                                (subreddit-id "subreddit_id")
                                (created-utc "created_utc")
                                (media-only "media_only")
                                (author-fullname "author_fullname" :safe)
                                (over-18 "over_18")
                                (selftext-html "selftext_html"))
      link-obj
    (if-let ((existing-link (with-pg (get-dao 'link id))))
      (progn
        (with-pg (query (:update 'link :set 'bulk '$1 :where (:= 'id '$2))
                        (jsown:to-json link-obj)
                        id))
        (when (link-hidden existing-link)
          (return-from handle-possible-new-link))
        (update-like-cache link-obj)
        (find-media link-obj id)
        (send-update-graph existing-link))
      (unless (has-seen-p id)
        (log:info "~a -> ~a" subreddit title)
        (let ((new-link (make-instance
                         'link
                         :id id
                         :hidden nil
                         :written (get-universal-time)
                         :created-utc created-utc
                         :url url
                         :permalink permalink
                         :author author
                         :subreddit-id subreddit-id
                         :media-only media-only
                         :score score
                         :title title
                         :nsfw over-18
                         :author-fullname author-fullname
                         :selftext selftext
                         :subreddit subreddit
                         :bulk (jsown:to-json link-obj)
                         :body selftext-html
                         :label-text label-flair-text

                         :label-background-color (or link-flair-background-color
                                                     "yellow")
                         :label-color (or link-flair-text-color
                                          "black"))))
          (add-seen id)
          (with-pg (insert-dao new-link))
          (find-media link-obj (link-id new-link))
          (update-like-cache link-obj)
          (lparallel.queue:push-queue (jsown:to-json `(:obj ("id" . ,(link-id new-link))
                                                            ("action" . "add-link")))
                                      *interface-queue*)
          (send-update-graph new-link))))))

(defun send-hide-link (id)
  (lparallel.queue:push-queue (jsown:to-json `(:obj ("id" . ,id)
                                              ("action" . "remove-link")))
                              *interface-queue*))

(defmethod hide-subreddit ((subreddit string))
  (hide-subreddit (car (with-pg (select-dao 'subreddit (:= :display_name subreddit))))))

(defmethod hide-subreddit ((subreddit subreddit))
  (dolist (link-id (with-pg (query (:select 'id :from 'link :where (:and (:= 'subreddit
                                                                             (subreddit-display-name subreddit))
                                                                         (:= 'hidden nil)))
                                   :column)))
    (with-pg (query (:update 'link :set 'hidden '$1 :where (:= 'id '$2))
                    t link-id))
    (send-hide-link link-id)))

(defmethod show-subreddit ((subreddit string))
  (show-subreddit (car (with-pg (select-dao 'subreddit (:= :display_name subreddit))))))

(defmethod show-subreddit ((subreddit subreddit))
  (dolist (id (with-pg (query (:select 'id :from 'link :where (:= 'subreddit (subreddit-display-name subreddit))))))
    (lparallel.queue:push-queue (jsown:to-json `(:obj ("id" . ,id)
                                                      ("action" "add-link")))
                                *interface-queue*)))


(defmethod update-subreddit ((subreddit subreddit))
  (unless (ppcre:scan "u_" (subreddit-display-name subreddit))
    (dolist (type '(nil :new :controversial :top))
      (dolist (link (distill-jsown-object-list
                     (get-subreddit-links subreddit :type type)
                     "data"))
        (handle-possible-new-link link)))))

(defmethod update-subreddit ((subreddit-name string))
  (update-subreddit (car (with-pg (select-dao 'subreddit (:= 'display-name subreddit-name))))))


(defun scan-subreddits ()
  (dolist (subreddit (with-pg (select-dao 'subreddit)))
    (update-subreddit subreddit)
    (sleep 1)))

(defun global-refresh ()
  (sync-subreddits)
  (scan-subreddits))

(defun start-refresh-threads ()
  (bt:make-thread (lambda ()
                    (loop
                      (global-refresh)))
                  :name "fetchers"))

(defun stop-refresh-threads ()
  (cl-ivy:stop-thread-by-name "fetchers"))

(defmethod mark-subreddit ((subreddit string) (mark-type (eql :read)))
  (mark-subreddit
   (car (with-pg (query
                  (:select '* :from 'subreddit :where (:= 'display_name subreddit))
                  (:dao subreddit))))
   :read))
(defmethod mark-subreddit ((subreddit subreddit) (mark-type (eql :read)))
  (with-slots (display-name) subreddit
    (dolist (link (with-pg (select-dao 'link (:and (:= 'hidden nil)
                                                   (:= 'subreddit display-name)))))
      (mark-link link :read))))
(defmethod mark-subreddit ((subreddit string) (mark-type (eql :unread)))
  (mark-subreddit
   (car (with-pg (query
                  (:select '* :from 'subreddit :where (:= 'display_name subreddit))
                  (:dao subreddit))))
   :unread))
(defmethod mark-subreddit ((subreddit subreddit) (mark-type (eql :unread)))
  (with-slots (display-name) subreddit
    (dolist (link (with-pg (select-dao 'link (:and (:= 'hidden nil)
                                                   (:= 'subreddit display-name)))))
      (mark-link link :unread))))

(defmethod mark-link ((link link) (mark-type (eql :shadow)))
  (if (link-shadow link)
      (with-pg (query (:update 'link :set 'shadow '$1 :where (:= 'id '$2))
                      nil (link-id link)))
      (with-pg (query (:update 'link :set 'shadow '$1 :where (:= 'id '$2))
                      t (link-id link)))))
(defmethod mark-link ((link link) (mark-type (eql :nsfw)))
  (if (link-nsfw link)
      (with-pg (query (:update 'link :set 'nsfw '$1 :where (:= 'id '$2))
                      nil (link-id link)))
      (with-pg (query (:update 'link :set 'nsfw '$1 :where (:= 'id '$2))
                      t (link-id link)))))
(defmethod mark-link ((link link) (mark-type (eql :favorite)))
  (if (link-favorite link)
      (with-pg (query (:update 'link :set 'favorite '$1 :where (:= 'id '$2))
                      nil (link-id link)))
      (with-pg (query (:update 'link :set 'favorite '$1 :where (:= 'id '$2))
                      t (link-id link)))))
(defmethod mark-link ((link link) (mark-type (eql :read)))
  (with-pg (query (:update 'link :set 'hidden '$1 :where (:= 'id '$2))
                  t (link-id link))))
(defmethod mark-link ((link link) (mark-type (eql :unread)))
  (with-pg (query (:update 'link :set 'hidden '$1 :where (:= 'id '$2))
                  nil (link-id link))))


(defmethod display-link ((link-id string))
  (display-link (with-pg (get-dao 'link link-id))))


(defroute handle-awards ("/awards/:id") ()
  (let ((link (with-pg (get-dao 'link id))))
    (let ((doc-obj (jsown:parse (link-bulk link))))
      (with-html-string
        (dolist (award (jsown:val doc-obj "all_awardings"))
          (destructure-jsown-object (name (icon-url "icon_url"))
              award
            (:img
              :style "max-height:25px;margin-left:2px;"
              :title name
              :src icon-url)))))))

(defmethod display-link ((link link))
  (with-slots (id title url author subreddit permalink bulk created-utc)
      link
    (with-html
      (let ((unique-id (format nil "~a" (link-id link)))
            (doc-obj (and (not (eq :NULL bulk))
                           (jsown:parse bulk))))
        (:div :class "col-md-4 post" :id (format nil "wx~a" (link-id link))
          (:div.panel.panel-default.panel-borders.panel-heading-fullwidth
           :style "border:1px solid rgb(7,7,7);"
           (:div :class "panel-heading"
             :style "font-size:11px;padding: 15px 15px 10px;"
             (:div :class "tools"
               (:div :class "icon"
                 (:a :id (format nil "hider-~a" unique-id)
                   (:i :class "s7-look")))
               (:script
                 (:raw
                  (ps:ps*
                   `(-> (sel ,(format nil "#hider-~a" unique-id))
                        (click (lambda (e)
                                 (-> (sel ,(format nil "#link-~a" unique-id))
                                     (toggle))
                                 (-> e (prevent-default))))))))
               (make-author-link link)
               (:a :style "font-weight:bold;"
                 :target (format nil "win-~a" unique-id)
                 :href (format nil "/link/~a" id)
                 (:div.icon (:span.s7-id)))
               (:a :style "font-weight:bold;"
                 :target (format nil "json-~a" unique-id)
                 :href (format nil "/json/~a" id)
                 (:div :class "icon" (:i :class "s7-safe")))
               (:a :id (format nil "shadow-~a" unique-id)
                 (:div :class "icon" (:i :class "s7-trash")))
               (:script
                 (:raw
                  (ps:ps*
                   `(-> (sel ,(format nil "#shadow-~a" unique-id))
                        (click (lambda (e)
                                 (-> (sel ,(format nil "#wx~a" unique-id)) (toggle))
                                 (-> (sel ,(format nil "#wx~a" unique-id)) (load ,(format nil "/shadow/~a" id)))
                                 (-> e (prevent-default))))))))

               (:div.icon :id (format nil "favorite-~a" unique-id)
                          (:span.s7-download))
               (:div.icon :id (format nil "hide-~a" unique-id)
                          (:span.s7-close-circle))

               (let* ((label-richtext (jsown:val doc-obj "link_flair_richtext")))
                 (if label-richtext
                     (progn
                       (:br)
                       (with-html
                         (:span :class "label pull-right"
                           :style (format nil "padding:0 5px;font-size:12px;font-weight:500;line-height:16px;white-space:nowrap;display:inline-block;height:16px;vertical-align:middle;background-color:~a;"
                                          (jsown:val doc-obj "link_flair_background_color"))
                           (mapcar (lambda (blob)
                                     (destructure-jsown-object (e (u "u" :safe) (thingy "t" :safe))
                                         blob
                                       (cond ((string= e "emoji")
                                              (:span :style (format nil "display:inline-block;background-position:center;background-repeat:no-repeat;background-size:contain;position:relative;height:15px;width:15px;vertical-align:middle;background-image:url(~a)"
                                                                    u)))
                                             ((string= e "text")
                                              (:span (:raw thingy))))))
                                   label-richtext)))
                       (:br)))))
             (:b
               (:a :target (format nil "win-~a" (uuid:make-v4-uuid))
                 :href url (html-entities:decode-entities title)))
             (:br)

             (:a :target (format nil "win-~a" (uuid:make-v4-uuid))
               :href (format nil "https://www.reddit.com~a" permalink)
               author)
             " "
             (:div :id (format nil "author-flair-~a" id))
             (:div :style "font-weight:bold;"
               (:a :href (format nil "/subreddit/~a/0" subreddit)
                 subreddit))
             (:div :id (format nil "awards-~a" id))
             (:div :class "col-md-12")
             (:script
               (:raw
                (ps:ps*
                 `(with-document-ready
                      (lambda ()
                        (-> (sel ,(format nil "#favorite-~a" unique-id))
                            (click (lambda (e)
                                     (ps:chain (sel ,(format nil "#wx~a" unique-id))
                                               (load ,(format nil "/link/favorite/~a" id)))
                                     (-> (sel ,(format nil "#wx~a" unique-id))
                                         (toggle))
                                     (-> e (prevent-default)))))
                        (-> (sel ,(format nil "#hide-~a" unique-id))
                            (click (lambda (e)
                                     (let ((selector ,(format nil "#wx~a" unique-id)))
                                       (-> (sel selector) (fade-out 100
                                                                    (lambda ()
                                                                      (ps:chain (sel selector)
                                                                                (load ,(format nil "/link/hide/~a" (hunchentoot:url-encode id))))
                                                                      (-> (sel selector) (remove))
                                                                      (-> e (prevent-default)))))))))))))))
           (:script
             (:raw
              (ps:ps*
               `(with-document-ready
                    (lambda ()
                      (-> (sel ,(format nil "#link-~a" unique-id))
                          (load ,(format nil "/link/body/~a" id)))
                      (update-graph ,id)
                      (update-post ,id))))))
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

(defun get-image-url-suffix (url)
  (let ((uri (puri:parse-uri url)))
    (cadr (ppcre:split "\\." (puri:uri-path uri)))))

(defun fetcher-is-running-p ()
  (member "fetchers" (mapcar #'bt:thread-name
                             (bt:all-threads))
          :test #'string=))

(defun mark-everything-unread ()
  (dolist (subreddit (with-pg (select-dao 'subreddit)))
    (mark-subreddit (subreddit-display-name subreddit) :unread)))

(defun mark-everything-read ()
  (dolist (subreddit (with-pg (select-dao 'subreddit)))
    (mark-subreddit subreddit :read)))

(defvar *display-offset* nil)

(defmacro links-page (&rest body)
  `(with-page ()
     (:div :class "row"
       (:div :class "col-md-12"
         (:form :method :post :action "/search"
           (:div :class "col-md-3"
             (:div :class "btn-group btn-space"
               (:a :class "btn btn-default btn-xs"
                 :type "button" :href "#" :id "resetbutton"
                 (:div :class "icon"
                   (:i :class "s7-refresh")))
               (:a :class "btn btn-default btn-xs"
                 :type "button" :href "/nsfw" "NSFW")
               (:a :class "btn btn-default btn-xs"
                 :type "button" :href "/favorites" "Fav")
               (:a :class "btn btn-default btn-xs" :type "button" :href "/live" "Live")
               (:a.btn.btn-default.btn-xs :type "button" :href "/mine" :target "mineminemine" "Mine")

               (:a :class "btn btn-default btn-xs" :id "hidebutton" :type "button" :href "#" "C")
               (:script (:raw
                         (ps:ps (-> (sel "#resetbutton")
                                    (click (lambda (e)
                                             (reset-interface)
                                             (-> event (prevent-default))))))
                         (ps:ps (-> (sel "#hidebutton")
                                    (click (lambda (e)
                                             (-> (sel ".link-body") (hide))
                                             (-> e (prevent-default))))))))

               (:a :class "btn btn-default btn-xs" :id "expandcomments" :type "button" :href "#" "E")
               (:script (:raw
                         (ps:ps (-> (sel "#expandcomments")
                                    (click (lambda (e)
                                             (-> (sel ".comment-trigger") (click))
                                             (-> e (prevent-default))))))))
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
     ,@body))

(defun display-links (links)
  (links-page
   (when *display-offset*
     (:a :class "btn btn-default btn-xs" :href *display-offset* "Next"))
   (:div :class "row"
     (dolist (link links)
       (display-link link)))
   (when *display-offset*
     (:a :class "btn btn-default btn-xs" :href *display-offset* "Next"))))

(defmethod make-author-link-from-comment ((comment comment))
  (let ((author (comment-author comment)))
    (with-html
      (:div.icon
       (:a
         :target (format nil "~aauthorcomments" author)
         :href (format nil "/comments/author/~a" author)
         (:i :class "s7-note"))))))

(defmethod display-comment ((comment comment))
  (with-slots (body flair-text bulk) comment
    (with-html
      (:div.pull-left :style "font-size:10px;" (make-author-link comment))
      (:span.pull-left :style "font-size:12px;"
                       (:a :class "pull-left"
                         :style "padding-left:5px;"
                         :href (format nil "https://reddit.com/u/~a" (comment-author comment))
                         (comment-author comment))

                       (when-let ((bulk (comment-bulk comment)))
                         (dolist (award (jsown:val (jsown:parse bulk)
                                                   "all_awardings"))
                           (destructure-jsown-object (name (icon-url "icon_url"))
                               award
                             (:img :class "pull-left"
                               :style "max-height:12px;margin-left:2px;"
                               :title name
                               :src icon-url))))

                       (when (and flair-text
                                  bulk
                                  (not (string= flair-text "false")))
                         (when-let* ((parsed (jsown:parse bulk))
                                     (bgcolor (or (jsown:val parsed "author_flair_background_color")
                                                  "#444444;")))
                           (:span.pull-right
                            (:span.text-warning :style (format nil "padding-left:4px;padding-right:4px;margin-left:5px;background-color:~a"
                                                               bgcolor)
                                                flair-text)))))

      (:span.pull-right :style "font-size:12px;" ("~a" (comment-score comment)))
      (:br)
      (:span :class "comment" :style "font-size:12px;"
        (alexandria:if-let ((html-body (comment-body-html comment)))
          (:raw (html-entities:decode-entities html-body))
          (handler-case
              (with-output-to-string (sink)
                (ignore-errors (cl-markdown:markdown body
                                                     :stream *html*)))
            (error (condition)
              (log:info condition)))))
      (let ((replies (with-pg (select-dao 'comment (:= 'parent (format nil "t1_~a"
                                                                       (comment-id comment)))))))
        (dolist (reply (or replies nil))
          (:div :style "border-left:1px solid pink;padding-left:12px;"
            (display-comment reply)))))))

(defun add-or-update-comment (comment-obj)
  (destructure-jsown-object (id score author body replies
                                (parent-id "parent_id")
                                (author-flair-text "author_flair_text")
                                (body-html "body_html")
                                (body-text "body_text" :safe)
                                subreddit)
      comment-obj
    (when-let ((db-comment (with-pg (get-dao 'comment id))))
      (with-pg (delete-dao db-comment)))
    (let* ((comment-text (or body-html
                             body-text
                             body))
           (new-comment (make-instance 'comment
                                       :id id
                                       :parent parent-id
                                       :score score
                                       :link-id id
                                       :flair-text (if (string= author-flair-text
                                                                "false")
                                                       nil
                                                       author-flair-text)
                                       :author author
                                       :bulk (jsown:to-json comment-obj)
                                       :body-html comment-text)))
      (with-pg (insert-dao new-comment))
      (update-comment-votes new-comment id score)
      (log:debug "~a ~a : ~a" author subreddit comment-text))
    (dolist (reply (ignore-errors (jsown:filter replies "data" "children")))
      (destructure-jsown-object (kind data)
          reply
        (when (string= "t1" kind)
          (add-or-update-comment data))))))

(defvar *seen-comments* (make-hash-table :test 'equal))

(defun link-hidden-p (link-id)
  (ivy:orf (gethash link-id *seen-comments*)
           (caar (with-pg (query (:select 'hidden :from 'link
                                   :where (:= 'id (ppcre:regex-replace "^t3_" link-id ""))))))))

(defun has-commentp (comment-id)
  (ivy:orf (gethash comment-id *seen-comments*)
       (< 0 (caar (with-pg (query (:select (:count '*) :from 'comment :where (:= 'id comment-id))))))))

(defun sync-comments ()
  (dolist (link (with-pg (select-dao 'link (:= 'hidden nil))))
    (dolist (comment (jsown:parse (get-reddit (format nil "~acomments.json"
                                                      (link-permalink link)))))
      (dolist (comment-bulk (jsown:filter comment "data" "children"))
        (let ((comment (jsown:val comment-bulk "data")))
          (destructure-jsown-object (id body author)
              comment
            (when (and (not (link-hidden-p (link-id link)))
                       (not (has-commentp id)))
              (add-or-update-comment comment)
              (handler-case
                  (log:info "~a on ~a ~a ~% ~a ~%"
                            author (link-subreddit link)
                            (link-id link) (subseq body 0 20))
                (error (condition)
                  (log:info "Encoding error: ~a" condition))))))))))

(defun comment-stream ()
  (loop
    (sync-comments)
    (sleep 2)))


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

(eval-when (:compile-toplevel :load-toplevel :execute)
  (pushnew 'find-room hunchensocket:*websocket-dispatch-table*))

(defmethod mailbox-send ((user user) (message string) &rest args)
  (handler-case
      (hunchensocket:send-text-message user (apply #'format nil message args))
    (error (condition)
      (log:info "ERROR sending websocket message: ~a" condition))))

(defun broadcast (room message &rest args)
  (dolist (client (hunchensocket:clients room))
    (mailbox-send client message args)))

(defmethod interface-add-link ((link link))
  (interface-add-link (link-id link)))

(defmethod interface-add-link ((link-id integer))
  (interface-add-link (format nil "~a" link-id)))

(defmethod interface-add-link ((link-id string))
  (lparallel.queue:push-queue (jsown:to-json `(:obj ("action" . "add-link")
                                                    ("id" . ,link-id)))
                              *interface-queue*))

(defun new-articles-to-live ()
  (dolist (link (with-pg (query (:order-by
                                 (:select '* :from 'link :where (:= 'hidden nil))
                                 (:asc 'created_utc))

                                (:dao link))))
    (lparallel.queue:push-queue (jsown:to-json `(:obj ("action" . "add-link")
                                                      ("id" . ,(link-id link))))
                                *interface-queue*)))

(defmethod hunchensocket:client-connected ((room chat-room) user)
  (declare (ignore user))
  (new-articles-to-live))

(defmethod hunchensocket:client-disconnected ((room chat-room) user)
  (declare (ignore user))
  nil)

(defun hide-all-shown-links ()
  (let ((ids (with-pg (query (:select 'id :from 'link :where (:= 'hidden nil))))))
    (dolist (id ids)
      (send-hide-link (car id)))))

(defmethod hunchensocket:text-message-received ((room chat-room) user message)
  (declare (ignore user))
  (switch (message :test #'string=)
    ("reset-interface" (hide-all-shown-links)
                       (new-articles-to-live))
    (t nil ;;(log:info "Message from interface: ~a" message)
       )))


(defvar *ws-server* nil)

(defun start-ws-reader-thread ()
  (bt:make-thread (lambda ()
                    (loop
                      (if-let ((message (lparallel.queue:pop-queue *interface-queue*)))
                        (handler-case (broadcast (car *chat-rooms*) message)
                          (error (condition)
                            (log:info "Error broadcasting message: ~a" condition)))
                        (sleep 1))))
                  :name "ws broadcast"))

(defun start-ws-server ()
  (unless *ws-server*
    (setf *ws-server* (make-instance 'hunchensocket:websocket-acceptor
                                     :port 9001
                                     :access-log-destination nil))
   (hunchentoot:start *ws-server*)))

(defun check-image (filepath)
  (let* ((filename (file-namestring (path:basename filepath)))
         (hash (car (ppcre:split "\\." filename))))
    (unless (or (ppcre:scan "git_keep" filename)
                (with-pg (query (:select 'hash :from 'media :where (:= 'hash hash)))))
      (delete-file filepath)
      (log:info "Deleted ~a" filename))))

(defun cleanup-images ()
  (log:info "Cleaning db...")
  (map 'nil (lambda (link-id)
              (unless (with-pg (query (:select 'id :from 'link
                                        :where (:= 'id link-id))))
                (with-pg (let ((obj (select-dao 'media (:= 'link-id link-id))))
                           (dolist (row obj)
                             (delete-dao row))))))
       (with-pg (query (:select (:distinct 'link-id)
                         :from 'media)
                       :column)))
  (log:info "Cleaning files...")
  (cl-fad:walk-directory (asdf:system-relative-pathname :sup "images")
                         #'check-image))

(defun cleanup ()
  (labels ((remove-comment-by-parent (parent)
             (dolist (comment-id (car (with-pg (query (:select 'id :from 'comment
                                                        :where (:= 'parent parent))))))
               (remove-comment-by-parent (format nil "t1_~a" comment-id))
               (with-pg (execute "delete from comment where id = $1"
                                 comment-id)))))

    (let ((subreddits (with-pg (query (:select 'display-name :from 'subreddit)
                                      :column))))
      (dolist (link (with-pg (query (:select 'id 'subreddit :from 'link))))
        (destructuring-bind (id subreddit) link
          (unless (member subreddit subreddits :test #'string=)
            (let ((link (with-pg (get-dao 'link id))))
              (with-pg (delete-dao link)))))))

    (dolist (comment-parent (with-pg (query (:select 'parent :from 'comment)
                                            :column)))
      (when (ppcre:scan "^t3_" comment-parent)
        (unless (car (with-pg (query (:select 'id :from 'link
                                       :where (:= 'id (ppcre:regex-replace "^t3_" comment-parent "")))
                                     :column)))
          (remove-comment-by-parent comment-parent))))

    (dolist (vote-id (with-pg (query (:select 'id :from 'up-history)
                                     :column)))
      (unless (car (with-pg (query (:select 'id :from 'link :where (:= 'id vote-id)))))
        (with-pg (delete-dao (get-dao 'up-history vote-id)))))
    (cleanup-images))
  (log:info "Cleanup done..."))

(defun download-entire-internet ()
  (dolist (subreddit (with-pg (select-dao 'subreddit)))
    (sync-subreddit subreddit)))

(defun mark-everything-older-than-a-month-as-read ()
  (dolist (linkid (with-pg (query (:order-by (:select 'id :from 'link :where
                                               (:and (:= 'hidden nil)
                                                     (:>= (- (cl-ivy:epoch-time) (* 2 24 60 60))
                                                          'created_utc)))
                                             (:desc 'created_utc))
                                  :column)))
    (let ((the-link (with-pg (get-dao 'link linkid))))
      (mark-link the-link :read))))



(defroute stop-threads ("/stop-fetchers") ()
  (stop-refresh-threads)
  (hunchentoot:redirect "/live"))

(defroute start-threads ("/start-fetchers") ()
  (start-refresh-threads)
  (hunchentoot:redirect "/live"))

(defroute get-votes ("/votes/:id") ()
  (setf (hunchentoot:content-type*) "application/json")
  (if-let ((up-history (with-pg (get-dao 'up-history id))))
    (with-output-to-string (sink)
      (jsown:to-json (up-history-scores up-history)))
    "[]"))

(defroute show-author ("/author/:author") ()
  (display-links (with-pg (query (:order-by
                                  (:select '* :from 'link :where (:= 'author author))
                                  (:desc 'created-utc))
                                 (:dao link)))))

(defroute display-single-link-json ("/link/json/:id") ()
  (let ((link (with-pg (get-dao 'link id))))
    (setf (hunchentoot:content-type*) "application/json")
    (link-bulk link)))

(defroute display-single-link ("/link/:id") ()
  (with-page ()
    (display-link id)))

(defroute display-subreddit ("/subreddit/:subid/:offset" :method :get) ()
  (let* ((*display-offset* (format nil "/subreddit/~a/~a"
                                   subid (+ (or (parse-integer offset) 0) 100))))
    (display-links
     (with-pg (query (:limit (:order-by (:select '* :from 'link
                                          :where (:and (:= 'subreddit subid)
                                                       (:= 'shadow nil)))
                                        (:desc 'created-utc))
                             100 offset)
                     (:dao link))))))

(defroute favorites ("/favorites") ()
  (display-links (with-pg (query (:order-by (:select '* :from 'link
                                              :where (:= 'favorite t))
                                            (:desc 'created_utc))
                                 (:dao link)))))

(defroute shadow-link ("/shadow/:id") ()
  (when-let ((link (with-pg (get-dao 'link id))))
    (mark-link link :shadow)))

(defroute index ("/") ()
  (hunchentoot:redirect "/live"))

(defroute hide-link ("/link/hide/:id") ()
  (let ((link (with-pg (get-dao 'link id))))
    (mark-link link :read))
  "ok")

(defroute make-favorite ("/link/favorite/:id") ()
  (let ((link (with-pg (get-dao 'link id))))
    (mark-link link :favorite))
  "ok")

(defroute reset-media ("/media/reset/:id") ()
  (with-pg
    (when-let ((media (get-dao 'media id)))
      (with-slots (url link-id) media
        (log:info (media-path media) (probe-file (media-path media)))
        (ignore-errors (delete-file (media-path media)))
        (delete-dao media)
        (add-to-media-queue url link-id))))
  "ok")

(deftag post-media (control attrs &key link-id)
  `(progn
     (when-let ((media-list (with-pg (select-dao 'media (:= 'link-id ,link-id)))))
       (with-html
         (:div.panel-body
          (:div.gallery-container
           (dolist (media media-list)
             (:div.item
              (:div :class "photo"
                (:div :class "img" :style "min-height:75px;"
                  (:img :class "img-responsive"
                    :id (media-id media)
                    :src (format nil "/media/~a" (media-id media)))
                  (:div.over
                   (:div.func
                    (:a :class "image-zoom" :href (format nil "/media/~a" (media-id media))
                      (:i.icon.s7-search))))))))))
         (:script
           (let ((media-ids (mapcar #'media-id media-list)))
             (:raw
              (ps:ps*
               `(set-timeout
                 (lambda ()
                   (-> -app (page-gallery))
                   (let ((media-list (list ,@media-ids)))
                     (ps:for-in (media media-list)
                                (let ((media-id (+ "#" (aref media-list media))))
                                  (-> (sel media-id)
                                      (css "border" "1px solid red")))))
                   true)
                 1000)))))))
     ,@attrs ,@control))

(defroute show-comments ("/comments/:id") ()
  (let ((link (with-pg (get-dao 'link id))))
    (update-link-comments link)
    (let ((comments (with-pg
                      (query (:order-by (:select '* :from 'comment
                                          :where (:= 'parent (format nil "t3_~a" id)))
                                        (:desc 'score))
                             (:dao comment)))))
      (with-html-string
        (post-media :link-id id)
        (if comments
            (dolist (comment comments)
              (display-comment comment))
            (:div "No comments yet."))))))

(defroute show-link-json ("/json/:id") ()
  (setf (hunchentoot:content-type*) "application/json")
  (link-bulk (with-pg (get-dao 'link id))))

(defun render/selftext-html (doc)
  (let ((id (jsown:val doc "id")))
    (when-let ((toggle (jsown:val-safe doc "selftext_html")))
      (with-html-string
        (:raw
         (with-output-to-string (sink)
           (let ((decoded (html-entities:decode-entities toggle)))
             (handler-case (markdown:markdown decoded :stream sink)
               (error (condition)
                 (declare (ignore condition))
                 (format sink "~a" decoded))))))
        (when-let ((media-list
                    (with-pg
                      (select-dao 'media (:= 'link-id id)))))
          (:div.panel-body
           (:div.gallery-container
            (dolist (media media-list)
              (:div.item
               (:div.photo
                (:div.img :style "min-height:75px;"
                          (:img :class "img-responsive" :src (format nil "/media/~a" (media-id media)))
                          (:div.over
                           (:div.func
                            (:a.image-zoom :href (format nil "/media/~a" (media-id media))
                                           (:i.icon.s7-search))))))))))
          (:script
            (:raw
             (ps:ps
               (set-timeout
                (lambda ()
                  (-> -app (page-gallery)))
                1000)))))
        (:div "selftext-html")))))

(defun render/crossposted-reddit-video (doc)
  (when-let ((result (ignore-errors
                      (jsown:filter (car (jsown:val doc "crosspost_parent_list"))
                                    "secure_media" "reddit_video" "fallback_url"))))
    (with-html-string
      (:video :preload "none" :class "img-responsive" :controls 1
        (:source :src result))
      (:div "crossposted-reddit-video"))))

(defun render/reddit-preview-of-imgur-gif (doc)
  (when-let ((result (ignore-errors
                      (jsown:filter doc "preview" "reddit_video_preview" "fallback_url"))))
    (with-html-string
      (:video :preload "none" :class "img-responsive" :controls 1
        (:source :src result))
      (:div "reddit-preview-of-imgur-gif"))))

(defun render/reddit-video (doc)
  (when-let ((result (ignore-errors
                      (jsown:filter doc "secure_media" "reddit_video" "fallback_url"))))
    (with-html-string
      (:video :preload "auto" :class "img-responsive" :controls 1
        (:source :src result))
      (:div "is-reddit-video"))))

(defun render/crossposted-media-content (doc)
  (when-let ((result (ignore-errors
                      (jsown:filter (jsown:val-safe doc "crosspost_parent_list")
                                    "media_embed" "content"))))
    (with-html-string
      (:raw (html-entities:decode-entities result))
      (:div "crossposted-media-content"))))

(defun render/url-is-mp4 (doc)
  (when-let ((result (ppcre:scan ".mp4" (jsown:val-safe doc "url"))))
    (with-html-string
      (:video :class "img-responsive" :controls "1"
        (:source :src result))
      (:div "url-is-mp4"))))

(defun render/imgur-gifv (doc)
  (when-let ((result (ppcre:scan ".gifv$" (jsown:val-safe doc "url"))))
    (with-html-string
      (let ((thing (ppcre:regex-replace ".gifv" result ".mp4")))
        (:video :preload "none" :class "img-responsive" :controls "1"
          (:source :src thing))
        (:div "is-imgur-gifv")))))

(defun render/embedded-image (doc)
  (when-let ((result (ignore-errors
                      (jsown:filter doc "preview" "images"))))
    (with-html-string
      (dolist (image result)
        (cond ((jsown:val-safe image "variants")
               (let ((has-mp4 (ignore-errors
                               (jsown:filter image
                                             "variants"
                                             "mp4"
                                             "source"
                                             "url")))
                     (has-gif (ignore-errors
                               (jsown:filter image
                                             "variants"
                                             "gif"
                                             "source"
                                             "url")))
                     (has-still-image (ignore-errors
                                       (jsown:filter image "source" "url"))))
                 (cond (has-mp4
                        (:video :preload "none" :class "img-responsive"
                          :controls 1
                          (:source :src (html-entities:decode-entities has-mp4))))
                       (has-gif
                        (:img :class "img-responsive"
                          :src (html-entities:decode-entities has-gif)))
                       (has-still-image
                        (let ((image (html-entities:decode-entities has-still-image)))
                          (:img :class "img-responsive" :src image))))))))
      (:div "is-embedded-image"))))

(defun render/imgur-image (doc)
  (when-let ((result (when-let ((url (jsown:val-safe doc "url")))
                       (when (ppcre:scan "imgur.com" url)
                         (ppcre:regex-replace "https?://i?.?imgur.com/" url "")))))
    (with-html-string
      (render/selftext-html doc)
      (post-media :link-id (jsown:val-safe doc "id"))
      (:blockquote.imgur-embed-pub :data-id result)
      (:script :src "//s.imgur.com/min/embed.js" :charset "utf-8")
      (:div "imgur-image"))))

(defun render/video (doc)
  (when-let ((result (when-let ((url (jsown:val-safe doc "url")))
                       (when (ppcre:scan ".mp4|.MP4" url)
                         url))))
    (with-html-string
      (:video :class "img-responsive" :preload "none" :controls "1"
        (:source :src (html-entities:decode-entities result)))
      (:div "url-is-video"))))

(defun render/url-image (doc)
  (when-let ((result (when-let ((url (jsown:val-safe doc "url")))
                       (when (ppcre:scan ".jpg|.png$|.gif$|.JPG$|.PNG$|.GIF$" url)
                         url))))
    (with-html-string
      (:img :class "img-responsive lozad" :data-src result)
      (:div "url-image")
      (:script
        (:raw
         (ps:ps
           (with-document-ready
               (lambda ()
                 (-> observer (observe))))))))))

(defun render/crosspost-parent-media (doc)
  (when-let ((result (let ((crosspost-parent-list
                             (jsown:val-safe doc "crosspost_parent_list")))
                       (when (listp crosspost-parent-list)
                         (ignore-errors
                          (jsown:filter (car crosspost-parent-list)
                                        "preview" "reddit_video_preview" "fallback_url"))))))
    (with-html-string
      (:video :preload "none" :class "img-responsive" :controls 1
        (:source :src result))
      (:div "has-crossposted-parent-media"))))

(defun render/oembed-media (doc)
  (when-let ((result (ignore-errors
                      (jsown:filter doc "secure_media" "oembed" "html"))))
    (with-html-string
      (let ((decoded (ppcre:regex-replace
                      "new.reddit.com"
                      (html-entities:decode-entities result)
                      "theon.local")))
        (:div :class "youtube-container"
          (:raw decoded))
        (:div "has-oembed-media")))))

(defun render/selftext (doc)
  (when-let ((result (jsown:val-safe doc "selftext")))
    (with-html-string
      (:raw (html-entities:decode-entities result))
      (:div (:div "selftext")))))

(defun render/just-a-link (doc)
  (when-let ((url (jsown:val-safe doc "url_overridden_by_dest")))
    (with-html-string
      (:a :target "farts" :href url "Link only")
      (:span "just-a-link"))))

(defun render/just-an-image (doc)
  (when-let ((image (ignore-errors
                     (jsown:filter (car (jsown:filter doc "preview" "images"))
                                   "source" "url"))))
    (with-html-string
      (post-media :link-id (jsown:val doc "id"))
      (:span "just-an-image"))))

(defparameter *render-map*
  (list 'render/url-is-mp4
        'render/selftext-html
        'render/crosspost-parent-media
        'render/imgur-gifv
        'render/imgur-image
        'render/oembed-media
        'render/reddit-preview-of-imgur-gif
        'render/reddit-video
        'render/video
        'render/url-image
        'render/embedded-image
        'render/just-a-link
        'render/just-an-image
        'render/selftext
        'render/crossposted-media-content
        'render/crossposted-reddit-video))

(defun render-post (doc)
  (let ((id (jsown:val doc "id")))
    (with-html-string
      (:div :class "row" :style "min-height:500px;" :id (format nil "body-~a" id)
        (:div.col-md-12
         (dolist (renderer *render-map*)
           (when-let ((result (funcall (symbol-function renderer) doc)))
             (return-from render-post result)))
         (:span :style "font-size:60px;color:red;font-weight:bold;"
           "WTF IS THIS"))))))

(defroute show-link-body ("/link/body/:id") ()
  (when-let (link (with-pg (get-dao 'link id)))
    (when-let ((bulk (link-bulk link)))
      (unless (eq bulk :null)
        (let ((doc (jsown:parse bulk)))
          (render-post doc))))))

;;
;; 6809 Guru Meditation
;;

(defroute make-history ("/graph/history/:id") ()
  (setf (hunchentoot:content-type*) "application/json")
  (if-let ((history (with-pg (get-dao 'up-history id))))
    (jsown:to-json (up-history-scores history))
    "[]"))

(defroute ajax-link ("/singlelink/:id") ()
  (when-let ((link (with-pg (get-dao 'link id))))
    (with-html-string (display-link link)))
  ;; (when-let ((item (get-item id)))
  ;;   (with-html-string (display-link item)))
  )

(defroute authors-comments ("/comments/author/:author") ()
  (with-page ()
    (dolist (comment (with-pg (select-dao 'comment (:and (:not (:like 'parent "t3_%"))
                                                         (:= 'author author)))))
      (:div :class "col-md-6 col-md-offset-3"
        :style "padding-top:10px;padding-bottom:10px;margin-bottom:10px;background:white;"
        (display-comment comment)))))

(defroute mine ("/mine") ()
  (display-links
   (sort (mapcar (lambda (realid)
                   (with-pg (get-dao 'link (ppcre:regex-replace "^t3_" realid ""))))
                 (remove-duplicates
                  (remove-if-not (lambda (thing)
                                   (ppcre:scan "^t3_" thing))
                                 (with-pg
                                   (query
                                    (:select 'parent
                                      :from 'comment
                                      :where (:= 'author "clintm"))
                                    :column)))))
         #'> :key #'link-created-utc)))

(defroute live ("/live") ()
  (links-page
   (:div :id "graphs" :class "col-md-12")
   (:div :id "loader")
   (:div :id "scratch" :style "display:none;")
   (:script
     (:raw
      (ps:ps*
       `(progn
          (defun ws-connect ()
            (setf (ps:@ window ws)
                  (ps:new (-web-socket (+ "ws://" (ps:@ window location hostname) ":9001/ws/news"))))
            (setf (ps:@ ws onopen)
                  (lambda ()
                    (-> console (log "Connected."))
                    (set-interval (lambda ()
                                    (-> ws (send "ping")))
                                  40000)))
            (setf (ps:@ ws onerror) (lambda (event)
                                      (-> console (log (concatentate 'string
                                                                     "Websockets error"
                                                                     (ps:@ event data)
                                                                     "\n")))))
            (setf (ps:@ ws onclose) (lambda ()
                                      (-> console (log "Connection closed."))))
            (setf (ps:@ ws onmessage)
                  (lambda (event)
                    (if (string= (ps:@ event data) "pong")
                        (-> console (log "pongers")))
                    (if (string= (ps:@ event data) "ping")
                        (-> (ps:@ window ws)
                            (send "pong"))
                        (progn
                          (let ((env (-> -j-s-o-n (parse (ps:@ event data)))))
                            (when (string= (ps:@ env action) "update-link")
                              (let* ((the-id (ps:@ env id))
                                     (the-selector (+ "#wx" the-id)))
                                (when (< 0 (ps:@ (sel the-selector) length))
                                  (-> (sel the-selector) (remove))
                                  (add-link the-id))))
                            (when (string= (ps:@ env action) "remove-link")
                              (remove-link (ps:@ env id)))
                            (when (string= (ps:@ env action) "add-link")
                              (add-link (ps:@ env id))
                              (update-graph (ps:@ env id))
                              (update-post (ps:@ env id)))
                            (when (string= (ps:@ env action) "update-graph")
                              (update-graph (ps:@ env id))
                              (update-post (ps:@ env id)))))))))

          (defun reset-interface ()
            (-> (ps:@ window ws) (send "reset-interface")))

          (defun update-all-graphs ()
            (map (lambda (element)
                   (let ((this-element (-> (ps:@ element id) (replace (ps:regex "/^wx/") ""))))
                     (update-graph this-element)))
                 (-> (sel ".post"))))

          (defun remove-link (link-id)
            (-> (sel (+ "#wx" link-id))
                (remove)))

          (defun add-link (link-id)
            (-> $ (ajax (ps:create :type "get"
                                   :url (concatenate 'string "/singlelink/" link-id)
                                   :error (lambda (e)
                                            (-> console (log e)))
                                   :success (lambda (text)
                                              (-> (sel "#loader")
                                                  (append text)))))))

          (with-document-ready (lambda ()
                                 (-> (sel window) (resize (lambda ()
                                                            (update-all-graphs))))))

          (-> ($ document)
              (ready
               (lambda ()
                 (ws-connect))))))))))

(defroute nsfw ("/nsfw") ()
  (display-links
   (with-pg (select-dao 'link (:and (:!= 'shadow t)
                                    (:= 'nsfw t))
                        (:desc 'created-utc)))))

(defroute shadowed ("/shadowed") ()
  (display-links
   (with-pg (query (:limit (:order-by (:select 'id :from 'link :where (:= 'shadow t))
                                      (:desc 'created_utc))
                           100)
                   :column))))

(defun get-links-by-text (searchtext)
  (with-pg
    (query (:select '* :from 'link
             :where (:and (:= 'shadow nil)
                          (:raw (format nil "ts_index @@ websearch_to_tsquery('english','~a')"
                                        searchtext))))
           (:dao link))))

(defroute search-h ("/search" :method :post) (pattern)
  (log:info "Searching... ~a" pattern)
  (links-page
   (dolist (link (get-links-by-text pattern))
     (display-link link))))

(defun check-file (filename)
  (let ((hash (car (ppcre:split "\\."
                                (file-namestring
                                 (path:basename filename))))))
    (unless (with-pg (query (:select 'hash :from 'media
                              :where (:= 'hash hash))))
      (log:info "Cleaning up ~a" filename)
      (delete-file filename))))


(defun media-checker ()
  (flet ((check-media (temp-id)
           (let ((media (with-pg (get-dao 'media (car temp-id)))))
             (unless (probe-file (media-path media))
               (with-pg (delete-dao media))))))
    (with-lparallel (:workers 4)
      (lparallel:pmapcar #'check-media (with-pg (query (:select 'id :from 'media)))))))

;; #+lispworks
;; (capi:define-interface wutface-interface () ()
;;   (:panes
;;    (browser capi:browser-pane
;;             :min-height 600
;;             :min-width 800
;;             :url "http://localhost:9000"))
;;   (:layouts
;;    (main-layout capi:column-layout
;;                 '(row-with-editor-pane))
;;    (row-with-editor-pane capi:row-layout '(browser)))
;;   ;; (:menus
;;   ;;  (file-menu "File" (("Open"))
;;   ;;             :selection-callback 'file-choice)
;;   ;;  (page-menu "Page"
;;   ;;             (("Page Up" :selection-callback 'scroll-up)
;;   ;;              ("Page Down" :selection-callback 'scroll-down))))
;;   ;; (:menu-bar file-menu page-menu)
;;   )

;; #+lispworks
;; (defun interface-main ()
;;   (start)
;;   (capi:display (make-instance 'wutface-interface))
;;   (capi:contain (make-instance 'capi:listener-pane
;;                                :title "Listener")))






;; "hacker" news...

;; (eval-when (:compile-toplevel :load-toplevel :execute)
;;   (defclass item ()
;;     ((id :json-key "id")
;;      (deleted :json-key "deleted")
;;      (type :json-key "type")
;;      (by :json-key "by")
;;      (time :json-key "time")
;;      (text :json-key "text")
;;      (dead :json-key "dead")
;;      (parent :json-key "parent")
;;      (poll :json-key "poll")
;;      (kids :json-key "kids"
;;            :json-type :list)
;;      (url :json-key "url")
;;      (score :json-key "score")
;;      (title :json-key "title")
;;      (parts :json-key "parts")
;;      (descendants :json-key "descendants"))
;;     (:metaclass json-serializable-class)))


;; (defparameter *hn-cache* (cache:make-cache-table :size 100000
;;                                                  :test #'equal))

;; (defun get-item (item-id)
;;   (handler-case
;;       (cache:cache-table-get-or-fill item-id
;;                                      *hn-cache*
;;                                      #'(lambda (item-id)
;;                                          (json-mop:json-to-clos
;;                                           (raw-http-request
;;                                            (format nil "https://hacker-news.firebaseio.com/v0/item/~a.json"
;;                                                    item-id))
;;                                           'item))
;;                                      :expire (* 60 60 24))
;;     (sb-pcl::no-applicable-method-error (condition)
;;       (declare (ignore condition))
;;       nil)))

(defun start ()
  (read-config)
  (start-server)
  (start-ws-server)
  (start-ws-reader-thread)
  ;;(start-refresh-threads)
  (dotimes (x 5)
    (start-media-worker)))

;; (defun copy-bulks-to-couchdb ()
;;   (let ((link-ids (with-pg (query (:select 'id :from 'link)
;;                                   :column))))
;;     (with-lparallel (:workers 8)
;;       (lparallel:pmap nil (lambda (id)
;;                             (let ((link (with-pg (get-dao 'link id))))
;;                               (mango:doc-put "testing" (link-bulk link))))
;;                       link-ids))))
