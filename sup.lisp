;;; -*- mode: Lisp; Syntax: common-lisp; Package: sup; -*-

;; 🆑 or gtfo

(defpackage :sup
  (:use #:cl
        #:json-mop
        #:easy-routes
        #:spinneret
        #:cl-hash-util)
  (:import-from :alexandria :when-let :when-let*
                :if-let :read-file-into-string
                :switch :flatten
                :alist-hash-table))

(in-package #:sup)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (let ((*config* (yason:parse (alexandria:read-file-into-string
                                (asdf:system-relative-pathname :sup "config.json")))))
    (defvar *client-id* (gethash "client-id" *config*))
    (defvar *client-secret* (gethash "client-secret" *config*))
    (defvar *subreddits-cache* nil)
    (setf mango:*username* "admin")
    (setf mango:*password* "fl33j0b")
    (setf mango:*host* "127.0.0.1")
    (setf mango:*port* 5984)
    (setf yason:*parse-json-booleans-as-symbols* t)
    (setf hunchentoot:*catch-errors-p* t)
    (push (cons "application" "json") drakma:*text-content-types*)))

(defmacro cq (&rest args)
  `(gethash "docs"
            (yason:parse
             (mango:couch-query "reddit" ,@args))))


(defun update-subreddits-cache ()
  (setf *subreddits-cache*
        (cq '(("type" . "subreddit"))
            :sort (list (alist-hash-table
                         (list (cons "display_name" "asc"))))))
  nil)


(defvar *reddit-user* nil)

(defvar *listener* nil)


(defvar *mailbox*
  #+lispworks (mp:make-mailbox :name "New articles mailbox")
  #+sbcl (sb-concurrency:make-mailbox :name "New articles." ))

(defvar *fetchbox*
  #+lispworks (mp:make-mailbox :name "Image fetch box")
  #+sbcl (sb-concurrency:make-mailbox :name "Image fetch box"))


(defun has-seen-p (name)
  (car (cq `(("name" . ,name)
             ("type" . "link")))))

(defun add-seen (id)
  (unless (has-seen-p id)
    (mango:doc-put "reddit"
                   (to-json (alist-hash-table
                             (list (cons "id" id)
                                   (cons "type" "seen")))))))


(defun make-author-link (link-hash)
  (with-html
    (:div.icon
     (:a :target (format nil "~aauthorlink" (gethash "author" link-hash))
       :href (format nil "/author/~a" (gethash "author" link-hash))
       (:i :class "s7-user-female")))))

;; (defmethod make-author-link ((comment comment))
;;   (with-slots (author) comment
;;     (with-html
;;       (:div.icon
;;        (:a :target (format nil "~aauthorcomments" author)
;;          :href (format nil "/comments/author/~a" author)
;;          (:i :class "s7-note"))))))

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
  (declare (optimize (debug 3)))
  (multiple-value-bind (data code)
      (handler-case
          (drakma:http-request url
                               :additional-headers (list
                                                    (cons "User-Agent" "supyawl:0.2 (by /u/clintm")
                                                    (cons "X-BULL" "Ferdinand")
                                                    (cons "X-BULL-FIGHTS" "No, he's a peaceful bull."))
                               :external-format-in :utf-8
                               :external-format-out :utf-8)
        (drakma::drakma-simple-error (condition)
          (log:info "~a" condition)
          (raw-get-reddit url))
        (usocket:timeout-error (condition)
          (log:info "TIMED OUT - retrying: ~a" condition)
          (raw-get-reddit url)))
    (switch (code)
      (200 (values data code))
      (503 (values nil code))
      (t (when retry
           (sleep 1)
           (raw-get-reddit url))))))

(defun get-reddit (path &key (retry nil))
  (let ((url (format nil "https://reddit.com~a" path)))
    (raw-get-reddit url :retry retry)))

(defun authenticate ()
  (let ((config (yason:parse (read-file-into-string
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
          (switch (code)
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
     ((ps:@ ($ document) ready) ,@body)))

(ps:defpsmacro jq (selector &body body)
  `(-> (sel ,selector)
       ,@body))

(ps:defpsmacro sel (name)
  `($ ,name))

(ps:defpsmacro $. (name)
  `(ps:@ (sel ,name)))

(ps:defpsmacro -> (&body body)
  `(ps:chain ,@body))

(ps:defpsmacro map (func list)
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
         (:link :rel "stylesheet" :type "text/css" :href "/am/css/style.css")
         (:script :src "/am/lib/jquery/jquery.min.js")
         (:style :type "text/css"
           (:raw (css-lite:css
                   ((".md > p") (:margin-bottom "0px"))
                   ((".am-wrapper") (:padding-top "0px !important"))
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
    (when-let ((post-data (gethash "data" (yason:parse
                                                (reddit-request "/subreddits/mine")))))
      (hash-extract "data"
                    (gethash "children" post-data)))))

(defun sync-subreddits ()
  (if-let ((reddit-subreddits (get-reddit-subreddits)))
    (let* ((reddit-names (hash-extract "name" reddit-subreddits))
           (our-names
             (hash-extract "name"
                           (cq `(("name" . ,(alist-hash-table
                                             (list (cons "$regex" "^t5_*")))))))))

      ;; Confirm that all subscribed subreddits are in the database.
      (dolist (reddit-name reddit-names)
        (unless (member reddit-name our-names :test #'string=)
          (let ((reddit-hash (car (remove-if-not #'(lambda (x)
                                                     (string= (gethash "name" x) reddit-name))
                                                 reddit-subreddits))))
            (setf (gethash "type" reddit-hash) "subreddit")
            (mango:doc-put "reddit" (to-json reddit-hash))
            (log:info "Missing ~a" (gethash "display_name" reddit-hash)))))

      ;; Remove db subreddits that aren't currently subscribed.
      (dolist (name our-names)
        (unless (member name reddit-names :test #'string=)
          (let ((stale-record (car (cq `(("name" . ,name))))))
            (mango:doc-delete "reddit" (gethash "_id" stale-record) (gethash "_rev" stale-record))

            (log:info "Removing ~a" name)))))
    (log:info "Failed")))

;; (defun make-history-url (subreddit first-id)
;;   (format nil "https://www.reddit.com~anew.json?limit=100&before=~a"
;;           (subreddit-url subreddit) (format nil "t3_~a" first-id)))

;; (defmethod get-latest-link-for-subreddit ((subreddit subreddit))
;;   (car (flatten
;;         (with-pg (query (:limit (:order-by (:select 'id :from 'link
;;                                              :where (:= 'subreddit (subreddit-display-name subreddit)))
;;                                            (:asc 'created_utc))
;;                                 1))))))

;; (defun make-new-url (subreddit)
;;   (format nil "~anew/.json?limit=100" (subreddit-url subreddit)))


(defun is-repostp (link-hash)
  (< 0 (length
        (cq `(("name" . ,(gethash "name" link-hash)))))))

(defun handle-possible-new-link (link-hash &key (update t))
  ;; (when (is-repostp link-hash)
  ;;   (return-from handle-possible-new-link))
  (if-let ((existing-link (car (cq (list (cons "name" (gethash "name" link-hash)))))))
    (progn (when update
             (setf (gethash "type" link-hash) "link")
             (setf (gethash "_id" link-hash) (gethash "_id" existing-link))
             (setf (gethash "_rev" link-hash) (gethash "_rev" existing-link))
             (setf (gethash "scores" link-hash) (append (gethash "scores" existing-link)
                                                        (list (gethash "score" link-hash))))
             (if (gethash "hidden" existing-link)
               (setf (gethash "hidden" link-hash)
                     (gethash "hidden" existing-link))
               (setf (gethash "hidden" link-hash) nil))
             (mango:doc-put "reddit" (to-json link-hash)))
           (unless (gethash "hidden" link-hash nil)
             (send-update-graph link-hash)))

    (unless (has-seen-p (gethash "name" link-hash))
      (log:info "~a ~a"
                (gethash "subreddit" link-hash)
                (gethash "title" link-hash))
      (setf (gethash "type" link-hash) "link")
      (setf (gethash "hidden" link-hash) nil)
      (setf (gethash "scores" link-hash) (list (gethash "score" link-hash)
                                               (gethash "score" link-hash)))
      (mango:doc-put "reddit" (to-json link-hash))
      (add-seen (gethash "name" link-hash))
      (#+lispworks mp:mailbox-send
       #+sbcl sb-concurrency:send-message
       *mailbox*
       (to-json (alist-hash-table
                 (list (cons "name" (gethash "name" link-hash))
                       (cons "action" "add-link")))))
      (send-update-graph link-hash))))


(defun sync-subreddit (subreddit-hash)
  "Fetch the entire history of this subreddit."
  (labels ((make-request-url ()
             (format nil "/~a/.json" (gethash "display_name_prefixed" subreddit-hash)))
           (get-next-page (after-id)
             (let* ((new-url (format nil "~a?show=all&limit=100&after=~a" (make-request-url) after-id))
                    (link-hash (yason:parse (get-reddit new-url))))
               (log:info new-url)
               (dolist (link (hu:hash-get link-hash '("data" "children")))
                 (when (string= (gethash "kind" link) "t3")
                   (handle-possible-new-link (gethash "data" link))))
               (when-let ((next-page (hu:hash-get link-hash '("data" "after"))))
                 (get-next-page next-page)))))
    (get-next-page (hu:hash-get (yason:parse (get-reddit (make-request-url)))
                                '("data" "after")))))

(defun get-subreddit-links (subreddit-hash)
  (declare (optimize (debug 3)))
  (labels ((safe-parse (blob)
             (handler-case (yason:parse blob)
               (end-of-file (condition)
                 (log:info "END OF FILE?? WTF: ~a" condition)
                 nil))))
    (multiple-value-bind (data code)
        (handler-case
            (let ((wtf-url (format nil "/~a/.json?limit=100"
                                   (gethash "display_name_prefixed"
                                            subreddit-hash))))
              (get-reddit wtf-url))
          (usocket:timeout-error (condition)
            (declare (ignore condition))
            (log:info "Timeout error.  Restarting.")
            (get-subreddit-links subreddit-hash))
          (error (condition)
            (log:info "Some sort of error:" condition)
            (log:info "Restarting.")
            (get-subreddit-links subreddit-hash)))
      (switch (code)
        (200 (typecase data
               (string (if-let ((json-data (safe-parse data)))
                         (hu:hash-get json-data '("data" "children"))
                         (log:info "Links fetch was null? ~a ~a" code data)))
               (list (log:info "WTF IS THIS: ~a" (flatten data)))))
        (t (log:info "Connecting to reddit failed with: ~a for ~a"
                     code (gethash "display_name" subreddit-hash)))))))


(defun send-update-graph (link-hash)
  (let ((package (to-json (alist-hash-table
                           (list (cons "name" (gethash "_id" link-hash))
                                 (cons "action" "update-graph"))))))
    #+lispworks (mp:mailbox-send *mailbox* package)
    #+sbcl (sb-concurrency:send-message *mailbox* package)))

;; (defun update-all-graphs ()
;;   (dolist (link (with-pg (select-dao 'link (:= 'hidden nil))))
;;     (send-update-graph link)))


;; (defgeneric update-link-comments (link &key refresh))

(defun update-link-comments (link &key (refresh t))
  (unless (and (< 0 (length (cq (list (cons "parent_id" (gethash "name" link)))
                                :limit 1
                                :fields (list "_id"))))
               (not refresh))
    (multiple-value-bind (data code)
        (handler-case (get-reddit (format nil "~acomments.json"
                                          (gethash "permalink" link)) :retry nil)
          (flexi-streams:external-format-encoding-error (condition)
            (log:info "ENCODING ERROR: ~a" condition)
            (values 201 "NOTHING")))
      (switch (code)
        (200 (dolist (comment-hash (yason:parse data))
               (let ((the-listing (ignore-errors (hu:hash-get comment-hash '("data" "children")))))
                 (dolist (listing the-listing)
                   (when (string= (gethash "kind" listing) "t1")
                     (add-or-update-comment (gethash "data" listing)))))))
        (t nil)))))

;; (defmethod update-comment-votes ((comment comment)
;;                                  (id string)
;;                                  (count integer))
;;   (if-let ((maybe-votes (with-pg (get-dao 'comment-votes id))))
;;     (progn
;;       (let ((votes (make-array (length (comment-votes-scores maybe-votes))
;;                                :initial-contents (comment-votes-scores maybe-votes)
;;                                :adjustable t
;;                                :fill-pointer t)))
;;         (vector-push-extend count votes)
;;         (setf (comment-votes-scores maybe-votes) votes)
;;         (with-pg (update-dao maybe-votes))))
;;     (with-pg (insert-dao (make-instance 'comment-votes
;;                                         :id id
;;                                         :scores (vector count))))))




(defun update-subreddit (subreddit)
  (let ((yason:*parse-json-booleans-as-symbols* nil))
    (unless (ppcre:scan "u_" (gethash "display_name" subreddit))
      (dolist (link (hash-extract "data" (get-subreddit-links subreddit)))
        (handle-possible-new-link link)))))

(defun scan-subreddits ()
  (dolist (subreddit (cq (list (cons "type" "subreddit"))))
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

(defun mark-subreddit-as-read (subreddit-display-name)
  (let ((subreddit (car (cq (list (cons "display_name" subreddit-display-name)
                                  (cons "type" "subreddit"))
                            :limit 1
                            :fields (list "name")))))
    (let ((links (cq (list (cons "subreddit_id" (gethash "name" subreddit))
                            (cons "type" "link")
                            (cons "hidden" nil))
                     :limit 100000)))
      (dolist (link links)
        (setf (gethash "hidden" link) t)
        (mango:doc-put "reddit" (to-json link)))
      (length links))))

;; (defmethod mark-subreddit-as-unread ((subreddit subreddit))
;;   (with-slots (display-name) subreddit
;;     (dolist (link (with-pg (select-dao 'link (:and (:= 'subreddit display-name)
;;                                                    (:= 'hidden t)))))
;;       (setf (link-hidden link) nil)
;;       (with-pg (update-dao link)))))

(defun display-link (link-hash &key (public))
  (let ((name (gethash "name" link-hash)))
    (with-html
      (let ((unique-id (gethash "_id" link-hash)))
        (:div :class "col-md-4 post" :id (format nil "wx~a" unique-id)
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
                (make-author-link link-hash)
                (:a :style "font-weight:bold;"
                  :target (format nil "win-~a" unique-id)
                  :href (format nil "/link/~a" unique-id)
                  (:div.icon (:span.s7-id)))

                (:div.icon (:a :id (format nil "shadow-~a" unique-id)
                             (:i :class "s7-trash")))
                (:script
                  (ps:ps*
                   `(-> (sel ,(format nil "#shadow-~a" unique-id))
                        (click (lambda (e)
                                 (-> (sel ,(format nil "#wx~a" unique-id)) (toggle))
                                 (-> (sel ,(format nil "#wx~a" unique-id)) (load ,(format nil "/shadow/~a" unique-id)))
                                 (-> e (prevent-default)))))))

                (:div.icon :id (format nil "favorite-~a" unique-id)
                           (:span.s7-download))
                (:div.icon :id (format nil "hide-~a" unique-id)
                           (:span.s7-close-circle))
                (let ((llt (gethash "link_flair_text" link-hash)))
                  (when (and llt
                             ;; WTF IS THIS
                             (not (string= "false" llt)))
                    (let ((background-color (if-let ((bg-color (let ((wuzit (gethash "link_flair_background_color" link-hash)))
                                                                 (and (< 0 (length wuzit))
                                                                      wuzit))))
                                              bg-color
                                              "orange"))
                          (color (if-let ((tcolor (gethash "link_flair_text_color" link-hash)))
                                   (if (string= "dark" tcolor)
                                     "black"
                                     tcolor))))
                      (:br)
                      (:span :class "label pull-right"
                        :style (format nil "font-size:10px;background-color:~a;color:~a"
                                       background-color color)
                        (ppcre:regex-replace-all "&amp;"
                                                 (gethash "link_flair_text" link-hash)
                                                 "&")))))))
             (:a :target (format nil "win-~a" (uuid:make-v4-uuid))
               :href (gethash "url" link-hash)
               (html-entities:decode-entities
                (gethash "title" link-hash)))
             (:br)
             (:div
               (:a :target (format nil "win-~a" (uuid:make-v4-uuid))
                 :href (format nil "https://www.reddit.com~a" (gethash "permalink" link-hash))
                 (format nil "~a in ~a on ~a"
                         (gethash "author" link-hash)
                         (gethash "subreddit" link-hash)
                         (with-output-to-string (sink)
                           (local-time:format-timestring
                            sink
                            (local-time:unix-to-timestamp
                             (rationalize (gethash "created_utc" link-hash)))
                            :format (list :month "/"
                                          :day "/"
                                          :year " "
                                          :hour ":"
                                          :min ":" :sec)))))
               (dolist (award (gethash "all_awardings" link-hash))
                 (:img
                   :style "max-height:13px;"
                   :title (gethash "name" award)
                   :src (gethash "icon_url" award))))
             (:div :class "col-md-12")
             (:script
               (ps:ps*
                `(with-document-ready
                     (lambda ()
                       (-> (sel ,(format nil "#favorite-~a" unique-id))
                           (click (lambda (e)
                                    (ps:chain (sel ,(format nil "#wx~a" unique-id))
                                              (load ,(format nil "/link/favorite/~a" unique-id)))
                                    (-> (sel ,(format nil "#wx~a" unique-id))
                                        (toggle))
                                    (-> e (prevent-default)))))
                       (-> (sel ,(format nil "#hide-~a" unique-id))
                           (click (lambda (e)
                                    (let ((selector ,(format nil "#wx~a" unique-id)))
                                      (ps:chain (sel selector)
                                                (load ,(format nil "/link/hide/~a" (hunchentoot:url-encode unique-id))))
                                      (-> (sel selector) (remove))
                                      (-> e (prevent-default)))))))))))
           (if (gethash "nsfw" link-hash)
             (let ((nsfw-button-id (format nil "nsfw~a" unique-id)))
               (:button.btn-primary.btn-xs :id nsfw-button-id
                                           :style "margin-top:5px;margin-left:5px;"
                                           :onclick (ps:ps*
                                                     `(progn
                                                        (-> (sel ,(format nil "#link-~a" unique-id))
                                                            (load ,(format nil "/link/body/~a" unique-id)))
                                                        (-> (sel ,(format nil "#~a" nsfw-button-id)) (hide))))
                                           "NSFW"))
             (:script
               (ps:ps*
                `(with-document-ready
                     (lambda ()
                       (-> (sel ,(format nil "#link-~a" unique-id))
                           (load ,(format nil "/link/body/~a" name)))
                       (update-graph ,(gethash "_id" link-hash)))))))
           (:div.panel-body :id (format nil "graph~a" unique-id)
                            :style "height:30px;margin-bottom:5px;")
           (:div.panel-body :class "link-body"
                            :style "padding: 10px 15px 15px;"
                            :id (format nil "link-~a" unique-id))
           (:div :class "panel-heading"
             (:div
               :class "icon comment-trigger"
               :onclick (ps:ps* `(if (string= (-> (sel ,(format nil "#comments-~a" unique-id))
                                                  (css "display"))
                                              "none")
                                   (progn
                                     (-> (sel ,(format nil "#comments-~a" unique-id))
                                         (toggle))
                                     (-> (sel ,(format nil "#comments-~a" unique-id))
                                         (load ,(format nil "/comments/~a" name))))
                                   (-> (sel ,(format nil "#comments-~a" unique-id))
                                       (toggle))))

               (:span :class "s7-download")))
           (:div.panel-body :id (format nil "comments-~a" unique-id)
                            :style "display:none;"
                            "Loading...")))))))

(defun fetcher-is-running-p ()
  (member "fetchers" (mapcar #'bt:thread-name
                             (bt:all-threads))
          :test #'string=))

;; (defun mark-everything-unread ()
;;   (dolist (subreddit (with-pg (select-dao 'subreddit)))
;;     (mark-subreddit-as-unread (subreddit-display-name subreddit))))

;; (defun mark-everything-read ()
;;   (dolist (subreddit (with-pg (select-dao 'subreddit)))
;;     (mark-subreddit-as-read subreddit)))

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
               (:a
                 :target (format nil "wx~a" (gethash "name" subreddit))
                 :class "btn btn-primary btn-xs"
                 :href (format nil "/subreddit/~a/0" (gethash "name" subreddit))
                 (gethash "display_name" subreddit)))))))
     ,@body
     (:script
       (:raw
        (ps:ps
          (defun update-graph (link-id)
            (-> $ (ajax (ps:create :type "get"
                                :url (concatenate 'string "/graph/history/" link-id)
                                :success (lambda (obj)
                                           (-> (sel (concatenate 'string "#graph" link-id))
                                               (sparkline obj
                                                          (ps:create :width "100%"
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

(defun display-comment (comment)
  (with-html
    (:span.pull-left
     (:a.pull-left :style "padding-left:5px;" :href (format nil "https://reddit.com/u/~a" (gethash "author" comment))
                   (gethash "author" comment))
     (when-let ((total-awards (gethash "total_awards_received" comment))
                (gilded (gethash "gilded" comment)))
       (:div.pull-left :style "padding-right:5px;padding-left:5px;" (format nil " (~a/~a)" gilded total-awards)))
     (dolist (award (gethash "all_awardings" comment))
       (:img.pull-left
        :style "max-height:17px;"
        :title (gethash "name" award)
        :src (gethash "icon_url" award))))
    (:span.pull-right
     (when (gethash "flair_text" comment)
       (:span.text-warning :style "padding-left:4px;padding-right:4px;margin-left:5px;background-color:#444444;"
                           (gethash "flair_text" comment)))
     ("~a" (gethash "score" comment)))
    (:br)
    (:div :style "margin-left:5px;"
      (:raw (html-entities:decode-entities (gethash "body_html" comment))))
    (dolist (reply (cq `(("parent_id" . ,(gethash "name" comment)))))
      (:div :style "border-left:1px solid darkgrey;padding-left:5px;"
        (display-comment reply)))))

(defun add-or-update-comment (comment-hash)
  (dolist (comment (cq `(("name" . ,(gethash "name" comment-hash)))))
    (mango:doc-delete "reddit" (gethash "_id" comment) (gethash "_rev" comment)))
  (mango:doc-put "reddit" (to-json comment-hash))
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

(defgeneric mailbox-send (client message &rest args))

(defmethod mailbox-send ((user user) (message string) &rest args)
  (handler-case
      (hunchensocket:send-text-message user (apply #'format nil message args))
    (error (condition)
      (log:info "ERROR sending websocket message: ~a" condition)
      nil)))

(defun broadcast (room message &rest args)
  (dolist (client (hunchensocket:clients room))
    (mailbox-send client message args)))

(defun new-articles-to-live ()
  (dolist (link (cq (list (cons "type" "link")
                          (cons "hidden" nil))
                    :fields (list "name")
                    :sort (list (alist-hash-table
                                 (list (cons "created_utc" "asc"))))
                    :limit 500))
    (#+lispworks mp:mailbox-send
     #+sbcl sb-concurrency:send-message
     *mailbox*
     (to-json
      (alist-hash-table
       (list (cons "action" "add-link")
             (cons "name" (gethash "name" link))))))))

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
                      (if-let ((message (#+lispworks mp:mailbox-read
                                         #+sbcl sb-concurrency:receive-message
                                         *mailbox*)))
                        (handler-case (broadcast (car *chat-rooms*) message)
                          (error (condition)
                            (log:info "Error broadcasting message: ~a" condition)))
                        (sleep 1))))
                  :name "ws broadcast"))

(defun start-ws-server ()
  (unless *ws-server*
    (setf *ws-server* (make-instance 'hunchensocket:websocket-acceptor :port 8088))
    (hunchentoot:start *ws-server*)))

(defun download-entire-internet ()
  (dolist (subreddit (cq '(("type" . "subreddit"))))
    (sync-subreddit subreddit)))

;; (defun mark-everything-older-than-a-month-as-read ()
;;   (dolist (linkid (flatten
;;                    (with-pg (query (:order-by (:select 'id :from 'link :where
;;                                                 (:and (:= 'hidden nil)
;;                                                       (:>= (- (cl-ivy:epoch-time) (* 2 24 60 60))
;;                                                            'created_utc)))
;;                                               (:desc 'created_utc))))))
;;     (let ((the-link (with-pg (get-dao 'link linkid))))
;;       (setf (link-hidden the-link) t)
;;       (with-pg (update-dao the-link)))))

;; (defun start-comment-update-thread ()
;;   (bt:make-thread (lambda ()
;;                     (get-all-the-damned-comments))
;;                   :name "comment fetcher"))



(defroute stop-threads ("/stop-fetchers") ()
  (stop-refresh-threads)
  (hunchentoot:redirect "/live"))

(defroute start-threads ("/start-fetchers") ()
  (start-refresh-threads)
  (hunchentoot:redirect "/live"))

;; (defroute get-votes ("/votes/:id") ()
;;   (setf (hunchentoot:content-type*) "application/json")
;;   (if-let ((up-history (with-pg (get-dao 'up-history id))))
;;     (with-output-to-string (sink)
;;       (yason:encode (up-history-scores up-history) sink))
;;     "[]"))

;; (defroute show-author ("/author/:author") ()
;;   (display-links (with-pg (query (:order-by
;;                                   (:select '* :from 'link :where (:= 'author author))
;;                                   (:desc 'created-utc))
;;                                  (:dao link)))))

;; (defroute display-single-link ("/link/:id") ()
;;   (with-page ()
;;     (display-link (with-pg (get-dao 'link id)) :public t)))

(defroute display-subreddit ("/subreddit/:subid/:offset" :method :get) ()
  (let* ((offset-i (parse-integer offset))
         (*display-offset* (+ offset-i 500)))
    (display-links
     (cq `(("type" . "link")
           ("subreddit_id" . ,subid))
         :skip offset-i
         :limit 500
         :sort (list (alist-hash-table
                      (list (cons "created_utc" "desc"))))))))

(defroute favorites ("/favorites") ()
  (display-links (cq '(("favorite" . t))
                     :sort (list
                            (alist-hash-table
                             (list (cons "created_utc" "desc")))))))

(defroute shadow-link ("/shadow/:id") ()
  (when-let ((link (yason:parse (mango:doc-get "reddit" id))))
    (setf (gethash "shadow" link) t)
    (setf (gethash "hidden" link) t)
    (mango:doc-put "reddit" (to-json link))
    (with-html-string (:div "ok"))))

(defroute index ("/") ()
  (hunchentoot:redirect "/live"))

(defroute hide-link ("/link/hide/:id") ()
  (when-let ((link (yason:parse (mango:doc-get "reddit" id))))
    (setf (gethash "hidden" link) t)
    (mango:doc-put "reddit" (to-json link))
    "ok"))

(defroute make-favorite ("/link/favorite/:id") ()
  (when-let ((link (yason:parse (mango:doc-get "reddit" id))))
    (setf (gethash "favorite" link) t)
    (setf (gethash "hidden" link) t)
    (mango:doc-put "reddit" (to-json link))
    "ok"))

(defroute show-comments ("/comments/:name") ()
  (when-let ((link (car (cq `(("type" . "link")
                              ("name" . ,name))))))
    (update-link-comments link)

    (if-let ((comments (cq `(("parent_id" . ,(gethash "name" link))))))
      (with-html-string
        (dolist (comment comments)
          (display-comment comment)))
      (with-html-string
        (:div "No comments yet.")))))

(defroute show-link-body ("/link/body/:name") ()
  (labels (
           ;; (drop-leading-spaces (string)
           ;;   (ppcre:regex-replace "^\\s+" string ""))
           ;; (drop-trailing-spaces (string)
           ;;   (ppcre:regex-replace "\\s+$" string ""))
           ;; (trim-spaces (string)
           ;;   (drop-leading-spaces (drop-trailing-spaces string)))
           (has-image-suffix (string)
             (ppcre:scan ".jpg|.png$|.gif$|.JPG$|.PNG$|.GIF$" string))
           (render-link (doc-hash)
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

                       (url-is-imgur-image (when-let ((url (gethash "url" doc-hash)))
                                             (when (ppcre:scan "imgur.com" url)
                                               (ppcre:regex-replace "https?://i?.?imgur.com/" url ""))))

                       (url-is-video (when-let ((url (gethash "url" doc-hash)))
                                       (when (ppcre:scan ".mp4|.MP4" url)
                                         url)))

                       (url-is-image (when-let ((url (gethash "url" doc-hash)))
                                       (when (has-image-suffix url)
                                         url)))

                       (has-crosspost-parent-media (let ((crosspost-parent-list
                                                           (hash-get doc-hash '("crosspost_parent_list"))))
                                                     (when (listp crosspost-parent-list)
                                                       (hash-get (car crosspost-parent-list)
                                                                 '("preview" "reddit_video_preview" "fallback_url"))))))

                   (cond


                     (crossposted-reddit-video
                      (:video :preload "auto" :class "img-responsive" :controls 1
                        (:source :src crossposted-reddit-video))
                      (:div "crossposted-reddit-video"))


                     (reddit-preview-of-imgur-gif
                      (:video :preload "auto" :class "img-responsive" :controls 1
                        (:source :src reddit-preview-of-imgur-gif))
                      (:div "reddit-preview-of-imgur-gif"))

                     (has-oembed-media (:div :class "youtube-container"
                                         (:raw (html-entities:decode-entities has-oembed-media)))
                                       (:div "has-oembed-media"))


                     (url-is-mp4
                      (:video :preload "auto" :class "img-responsive" :controls "1"
                        (:source :src (gethash "url" doc-hash)))
                      (:div "url-is-mp4"))


                     (is-imgur-gifv (let ((thing (ppcre:regex-replace ".gifv" (gethash "url" doc-hash) ".mp4")))
                                      (:video :preload "auto" :class "img-responsive" :controls "1"
                                        (:source :src thing))
                                      (:div "is-imgur-gifv")))

                     (is-reddit-video
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
                                                   (cond (has-mp4
                                                          (:video :preload "auto" :class "img-responsive"
                                                            :controls 1
                                                            (:source :src (html-entities:decode-entities has-mp4))))
                                                         (has-gif
                                                          (:img :class "img-responsive"
                                                            :src (html-entities:decode-entities has-gif)))
                                                         (has-still-image
                                                          (:img :class "img-responsive"
                                                            :src (html-entities:decode-entities has-still-image))))))))
                                        (:div "is-embedded-image"))

                     (url-is-imgur-image (:blockquote.imgur-embed-pub :data-id url-is-imgur-image)
                                         (:script :src "//s.imgur.com/min/embed.js")
                                         (:div "url-is-imgur-image"))


                     (url-is-video (let ((video (html-entities:decode-entities url-is-video)))
                                     (:video :class "img-responsive" :preload "auto" :controls "1"
                                       (:source :src (html-entities:decode-entities url-is-video)))
                                     (:div "url-is-video")))
                     (url-is-image
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
                                   (when-let ((the-url (gethash "url" doc-hash nil)))
                                     (:a :target (format nil "~a" (get-universal-time)) :href the-url the-url))
                                   (:div "selftext"))



                     (t (:div "What the hell is this?")))))))))

    (when-let (link (car (cq `(("type" . "link")
                               ("name" . ,name)))))
      (if-let ((has-crosspost-parent (car (gethash "crosspost_parent_list" link nil))))
        (render-link has-crosspost-parent)
        (render-link link)))))

;; (defroute comment-history ("/graph/comment/history/:id") ()
;;   (setf (hunchentoot:content-type*) "application/json")
;;   (if-let ((history (with-pg (get-dao 'comment-votes id))))
;;     (with-output-to-string (sink)
;;       (yason:encode (comment-votes-scores history) sink))
;;     "[]"))

(defroute make-history ("/graph/history/:id") ()
  (setf (hunchentoot:content-type*) "application/json")
  (if-let ((link (handler-case
                     (yason:parse (mango:doc-get "reddit" id))
                   (mango:unexpected-http-response (condition)
                     (declare (ignore condition))
                     nil))))
    (with-output-to-string (sink)
      (yason:encode (gethash "scores" link) sink))
    "[]"))

;; (defroute make-graph ("/graph/:id") ()
;;   (with-html-string
;;     (:div :class "col-md-3"
;;       (:div :id (format nil "graph~a" id)))))

(defroute ajax-link ("/singlelink/:name") ()
  (with-html-string
    (display-link
     (car (cq `(("name" . ,name)))))))

;; (defroute authors-comments ("/comments/author/:author") ()
;;   (with-page ()
;;     (dolist (comment (with-pg (select-dao 'comment (:= 'author author))))
;;       (:div :class "col-md-6 col-md-offset-3" :style "padding-top:10px;padding-bottom:10px;margin-bottom:10px;background:white;"
;;         (display-comment comment)))))

;; (defroute mine ("/mine") ()
;;   (display-links
;;    (sort (mapcar (lambda (realid)
;;                    (with-pg (get-dao 'link (ppcre:regex-replace "^t3_" realid ""))))
;;                  (remove-duplicates
;;                   (remove-if-not (lambda (thing)
;;                                    (ppcre:scan "^t3_" thing))
;;                                  (flatten (with-pg (query (:select 'parent :from 'comment :where (:= 'author "clintm"))))))))
;;          #'> :key #'link-created-utc)))

(defroute live ("/live") ()
  (links-page
   (:div :id "graphs" :class "col-md-12")
   (:div :id "loader")
   (:script
     (:raw
      (ps:ps*
       `(progn
          (defvar ws nil)
          (defun ws-connect ()
            (defvar ws nil)
            (setf ws (ps:new (-web-socket (+ "ws://" (ps:@ window location hostname) ":8088/ws/news"))))
            (setf (ps:@ ws onopen)
                  (lambda ()
                    (-> console (log "Connected."))
                    (set-interval (lambda ()
                                    (-> ws (send "ping")))
                                  5000)))
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
                      (-> ws (send "pong"))
                      (progn
                        (let ((env (-> -j-s-o-n (parse (ps:@ event data)))))
                          (when (string= (ps:@ env action) "update-link")
                            (let* ((the-id (ps:@ env id))
                                   (the-selector (+ "#wx" the-id)))
                              (when (< 0 (ps:@ (sel the-selector) length))
                                (-> (sel the-selector) (remove))
                                (add-link the-id))))
                          (when (string= (ps:@ env action) "add-link")
                            (add-link (ps:@ env name))
                            (update-graph (ps:@ env name)))
                          (when (string= (ps:@ env action) "update-graph")
                            (update-graph (ps:@ env name)))))))))

          (defun update-all-graphs ()
            (map (lambda (element)
                   (let ((this-element (-> (ps:@ element id) (replace (ps:regex "/^wx/") ""))))
                     (update-graph this-element)))
                 (-> (sel ".post"))))

          (defun update-graph (link-id)
            (-> $ (ajax (ps:create :type "get"
                                   :url (concatenate 'string "/graph/history/" link-id)
                                   :success (lambda (obj)
                                              (-> (sel (concatenate 'string "#graph" link-id))
                                                  (sparkline obj
                                                             (ps:create :width "100%"
                                                                        :height "35"
                                                                        :line-width 1.7))))))))
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

;; (defun get-links-by-text (searchtext)
;;   (with-pg
;;     (query (:select '* :from 'link
;;              :where (:and (:= 'shadow nil)
;;                           (:raw (format nil "to_tsvector('english',title) @@ to_tsquery('english','~a')"
;;                                         searchtext))))
;;            (:dao link))))

;; (defroute search-h ("/search" :method :post) (pattern)
;;   (log:info "Searching... ~a" pattern)
;;   (links-page
;;    (dolist (link (get-links-by-text pattern))
;;      (display-link link))))

(defun start ()
  (start-server)
  ;;(start-refresh-threads)
  (start-ws-server)
  (start-ws-reader-thread))











;; (defun update-types ()
;;   (dolist (document-id (hash-extract "_id"
;;                                      (cq `(("type" . (alist-hash-table
;;                                                       (list (cons "$exists" 'yason:false)))))
;;                                          :limit 200000
;;                                          :fields (list "_id"))))
;;     (let* ((document (yason:parse (mango:doc-get "reddit" document-id)))
;;            (pattern (gethash "name" document))
;;            (type (cond ((ppcre:scan "^t5_*" pattern) "subreddit")
;;                        ((ppcre:scan "^t6_*" pattern) "award")
;;                        ((ppcre:scan "^t4_*" pattern) "message")
;;                        ((ppcre:scan "^t3_*" pattern) "link")
;;                        ((ppcre:scan "^t2_*" pattern) "account")
;;                        ((ppcre:scan "^t1_*" pattern) "comment")
;;                        (t (error "WTF IS THIS")))))
;;       (setf (gethash "type" document) type)
;;       (mango:doc-put "reddit" (to-json document)))))





























(defun copy-old-articles ()
  (declare (optimize (debug 3)))
  (with-open-file (reader "bulks.json")
    (do ((line (read-line reader)))
        (nil)
      (if line
        (let* ((parsed (yason:parse (ppcre:regex-replace-all "\\\\\\\\" line "\\\\")))
               (this-bulk (gethash "bulk" parsed))
               (document-hash (yason:parse this-bulk))
               (document-on-couch (car (cq `(("name" . ,(gethash "name" document-hash)))))))
          (if document-on-couch
            (progn
              (destructuring-bind (favorite hidden shadow)
                  (list (gethash "favorite" document-hash)
                        (gethash "hidden" document-hash)
                        (gethash "shadow" document-hash))
                (when (or favorite hidden shadow)
                  (setf (gethash "favorite" document-on-couch) favorite)
                  (setf (gethash "hidden" document-on-couch) hidden)
                  (setf (gethash "shadow" document-on-couch) shadow)
                  (mango:doc-put "reddit" (to-json document-on-couch)))))
            (progn
              (mango:doc-put "reddit" (to-json document-hash))
              (log:info "IS NOT on couch"))))
        (return)))))
