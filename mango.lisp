
(setf mango:*username* "admin")
(setf mango:*password* "fl33j0b")
(setf mango:*host* "192.168.0.21")
(setf mango:*port* 5984)


(defmacro cq (&rest args)
  `(gethash "docs"
            (yason:parse
             (mango:couch-query "reddit" ,@args))))

(defun couch-has-doc (id)
  (gethash "docs"
           (yason:parse (mango:couch-query "reddit" `(("_id" . ,id))
                                           :limit 1))))

(defun copy-links ()
  (dolist (link-pg-id (with-pg (query (:select 'id :from 'link))))
    (let* ((link (with-pg (get-dao 'link (car link-pg-id)))))
      (unless (eq (link-bulk link) :null)
        (let ((document (yason:parse (link-bulk link))))
          (unless (couch-has-doc (gethash "id" document))
            (setf (gethash "_id" document) (gethash "id" document))
            (setf (gethash "shadow" document) (link-shadow link))
            (setf (gethash "favorite" document) (link-favorite link))
            (setf (gethash "hidden" document) (link-hidden link))
            (setf (gethash "type" document) "link")
            (mango:doc-put "reddit" (with-output-to-string (sink)
                                      (yason:encode document sink)))))))))

(mango:defmango mseen reddit
  ((id :accessor mseen-id
       :initarg :id
       :json-type :string
       :json-key "id")))

(mango:defmango msubreddit reddit
  ((url :initarg :url
        :json-type :string
        :json-key "url"
        :accessor msubreddit-url)
   (display-name :initarg :display-name
                 :json-type :string
                 :json-key "display_name"
                 :accessor msubreddit-display-name)))

(mango:defmango mup-history reddit
  ((scores :json-type (:list integer)
           :json-key "scores"
           :accessor mup-history-scores
           :initarg :scores)))

(mango:defmango mcomment-votes reddit
  ((id :initarg :id
       :accessor mcomment-votes-id
       :json-key "id"
       :json-type :string)))


(defun copy-seen ()
  (dolist (seen-id (with-pg (query (:select 'id :from 'seen))))
    (unless (gethash "docs"
                     (yason:parse (mango:couch-query "reddit" `(("type" . "seen")
                                                                ("id" . ,(car seen-id)))
                                                     :limit 1)))
      (let ((new-seen (make-instance 'mseen :id seen-id)))
        (mseen-put new-seen)))))

(defun copy-info ()
  (dolist (doc-id-hash (cq '("type" . "link")
                           :fields (list "_id")
                           :count 500000))
    (let* ((doc-id (gethash "_id" doc-id-hash))
           (doc (mango:doc-get "reddit" doc-id)))
      )))
