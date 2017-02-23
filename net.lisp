(defpackage #:net
  (:use #:cl)
  (:export #:*net*
           #:with-net
           #:@set-cache-size
           #:@add-video
           #:@add-endpoint
           #:@add-request
           #:make-net))

(defvar *net*)

(defmacro with-net (net &body body)
  `(let ((*net* ,net))
     ,@body))

(defstruct connection
  endpoint-id
  latency
  cache-id) ;; if cache-id = t, connection is to data center

(defstruct endpoint
  connections
  vid-req)

(defstruct cache
  connection)

(defstruct video
  endpoint-requests ; alist
  total-requests
  size)

(defstruct net
  (endpoints-ht (make-hash-table))
  (caches-ht (make-hash-table))
  (videos-ht (make-hash-table))
  cache-size)

(defun @set-cache-size (size)
  (setf (net-cache-size *net*) size))

(defun @add-video (id size)
  (setf (gethash id (net-videos-ht *net*)) (make-video :size size :total-requests 0 :endpoint-requests nil)))

(defun @add-endpoint (id data-center-latency cache-latency-list) ; list of conses: alist (cache-id . latency)
  (let ((connections-list `((,(make-connection :endpoint-id id :latency data-center-latency :cache-id t)))))
    (map nil (lambda (el) ; el is cons of (cache-id . latency)
               (let ((connection (make-connection :endpoint-id id :cache-id (car el) :latency (cdr el))))
                 (push connection connections-list)
                 (setf (gethash (car el) (net-caches-ht *net*)) (make-cache :connection connection))))
         cache-latency-list)
    (setf (gethash id (net-endpoints-ht *net*)) (make-endpoint :connections connections-list))))

(defun @add-request (video-id endpoint-id requests-num)
  (setf (gethash video-id (net-videos-ht *net*)) (make-video :size (video-size (gethash video-id (net-videos-ht *net*)))
                                                             :endpoint-requests (push (cons endpoint-id requests-num)
                                                                                      (video-endpoint-requests (gethash video-id (net-videos-ht *net*))))
                                                             :total-requests (+ (video-total-requests (gethash video-id (net-videos-ht *net*)))))))
