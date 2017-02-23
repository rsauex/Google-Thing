(defpackage #:net
  (:use #:cl)
  (:export #:*net*
           #:with-net
           #:@set-cache-size
           #:@add-video
           #:@add-endpoint
           #:@add-request
           #:make-net
           #:pretty-print
           #:@get-cache
           #:@get-endpoint
           #:@get-video
           #:@get-cache-size
           #:@add-cache))

(in-package #:net)

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
  endpoint-requests ; alist -> (( endpoint-id . requests-num ) ... )
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

(defun @add-cache (id endpoint-id latency)
  (setf (gethash id (net-caches-ht *net*)) (make-connection :endpoint-id endpoint-id
                                                            :latency latency
                                                            :cache-id id)))

(defun @get-cache (id)
  (gethash id (net-caches-ht *net*)))

(defun @get-video (id)
  (gethash id (net-videos-ht *net*)))

(defun @get-endpoint (id)
  (gethash id (net-endpoints-ht *net*)))

(defun @get-cache-size ()
  (net-cache-size *net*))

(defun pretty-print (net)
  (with-net net
    (format t "cache-size: ~A~%" (net-cache-size *net*))
    (format t "endpoints-ht:~%")
    (maphash (lambda (key val)
               (format t "~A: ~A~%" key val))
             (net-endpoints-ht *net*))
    (format t "caches-ht:~%")
    (maphash (lambda (key val)
               (format t "~A: ~A~%" key val))
             (net-caches-ht *net*))
    (format t "videos-ht:~%")
    (maphash (lambda (key val)
               (format t "~A: ~A~%" key val))
             (net-videos-ht *net*))))
