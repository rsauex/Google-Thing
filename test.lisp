(defvar *net*)

(defmacro with-net (net &body body)
  `(let ((*net* ,net))
     ,@body))

(defstruct connection
  endpoint
  latency
  cache-id) ;; if cache-id = t, connection is to data center

(defstruct endpoint
  connections
  vid-req)

(defstruct cache
  connection)

(defstruct videos
  endpoint-requests
  total-requests
  size)

(defstruct net
  (endpoints-ht (make-hash-table))
  (caches-ht (make-hash-table))
  (videos-ht (make-hash-table)))

(defun @add-video (id size)
  (setf (gethash id (net-videos-ht *net*)) (make-videos :size size)))

;; (defun @add-endpoint (id latency cache-ids-list)
;;   (setf (gethash id ())))

;; (defun parse-input-file (file)
;;   (with-open-file (stream file :direction :input)
;;     (let ((first-line (read-line stream)))
;;       )
;;     ;; (do (line (read-line stream :eof-error-p nil :eof-value :eof) (read-line stream :eof-error-p nil :eof-value :eof)))
;;     ))
