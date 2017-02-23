
(defpackage #:net-from-file
  (:use #:cl #:net)
  (:export #:file->net
           #:res->file))

(in-package #:net-from-file)

(defun file->net (file)
  (let ((net (make-net))) 
    (with-net net
      (with-open-file (stream file)
        (let* ((videos (read stream))
               (endpoints (read stream))
               (requests (read stream))
               (caches (read stream))
               (cache-size (read stream)))
          (declare (ignore caches))
          (@set-cache-size cache-size)
          (dotimes (id videos)
            (@add-video id (read stream)))
          (dotimes (endp-id endpoints)
            (let ((data-c-latency (read stream))
                  (caches-number (read stream)))
              (@add-endpoint
               endp-id
               data-c-latency
               (loop for i from 0 below caches-number by 1
                     collecting (cons (read stream) (read stream))))))
          (dotimes (req-id requests)
            (@add-request (read stream) (read stream) (read stream))))))
    net))

(defun result->file (result file-name)
  (with-open-file (stream file-name :direction :output)
    (format stream "~A~%" (hash-table-size result))
    (maphash (lambda (cache videos)
               (format t "~A ~{~A~^ ~}~%" cache videos))
             result)))
