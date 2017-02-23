
(defpackage #:net-from-file
  (:use #:cl #:net)
  (:export #:file->net))

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
          (@set-cache-size cache-size)
          (dotimes (id videos)
            (@add-video id (read stream)))
          (dotimes (endp-id endpoints)
            (let ((data-c-latency (read))
                  (caches-number (read)))
              (@add-endpoint
               endp-id
               data-c-latency
               (loop for i from 0 below caches-number by 1
                     collecting (cons (read) (read))))))
          (dotimes (req-id requests)
            (@add-request (read) (read) (read))))))))
