(defvar *max-rand* 100.0)

(defstruct result
  cache-ids) ;;list of cache-set; each cache contains list of video

(defstruct cache-set
  cache
  vids)


(defun move (caches vids) 
  )

(defun get-rnd () 
  (/ (+ 1.0 (- (random *max-rand*) 1.0)) *max-rand*))

(defun get-cost (state) 
  (random 1000))



(defun calculate (net) 
  (let ((iter 50)
        (temp 100)    ;; temperature
        (curr '(make-cache-set :cache (make-cache ))) ;; current state
        (curr-cost 0)
        (next-cost 0)
        (next nil)    ;; next state
        (delta 0))
    (loop while (> temp 0) 
       do 
         (format t "temp: ~A~%" temp)
         (loop for i from 1 to iter
            do
              (format t "it: ~A~%" i)
              (setq curr-cost (get-cost curr))
              (setq next (move t t))
              (setq next-cost (get-cost next))
              (setq delta (- curr-cost next-cost))
              (if (> delta 0) 
                  (setq curr next)
                  (when (< (get-rnd) (exp (/ delta temp)))
                    (setq curr next)))
              (setq temp (- temp 1))))
    (format t "total: ~A~%" curr-cost)
    curr))
