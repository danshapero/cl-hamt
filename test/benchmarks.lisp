
(in-package #:cl-hamt-test)

(defvar state (make-random-state t))

(defun random-set (n)
  (do ((i 0 (1+ i))
       (set (empty-set) (set-insert set
                                    (random (ash 1 31) state))))
      ((= i n) set)))

(defmacro get-timing (trials form)
  (let ((start (gensym)))
    `(let ((,start (get-internal-run-time)))
       (progn
         (dotimes (,(gensym) ,trials) ,form)
         (float (/ (- (get-internal-run-time) ,start) ,trials))))))

;; TODO: test more operations
;; TODO: find some portable way to force a GC

(defun asymptotic-runtime (log-max-size &optional (trials 60))
  "Test the asymptotic runtime of HAMT operations by filling a hash-set
with size = 32, 64, ..., 2 ^ log-max-size random numbers, and return a
list of the execution times / size*log(size)."
  (loop for k from 5 below log-max-size
        collect (let ((n (ash 1 k)))
                  (/ (get-timing trials (random-set n))
                     (* n k)))))
