(in-package :cl-hamt)

;; Utility functions for operating on HAMTs.

(defun get-bits (hash depth)
  "Extract bits 5*depth : 5*(depth+1) from the number hash."
  (declare (type integer hash depth))
  (ldb (byte 5 (* 5 depth)) hash))

(defun get-index (bits bitmap)
  "Given the 5-bit int extracted from a hash at the present depth, find
the index in the current array corresponding to this bit sequence."
  (logcount (ldb (byte bits 0) bitmap)))

(defun vec-insert (vec pos item)
  (let* ((len (1+ (length vec)))
         (v (make-array len)))
    (loop for i below len do
         (setf (aref v i)
               (cond
                 ((< i pos) (aref vec i))
                 ((> i pos) (aref vec (1- i)))
                 (t item))))
    v))

(defun vec-remove (vec pos)
  (let* ((len (1- (length vec)))
         (v (make-array len)))
    (loop for i below len do
         (setf (aref v i)
               (if (< i pos)
                   (aref vec i)
                   (aref vec (1+ i)))))
    v))

(defun vec-update (vec pos item)
  (let* ((len (length vec))
         (v (make-array len)))
    (loop for i below len do
         (setf (aref v i)
               (if (= i pos)
                   item
                   (aref vec i))))
    v))


