(in-package :cl-dict)

(defclass value-node ()
  ((key
    :reader node-key
    :initarg :key
    :initform nil)
   (value
    :reader node-value
    :initarg :value
    :initform nil)))

(defclass conflict-node ()
  ((hash
    :reader conflict-hash
    :initarg :hash)
   (alist
    :reader conflict-alist
    :initarg :values
    :initform '())))

(defclass table-node ()
  ((bitmap
    :reader table-bitmap
    :initarg :bitmap
    :initform 0
    :type (unsigned-byte 5))
   (table
    :reader table-array
    :initarg :table
    :initform (make-array 0 :initial-element nil))))


;; Utility functions for operating on HAMTs
(defun get-bits (hash depth)
  "Extract the 5 bits needed for indexing at the present depth"
  (declare (type integer hash depth))
  (ldb (byte 5 (* 5 depth)) hash))

(defun get-index (bits bitmap)
  (logcount (ldb (byte bits 0) bitmap)))


;; Methods for looking up keys in various types of HAMT nodes
(defgeneric %hamt-lookup (node key hash depth test))

(defmethod %hamt-lookup ((node value-node) key hash depth test)
  (if (funcall test (node-key node) key)
      (values (node-value node) t)
      (values nil nil)))

(defmethod %hamt-lookup ((node table-node) key hash depth test)
  (let ((bitmap (table-bitmap node))
        (bits (get-bits hash depth)))
    (if (logbitp bits bitmap)
        (let ((index (get-index bits bitmap)))
          (%hamt-lookup (aref (table-array node) index) key hash (1+ depth) test))
        (values nil nil))))

(defmethod %hamt-lookup ((node conflict-node) key hash depth test)
  (declare (ignore hash depth))
  (assoc key (conflict-alist node) :test test))


;; Methods for inserting inserting key/value pairs into a HAMT
(defgeneric %hamt-insert (node key value hash depth test))

;; Inserting into a value-node either functionally updates the value stored in
;; the current node if the keys match, or creates a conflict node if the keys
;; do not match but their hashes do.
(defmethod %hamt-insert ((node value-node) key value hash depth test)
  (declare (ignore depth))
  (let ((nkey (node-key node)))
    (if (funcall test key nkey)
        (make-instance 'value-node
                       :key key
                       :value value)
        (make-instance 'conflict-node
                       :hash hash
                       :alist (->> '()
                                   (acons key value)
                                   (acons nkey (node-value node)))))))

;; Inserting into a conflict node either updates the value associated to an
;; existing key, or expands the scope of the conflict
(defmethod %hamt-insert ((node conflict-node) key value hash depth test)
  (let ((alist (conflict-alist node)))
    (make-instance 'conflict-node
                   :hash hash
                   :alist (if (assoc key alist)
                              (mapcar (lambda (kv)
                                        (if (funcall test (car kv) key)
                                            (cons key value)
                                            (cons (car kv) (cdr kv))))
                                      alist)
                              (acons key value alist)))))

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

(defmethod %hamt-insert ((node table-node) key value hash depth test)
  (let* ((bitmap (table-bitmap node))
         (array (table-array node))
         (bits (get-bits hash depth))
         (index (get-index bits bitmap)))
    (if (logbitp bits bitmap)
        (%hamt-insert (aref array index) key value hash (1+ depth) test)
        (make-instance 'table-node
                       :bitmap (logior bitmap (ash 1 bits))
                       :table (vec-insert array
                                          (max index 0)
                                          (if (= depth 6)
                                              (make-instance 'value-node
                                                             :key key
                                                             :value value)
                                              (%hamt-insert (make-instance 'table-node
                                                                           :bitmap 0
                                                                           :table (make-array 0))
                                                            key
                                                            value
                                                            hash
                                                            (1+ depth)
                                                            test)))))))

(defclass hamt ()
    ((test
      :reader hamt-test
      :initarg :test
      :initform #'equal)
     (hash
      :reader hamt-hash
      :initarg :hash
      :initform #'cl-murmurhash:murmurhash)
     (table
      :reader hamt-table
      :initarg :table
      :initform (make-instance 'table-node
                               :bitmap 0
                               :table (make-array 0)))))

(defun make-hamt (&optional (test #'equal) (hash #'cl-murmurhash:murmurhash))
  (make-instance 'hamt :test test :hash hash))

(defmethod dict-lookup ((dict hamt) key)
  (%hamt-lookup (hamt-table dict)
                key
                (funcall (hamt-hash dict) key)
                0
                (hamt-test dict)))

(defmethod dict-insert ((dict hamt) key value)
  (make-instance 'hamt
                 :test (hamt-test dict)
                 :hash (hamt-hash dict)
                 :table (%hamt-insert (hamt-table dict)
                                      key
                                      value
                                      (funcall (hamt-hash dict) key)
                                      0
                                      (hamt-test dict))))