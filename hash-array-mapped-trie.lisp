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
    :initarg :alist
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
  (let ((key-val (assoc key (conflict-alist node) :test test)))
    (if key-val
        (values (cdr key-val) t)
        (values nil nil))))


;; Methods for getting the size of HAMT nodes
(defmethod dict-size ((node value-node))
  1)

(defmethod dict-size ((node table-node))
  (loop for node across (table-array node)
     sum (dict-size node)))

(defmethod dict-size ((node conflict-node))
  (length (conflict-alist node)))


;; Methods for inserting key/value pairs into a HAMT
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

(defmethod %hamt-insert ((node table-node) key value hash depth test)
  (let* ((bitmap (table-bitmap node))
         (array (table-array node))
         (bits (get-bits hash depth))
         (index (get-index bits bitmap))
         (hit (logbitp bits bitmap))
         (new-node
          (cond
            (hit
             (%hamt-insert (aref array index) key value hash (1+ depth) test))
            ((= depth 6)
             (make-instance 'value-node :key key :value value))
            (t
             (%hamt-insert (make-instance 'table-node)
                           key
                           value
                           hash
                           (1+ depth)
                           test)))))
    (make-instance 'table-node
                   :bitmap (logior bitmap (ash 1 bits))
                   :table (funcall (if hit #'vec-update #'vec-insert)
                                   array
                                   index
                                   new-node))))


;; Methods for removing entries from a HAMT
(defgeneric %hamt-remove (node key hash depth test))

(defmethod %hamt-remove ((node value-node) key hash depth test)
  (let ((nkey (node-key node)))
    (if (funcall test key nkey)
        nil
        node)))

(defun alist-remove (key alist test)
  (remove key alist :test (lambda (k p) (funcall test (car p) k))))

;; Removing an entry from a conflict node reduces the scope of the hash
;; collision. If there is now only 1 key with the given hash, we can
;; return a value-node, since there is no longer a collision.
(defmethod %hamt-remove ((node conflict-node) key hash depth test)
  (let ((alist (alist-remove key (conflict-alist node) test)))
    (let ((len (length alist)))
      (cond
        ;; Can we get rid of this conditional branch? If the length of the
        ;; alist is 0, that means it used to be 1, but we should never have
        ;; had a conflict node with only 1 entry in the first place.
        ((= len 0) nil)
        ((= len 1) (make-instance 'value-node
                                  :key (caar alist)
                                  :value (cdar alist)))
        (t (make-instance 'conflict-node
                          :hash hash
                          :alist alist))))))

;; Removing a key from a table node can mean updating its bitmap if there
;; is nothing left in the corresponding branch.
(defmethod %hamt-remove ((node table-node) key hash depth test)
  (let* ((bitmap (table-bitmap node))
         (array (table-array node))
         (bits (get-bits hash depth))
         (index (get-index bits bitmap))
         (hit (logbitp bits bitmap)))
    (if (not hit)
        node
        (let ((new-node (%hamt-remove (aref array index) key hash (1+ depth) test)))
          (cond
            (new-node
             (make-instance 'table-node
                            :bitmap bitmap
                            :table (vec-update array index new-node)))
            ((= bitmap 1) nil)
            (t (make-instance 'table-node
                              :bitmap (logxor bitmap (ash 1 bits))
                              :table (vec-remove array index))))))))


;; Methods for reducing over elements of HAMTs
(defmethod dict-reduce (func (node value-node) initial-value)
  (funcall func initial-value (node-key node) (node-value node)))

(defmethod dict-reduce (func (node conflict-node) initial-value)
  (labels ((f (alist r)
             (if alist
                 (f (cdr alist)
                    (funcall func r (caar alist) (cdar alist)))
                 r)))
    (f (conflict-alist node) initial-value)))

(defmethod dict-reduce (func (node table-node) initial-value)
  (reduce (lambda (r child)
            (dict-reduce func child r))
          (table-array node)
          :initial-value initial-value))


;; Wrapper HAMT class
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

(defmethod dict-size ((dict hamt))
  (dict-size (hamt-table dict)))

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

(defmethod dict-remove ((dict hamt) key)
  (make-instance 'hamt
                 :test (hamt-test dict)
                 :hash (hamt-hash dict)
                 :table (%hamt-remove (hamt-table dict)
                                      key
                                      (funcall (hamt-hash dict) key)
                                      0
                                      (hamt-test dict))))

(defmethod dict-reduce (func (dict hamt) initial-value)
  (dict-reduce func (hamt-table dict) initial-value))

(defmethod dict-filter (predicate (dict hamt))
  (dict-reduce (lambda (filtered-dict k v)
                 (if (funcall predicate k v)
                     (dict-insert filtered-dict k v)
                     filtered-dict))
               dict
               (make-hamt (hamt-test dict) (hamt-hash dict))))
