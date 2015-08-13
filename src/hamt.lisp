
(in-package :cl-hamt)

(defclass leaf ()
  ((key
    :reader node-key
    :initarg :key
    :initform nil)))

(defclass conflict ()
  ((hash
    :reader conflict-hash
    :initarg :hash)
   (entries
    :reader conflict-entries
    :initarg :entries
    :initform '())))

(defclass table ()
  ((bitmap
    :reader table-bitmap
    :initarg :bitmap
    :initform 0
    :type (unsigned-byte 5))
   (table
    :reader table-array
    :initarg :table
    :initform (make-array 0 :initial-element nil))))

;; Base HAMT class
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
    :initarg :table)))

;; Convenience macro for accessing HAMT slots
(defmacro with-hamt (hamt (&key test hash table) &body body)
  `(with-accessors ((,test hamt-test)
                    (,hash hamt-hash)
                    (,table hamt-table))
       ,hamt
     ,@body))

;; Getting the size of a HAMT
(defgeneric %hamt-size (node))

(defmethod %hamt-size ((node leaf))
  1)

(defmethod %hamt-size ((node table))
  (loop for node across (table-array node)
     sum (%hamt-size node)))

(defmethod %hamt-size ((node conflict))
  (length (conflict-entries node)))


;; Depending on whether the HAMT is a set or a dict, looking up an entry
;; returns either a boolean or multiple values respectively, so we defer
;; implementation to the respective classes.
(defgeneric %hamt-lookup (node key hash depth test))


;; Removing entries from HAMTs
(defgeneric %hamt-remove (node key hash depth test))

(defmethod %hamt-remove ((node leaf) key hash depth test)
  (let ((nkey (node-key node)))
    (if (funcall test key nkey)
        nil
        node)))

;; Removing entries from a conflict node differs for sets and dicts

;; Removing a key from a table node can mean updating its bitmap if there
;; is nothing left in the corresponding branch.
(defmethod %hamt-remove ((node table) key hash depth test)
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
             (make-instance (type-of node)
                            :bitmap bitmap
                            :table (vec-update array index new-node)))
            ((= bitmap 1) nil)
            (t (make-instance (type-of node)
                              :bitmap (logxor bitmap (ash 1 bits))
                              :table (vec-remove array index))))))))


;; Reducing over a HAMT is the same for table nodes of sets and dicts
(defmethod %hamt-reduce (func node initial-value))

(defmethod %hamt-reduce (func (node table) initial-value)
  (reduce (lambda (r child)
            (%hamt-reduce func child r))
          (table-array node)
          :initial-value initial-value))
