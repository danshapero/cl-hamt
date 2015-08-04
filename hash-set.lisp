(in-package :cl-hamt)

(defclass set-leaf-node ()
  ((key
    :reader node-key
    :initarg :key
    :initform nil)))


;; Methods for looking up whether items are contained in the set
(defmethod %hamt-lookup ((node set-leaf-node) key hash depth test)
  (declare (ignore hash depth))
  (funcall test (node-key node) key))


;; Methods for inserting items into the set
(defgeneric %set-insert (node key hash depth test))

;; Adding a new element to a leaf node either returns the leaf node if that
;; item was already present in the set, or creates a conflict node if there
;; was a hash collision.
(defmethod %set-insert ((node set-leaf-node) key hash depth test)
  (declare (ignore depth))
  (let ((nkey (node-key node)))
    (if (funcall test key nkey)
        node
        (make-instance 'conflict-node
                       :hash hash
                       :entries (list key nkey)))))

(defmethod %set-insert ((node conflict-node) key hash depth test)
  (let ((entries (conflict-entries node)))
    (make-instance 'conflict-node
                   :hash hash
                   :entries (if (member key entries :key test)
                                entries
                                (cons key entries)))))

(defmethod %set-insert ((node table-node) key hash depth test)
  (let* ((bitmap (table-bitmap node))
         (array (table-array node))
         (bits (get-bits hash depth))
         (index (get-index bits bitmap))
         (hit (logbitp bits bitmap))
         (new-node
          (cond
            (hit
             (%set-insert (aref array index) key hash (1+ depth) test))
            ((= depth 6)
             (make-instance 'set-leaf-node :key key))
            (t
             (%set-insert (make-instance 'table-node)
                                         key
                                         hash
                                         (1+ depth)
                                         test)))))
    (make-instance 'table-node
                   :bitmap (logior bitmap (ash 1 bits))
                   :table (funcall (if hit #'vec-update #'vec-insert)
                                   array
                                   index
                                   new-node))))


;; Methods for removing items from a hash-set
(defgeneric %set-remove (node key hash depth test))

;; What do we do about removal from conflict nodes that are contained
;; in a hash-set?


;;

;; Methods for getting the size of the set
(defmethod %hamt-size ((node set-leaf-node))
  1)


;; Wrapper HAMT class
(defclass hash-set ()
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

(defun make-hash-set (&key (test #'equal) (hash #'cl-murmurhash:murmurhash))
  (make-instance 'hash-set :test test :hash hash))

(defun set-lookup (s key)
  (%hamt-lookup (hamt-table s)
                key
                (funcall (hamt-hash s) key)
                0
                (hamt-test s)))

(defun set-size (s)
  (%hamt-size (hamt-table s)))

(defun set-insert (s key)
  (make-instance 'hash-set
                 :test (hamt-test s)
                 :hash (hamt-hash s)
                 :table (%set-insert (hamt-table s)
                                     key
                                     (funcall (hamt-hash s) key)
                                     0
                                     (hamt-test s))))

