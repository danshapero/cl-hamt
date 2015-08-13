(in-package :cl-hamt)

(defclass set-leaf (leaf) ())

(defclass set-conflict (conflict) ())
(defclass set-table (table) ())



;; Methods for looking up whether items are contained in the set
(defmethod %hamt-lookup ((node set-leaf) key hash depth test)
  (declare (ignore hash depth))
  (funcall test (node-key node) key))

(defmethod %hamt-lookup ((node set-table) key hash depth test)
  (let ((bitmap (table-bitmap node))
        (bits (get-bits hash depth)))
    (if (logbitp bits bitmap)
        (let ((index (get-index bits bitmap)))
          (%hamt-lookup (aref (table-array node) index) key hash (1+ depth) test))
        nil)))

(defmethod %hamt-lookup ((node set-conflict) key hash depth test)
  (declare (ignore hash depth))
  (member key (conflict-entries node) :test test))



;; Methods for inserting items into the set
(defgeneric %set-insert (node key hash depth test))

;; Adding a new element to a leaf node either returns the leaf node if that
;; item was already present in the set, or creates a conflict node if there
;; was a hash collision.
(defmethod %set-insert ((node set-leaf) key hash depth test)
  (declare (ignore depth))
  (let ((nkey (node-key node)))
    (if (funcall test key nkey)
        node
        (make-instance 'set-conflict
                       :hash hash
                       :entries (list key nkey)))))

(defmethod %set-insert ((node set-conflict) key hash depth test)
  (let ((entries (conflict-entries node)))
    (make-instance 'set-conflict
                   :hash hash
                   :entries (if (member key entries :key test)
                                entries
                                (cons key entries)))))

(defmethod %set-insert ((node set-table) key hash depth test)
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
             (make-instance 'set-leaf :key key))
            (t
             (%set-insert (make-instance 'set-table)
                                         key
                                         hash
                                         (1+ depth)
                                         test)))))
    (make-instance 'set-table
                   :bitmap (logior bitmap (ash 1 bits))
                   :table (funcall (if hit #'vec-update #'vec-insert)
                                   array
                                   index
                                   new-node))))



;; Methods for removing items from a hash-set
(defmethod %hamt-remove ((node set-conflict) key hash depth test)
  (let* ((entries (remove key (conflict-entries node) :test test))
         (len (length entries)))
    (cond
      ((= len 0) nil)
      ((= len 1) (make-instance 'set-leaf
                                :key (car entries)))
      (t (make-instance 'set-conflict
                        :hash hash
                        :entries entries)))))



;; Methods for reducing over elements of hash-sets
(defmethod %hamt-reduce (func (node set-leaf) initial-value)
  (funcall func initial-value (node-key node)))

(defmethod %hamt-reduce (func (node set-conflict) initial-value)
  (reduce func (conflict-entries node) :initial-value initial-value))



;; Wrapper set class
(defclass hash-set (hamt)
  ((table
    :reader hamt-table
    :initarg :table
    :initform (make-instance 'set-table
                             :bitmap 0
                             :table (make-array 0)))))

(defun make-hash-set (&key (test #'equal) (hash #'cl-murmurhash:murmurhash))
  (make-instance 'hash-set :test test :hash hash))

(defun set-lookup (set x)
  (with-hamt set (:test test :hash hash :table table)
    (%hamt-lookup table x (funcall hash x) 0 test)))

(defun set-size (set)
  (%hamt-size (hamt-table set)))

(defun set-insert (set x)
  (with-hamt set (:test test :hash hash :table table)
    (make-instance 'hash-set
                   :test test
                   :hash hash
                   :table (%set-insert table x (funcall hash x) 0 test))))

(defun set-remove (set x)
  (with-hamt set (:test test :hash hash :table table)
    (make-instance 'hash-set
                   :test test
                   :hash hash
                   :table (%hamt-remove table x (funcall hash x) 0 test))))

(defun set-reduce (func set initial-value)
  (%hamt-reduce func (hamt-table set) initial-value))

(defun set-map (func set
                &key
                  (test nil test-supplied-p)
                  (hash nil hash-supplied-p))
  (set-reduce (lambda (mapped-set x)
                (set-insert mapped-set
                            (funcall func x)))
              set
              (make-hash-set :test (if test-supplied-p test (hamt-test set))
                             :hash (if hash-supplied-p hash (hamt-hash set)))))

(defun set-filter (predicate set)
  (set-reduce (lambda (filtered-set x)
                (if (funcall predicate x)
                    (set-insert filtered-set x)
                    filtered-set))
              set
              (make-hash-set :test (hamt-test set)
                             :hash (hamt-hash set))))

(defun set->list (set)
  (set-reduce (lambda (lst x) (cons x lst))
              set
              '()))

