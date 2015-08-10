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
    :initform (make-instance 'set-table
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

(defun set-remove (s key)
  (make-instance 'hash-set
                 :test (hamt-test s)
                 :hash (hamt-hash s)
                 :table (%hamt-remove (hamt-table s)
                                      key
                                      (funcall (hamt-hash s) key)
                                      0
                                      (hamt-test s))))

(defun set-reduce (func s initial-value)
  (%hamt-reduce func (hamt-table s) initial-value))

(defun set-map (func s)
  (set-reduce (lambda (mapped-s k)
                (set-insert mapped-s
                            (funcall func k)))
              s
              (make-hash-set :test (hamt-test s)
                             :hash (hamt-hash s))))

(defun set-filter (predicate s)
  (set-reduce (lambda (filtered-s k)
                (if (funcall predicate k)
                    (set-insert filtered-s k)
                    filtered-s))
              s
              (make-hash-set :test (hamt-test s)
                             :hash (hamt-hash s))))

(defun set->list (s)
  (set-reduce (lambda (lst k) (cons k lst))
              s
              '()))

