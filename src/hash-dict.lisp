(in-package :cl-hamt)

;; Leaves in a dictionary also store the value contained at the node, as well
;; as a key
(defclass dict-leaf (leaf)
  ((value
    :reader node-value
    :initarg :value
    :initform nil)))

;; These classes give extra information to dispatch on, e.g. for looking up
;; entries in conflict nodes
(defclass dict-conflict (conflict) ())
(defclass dict-table (table) ())



;; Methods for looking up key/value pairs in a dict
(defmethod %hamt-lookup ((node dict-leaf) key hash depth test)
  (if (funcall test (node-key node) key)
      (values (node-value node) t)
      (values nil nil)))

(defmethod %hamt-lookup ((node dict-table) key hash depth test)
  (let ((bitmap (table-bitmap node))
        (bits (get-bits hash depth)))
    (if (logbitp bits bitmap)
        (let ((index (get-index bits bitmap)))
          (%hamt-lookup (aref (table-array node) index) key hash (1+ depth) test))
        (values nil nil))))

(defmethod %hamt-lookup ((node dict-conflict) key hash depth test)
  (declare (ignore hash depth))
  (let ((key-val (assoc key (conflict-entries node) :test test)))
    (if key-val
        (values (cdr key-val) t)
        (values nil nil))))



;; Methods for inserting key/value pairs into a dict
(defgeneric %dict-insert (node key value hash depth test))

;; Inserting into a leaf either functionally updates the value stored in the
;; current node if the keys match, or creates a conflict node if the keys do
;; not match but their hashes do.
(defmethod %dict-insert ((node dict-leaf) key value hash depth test)
  (declare (ignore depth))
  (let ((nkey (node-key node)))
    (if (funcall test key nkey)
        (make-instance 'dict-leaf
                       :key key
                       :value value)
        (make-instance 'dict-conflict
                       :hash hash
                       :entries (->> '()
                                     (acons key value)
                                     (acons nkey (node-value node)))))))

;; Inserting into a conflict node either updates the value associated to an
;; existing key, or expands the scope of the conflict
(defmethod %dict-insert ((node dict-conflict) key value hash depth test)
  (let ((entries (conflict-entries node)))
    (make-instance 'dict-conflict
                   :hash hash
                   :entries (if (assoc key entries)
                                (mapcar (lambda (kv)
                                          (if (funcall test (car kv) key)
                                              (cons key value)
                                              (cons (car kv) (cdr kv))))
                                        entries)
                                (acons key value entries)))))

(defmethod %dict-insert ((node dict-table) key value hash depth test)
  (let* ((bitmap (table-bitmap node))
         (array (table-array node))
         (bits (get-bits hash depth))
         (index (get-index bits bitmap))
         (hit (logbitp bits bitmap))
         (new-node
          (cond
            (hit
             (%dict-insert (aref array index) key value hash (1+ depth) test))
            ((= depth 6)
             (make-instance 'dict-leaf :key key :value value))
            (t
             (%dict-insert (make-instance 'dict-table)
                           key
                           value
                           hash
                           (1+ depth)
                           test)))))
    (make-instance 'dict-table
                   :bitmap (logior bitmap (ash 1 bits))
                   :table (funcall (if hit #'vec-update #'vec-insert)
                                   array
                                   index
                                   new-node))))



;; Removing entries from dictionaries.
;; Most of the functionality is contained in the file hamt.lisp.

(defun alist-remove (key alist test)
  (remove key alist :test (lambda (k p) (funcall test (car p) k))))

;; Removing an entry from a conflict node reduces the scope of the hash
;; collision. If there is now only 1 key with the given hash, we can
;; return a dict-leaf, since there is no longer a collision.
(defmethod %hamt-remove ((node dict-conflict) key hash depth test)
  (let ((entries (alist-remove key (conflict-entries node) test)))
    (let ((len (length entries)))
      (cond
        ;; Can we get rid of this conditional branch? If the length of the
        ;; alist is 0, that means it used to be 1, but we should never have
        ;; had a conflict node with only 1 entry in the first place.
        ((= len 0) nil)
        ((= len 1) (make-instance 'dict-leaf
                                  :key (caar entries)
                                  :value (cdar entries)))
        (t (make-instance 'dict-conflict
                          :hash hash
                          :entries entries))))))



;; Methods for reducing over elements of HAMTs
(defgeneric %hamt-reduce (func node initial-value))

(defmethod %hamt-reduce (func (node dict-leaf) initial-value)
  (funcall func initial-value (node-key node) (node-value node)))

(defmethod %hamt-reduce (func (node dict-conflict) initial-value)
  (labels ((f (alist r)
             (if alist
                 (f (cdr alist)
                    (funcall func r (caar alist) (cdar alist)))
                 r)))
    (f (conflict-entries node) initial-value)))

(defmethod %hamt-reduce (func (node dict-table) initial-value)
  (reduce (lambda (r child)
            (%hamt-reduce func child r))
          (table-array node)
          :initial-value initial-value))


;; Wrapper HAMT class
(defclass hash-dict ()
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
      :initform (make-instance 'dict-table
                               :bitmap 0
                               :table (make-array 0)))))

(defun make-hash-dict (&key (test #'equal) (hash #'cl-murmurhash:murmurhash))
  (make-instance 'hash-dict :test test :hash hash))

(defun dict-lookup (dict key)
  (%hamt-lookup (hamt-table dict)
                key
                (funcall (hamt-hash dict) key)
                0
                (hamt-test dict)))

(defun dict-size (dict)
  (%hamt-size (hamt-table dict)))

(defun dict-insert (dict key value)
  (make-instance 'hash-dict
                 :test (hamt-test dict)
                 :hash (hamt-hash dict)
                 :table (%dict-insert (hamt-table dict)
                                      key
                                      value
                                      (funcall (hamt-hash dict) key)
                                      0
                                      (hamt-test dict))))

(defun dict-remove (dict key)
  (make-instance 'hash-dict
                 :test (hamt-test dict)
                 :hash (hamt-hash dict)
                 :table (%hamt-remove (hamt-table dict)
                                      key
                                      (funcall (hamt-hash dict) key)
                                      0
                                      (hamt-test dict))))

(defun dict-reduce (func dict initial-value)
  (%hamt-reduce func (hamt-table dict) initial-value))

(defun dict-filter (predicate dict)
  (dict-reduce (lambda (filtered-dict k v)
                 (if (funcall predicate k v)
                     (dict-insert filtered-dict k v)
                     filtered-dict))
               dict
               (make-hash-dict :test (hamt-test dict)
                               :hash (hamt-hash dict))))

(defun dict-reduce-keys (func dict initial-value)
  (flet ((f (r k v)
           (declare (ignore v))
           (funcall func r k)))
    (dict-reduce #'f dict initial-value)))

(defun dict-reduce-values (func dict initial-value)
  (flet ((f (r k v)
           (declare (ignore k))
           (funcall func r v)))
    (dict-reduce #'f dict initial-value)))

(defun dict->alist (dict)
  (dict-reduce (lambda (alist k v)
                 (acons k v alist))
               dict
               '()))
