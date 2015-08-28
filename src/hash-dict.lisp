(in-package #:cl-hamt)

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
  (with-table node hash depth
      (bitmap array bits index hit)
    (if hit
        (%hamt-lookup (aref array index) key hash (1+ depth) test)
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
                       :entries (acons key
                                       value
                                       (acons nkey
                                              (node-value node)
                                              '()))))))

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
  (with-table node hash depth
      (bitmap array bits index hit)
    (flet ((%insert (table)
             (%dict-insert table key value hash (1+ depth) test)))
      (let ((new-node
              (cond
                (hit (%insert (aref array index)))
                ((= depth 6) (make-instance 'dict-leaf
                                            :key key
                                            :value value))
                (t (%insert (make-instance 'dict-table))))))
        (make-instance 'dict-table
                       :bitmap (logior bitmap (ash 1 bits))
                       :table (funcall (if hit #'vec-update #'vec-insert)
                                       array
                                       index
                                       new-node))))))



;; Removing entries from dictionaries.
;; Most of the functionality is contained in the file hamt.lisp.

(defun alist-remove (key alist test)
  (remove key alist :test (lambda (k p) (funcall test (car p) k))))

;; Removing an entry from a conflict node reduces the scope of the hash
;; collision. If there is now only 1 key with the given hash, we can
;; return a dict-leaf, since there is no longer a collision.
(defmethod %hamt-remove ((node dict-conflict) key hash depth test)
  (let ((entries (alist-remove key (conflict-entries node) test)))
    (if (= (length entries) 1)
        (make-instance 'dict-leaf
                       :key (caar entries)
                       :value (cdar entries))
        (make-instance 'dict-conflict
                       :hash hash
                       :entries entries))))



;; Methods for reducing over elements of HAMTs
(defmethod %hamt-reduce (func (node dict-leaf) initial-value)
  (funcall func initial-value (node-key node) (node-value node)))

(defmethod %hamt-reduce (func (node dict-conflict) initial-value)
  (labels ((f (alist r)
             (if alist
                 (f (cdr alist)
                    (funcall func r (caar alist) (cdar alist)))
                 r)))
    (f (conflict-entries node) initial-value)))



;; Wrapper dictionary class
(defclass hash-dict (hamt)
  ((table
    :reader hamt-table
    :initarg :table
    :initform (make-instance 'dict-table
                             :bitmap 0
                             :table (make-array 0)))))

(defun empty-dict (&key (test #'equal) (hash #'cl-murmurhash:murmurhash))
  "Return an empty hash-dict, in which keys will be compared and hashed
with the supplied test and hash functions. The hash must be a 32-bit hash."
  (make-instance 'hash-dict :test test :hash hash))

(defun dict-lookup (dict key)
  "Multiply-return the value mapped to by the key in the dictionary and
whether or not the value is present in the dictionary.
The multiple return is necessary in case a key is present but maps to nil."
  (with-hamt dict (:test test :hash hash :table table)
    (%hamt-lookup table key (funcall hash key) 0 test)))

(defun dict-size (dict)
  "Return the number of key/value pairs in the dict"
  (%hamt-size (hamt-table dict)))

(defun dict-insert (dict &rest args)
  "Return a new dictionary with the key/value pairs added. The key/value
pairs are assumed to be alternating in the &rest argument, so to add the
key/value pairs (k1, v1), ..., (kn, vn), one would invoke
  (dict-insert dict k1 v1 ... kn vn).
If any of the keys are already present in the dict passed, they are mapped
to the new values in the returned dict."
  (with-hamt dict (:test test :hash hash :table table)
    (flet ((%insert (table key value)
             (%dict-insert table key value (funcall hash key) 0 test)))
      (make-instance
       'hash-dict
       :test test
       :hash hash
       :table (labels ((f (table args)
                         (if args
                             (let ((key (car args))
                                   (value (cadr args)))
                               (f (%insert table key value)
                                  (cddr args)))
                             table)))
                (f table args))))))

(defun dict-remove (dict &rest keys)
  "Return a new dict with the keys removed. Any keys passed that are not
already present in the dict are ignored."
  (with-hamt dict (:test test :hash hash :table table)
    (flet ((%remove (table key)
             (%hamt-remove table key (funcall hash key) 0 test)))
      (make-instance 'hash-dict
                     :test test
                     :hash hash
                     :table (reduce #'%remove keys :initial-value table)))))

(defun dict-reduce (func dict initial-value)
  "Successively apply a function to key/value pairs of the dict.
The function is assumed to have the signature
   `func :: A K V -> A`,
where `A` is the type of the initial-value, `K` is the type of the dict
keys and `V` is the type of dictionary values.
Note that HAMTs do not store items in any order, so the reduction operation
cannot be sensitive to the order in which the items are reduced."
  (%hamt-reduce func (hamt-table dict) initial-value))

(defun dict-clone (dict test hash)
  (empty-dict :test (if test test (hamt-test dict))
              :hash (if hash hash (hamt-hash dict))))

(defun dict-map-values (func dict &key test hash)
  "Return a new dict with the values mapped by the given function.
Optionally use new comparison and hash functions for the mapped dict."
  (dict-reduce (lambda (d k v)
                 (dict-insert d k (funcall func v)))
               dict
               (dict-clone dict test hash)))

(defun dict-map-keys (func dict &key test hash)
  "Return a new dict with the keys mapped by the given function."
  (dict-reduce (lambda (d k v)
                 (dict-insert d (funcall func k) v))
               dict
               (dict-clone dict test hash)))

(defun dict-filter (predicate dict)
  "Return a new dict consisting of the key/value pairs satisfying the
given predicate."
  (dict-reduce (lambda (filtered-dict k v)
                 (if (funcall predicate k v)
                     (dict-insert filtered-dict k v)
                     filtered-dict))
               dict
               (empty-dict :test (hamt-test dict)
                           :hash (hamt-hash dict))))

(defun dict-reduce-keys (func dict initial-value)
  "Reducing over dictionary keys, ignoring the values."
  (flet ((f (r k v)
           (declare (ignore v))
           (funcall func r k)))
    (dict-reduce #'f dict initial-value)))

(defun dict-reduce-values (func dict initial-value)
  "Reducing over dictionary values, ignoring the keys."
  (flet ((f (r k v)
           (declare (ignore k))
           (funcall func r v)))
    (dict-reduce #'f dict initial-value)))

(defun dict->alist (dict)
  (dict-reduce (lambda (alist k v)
                 (acons k v alist))
               dict
               '()))
