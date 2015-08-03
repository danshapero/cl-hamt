(in-package :cl-hamt)

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
   (entries
    :reader conflict-entries
    :initarg :entries
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
  (let ((key-val (assoc key (conflict-entries node) :test test)))
    (if key-val
        (values (cdr key-val) t)
        (values nil nil))))


;; Methods for getting the size of HAMT nodes
(defgeneric %hamt-size (node))

(defmethod %hamt-size ((node value-node))
  1)

(defmethod %hamt-size ((node table-node))
  (loop for node across (table-array node)
     sum (%hamt-size node)))

(defmethod %hamt-size ((node conflict-node))
  (length (conflict-entries node)))


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
                       :entries (->> '()
                                     (acons key value)
                                     (acons nkey (node-value node)))))))

;; Inserting into a conflict node either updates the value associated to an
;; existing key, or expands the scope of the conflict
(defmethod %hamt-insert ((node conflict-node) key value hash depth test)
  (let ((entries (conflict-entries node)))
    (make-instance 'conflict-node
                   :hash hash
                   :entries (if (assoc key entries)
                                (mapcar (lambda (kv)
                                          (if (funcall test (car kv) key)
                                              (cons key value)
                                              (cons (car kv) (cdr kv))))
                                        entries)
                                (acons key value entries)))))

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
  (let ((entries (alist-remove key (conflict-entries node) test)))
    (let ((len (length entries)))
      (cond
        ;; Can we get rid of this conditional branch? If the length of the
        ;; alist is 0, that means it used to be 1, but we should never have
        ;; had a conflict node with only 1 entry in the first place.
        ((= len 0) nil)
        ((= len 1) (make-instance 'value-node
                                  :key (caar entries)
                                  :value (cdar entries)))
        (t (make-instance 'conflict-node
                          :hash hash
                          :entries entries))))))

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
(defgeneric %hamt-reduce (func node initial-value))

(defmethod %hamt-reduce (func (node value-node) initial-value)
  (funcall func initial-value (node-key node) (node-value node)))

(defmethod %hamt-reduce (func (node conflict-node) initial-value)
  (labels ((f (alist r)
             (if alist
                 (f (cdr alist)
                    (funcall func r (caar alist) (cdar alist)))
                 r)))
    (f (conflict-entries node) initial-value)))

(defmethod %hamt-reduce (func (node table-node) initial-value)
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
      :initform (make-instance 'table-node
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
                 :table (%hamt-insert (hamt-table dict)
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
