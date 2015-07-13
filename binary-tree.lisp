
(in-package :cl-dict)

(defgeneric %lookup (node key %<))
(defgeneric %insert (node key value %<))
(defgeneric %remove (node key %<))


(defclass binary-tree-node ()
  ((key
   :initarg :key
   :reader bt-key)
   (value
    :initarg :value
    :reader bt-value)
   (left
    :initarg :left
    :reader bt-left)
   (right
    :initarg :right
    :reader bt-right)))


;; Methods for nil nodes

(defmethod %lookup ((node (eql nil)) key %<)
  (declare (ignore node key %<))
  (values nil nil))

(defmethod %insert ((node (eql nil)) key value %<)
  (declare (ignore %<))
  (make-instance 'binary-tree-node
                 :key key
                 :value value
                 :left nil
                 :right nil))

(defmethod %remove ((node (eql nil)) key %<)
  (declare (ignore node key %<))
  node)


;; Methods for non-nil nodes

(defmethod %lookup ((node binary-tree-node) key %<)
  (cond
   ((funcall %< key (bt-key node)) (%lookup (bt-left node) key %<))
   ((funcall %< (bt-key node) key) (%lookup (bt-right node) key %<))
   (t (values (bt-value node) t))))

(defmethod %insert ((node binary-tree-node) key value %<)
  (cond
   ((funcall %< key (bt-key node))
    (make-instance 'binary-tree-node
                   :key (bt-key node)
                   :value (bt-value node)
                   :left (%insert (bt-left node) key value %<)
                   :right (bt-right node)))
   ((funcall %< (bt-key node) key)
    (make-instance 'binary-tree-node
                   :key (bt-key node)
                   :value (bt-value node)
                   :left (bt-left node)
                   :right (%insert (bt-right node) key value %<)))
   (t
    (make-instance 'binary-tree-node
                   :key key
                   :value value
                   :left (bt-left node)
                   :right (bt-right node)))))


(defun bt-next (node)
  (labels ((f (node)
             (let ((left (bt-left node)))
               (if (null left)
                   node
                   (f left)))))
    (f (bt-right node))))

(defun remove-root (node %<)
  (let ((left (bt-left node))
        (right (bt-right node)))
    (cond
      ((null left) right)
      ((null right) left)
      (t (remove-root-with-children node %<)))))

(defun remove-root-with-children (node %<)
  (let* ((next (bt-next node))
         (key (bt-key next)))
    (make-instance 'binary-tree-node
                   :key key
                   :value (bt-value next)
                   :left (bt-left node)
                   :right (%remove (bt-right node) key %<))))

(defmethod %remove ((node binary-tree-node) key %<)
  (cond
    ((funcall %< (bt-key node) key)
     (make-instance 'binary-tree-node
                    :key (bt-key node)
                    :value (bt-value node)
                    :left (bt-left node)
                    :right (%remove (bt-right node) key %<)))
    ((funcall %< key (bt-key node))
     (make-instance 'binary-tree-node
                    :key (bt-key node)
                    :value (bt-value node)
                    :left (%remove (bt-left node) key %<)
                    :right (bt-right node)))
    (t (remove-root node %<))))


(defclass binary-tree ()
  ((root
    :initarg :root
    :initform nil
    :reader tree-root)
   (compare
    :initarg :compare
    :initform #'<
    :reader tree-%<)))

(defmethod dict-lookup ((tree binary-tree) key)
  (%lookup (tree-root tree) key (tree-%< tree)))

(defmethod dict-insert ((tree binary-tree) key value)
  (make-instance 'binary-tree
                 :root (%insert (tree-root tree) key value (tree-%< tree))
                 :compare (tree-%< tree)))

(defmethod dict-remove ((tree binary-tree) key)
  (make-instance 'binary-tree
                 :root (%remove (tree-root tree) key (tree-%< tree))
                 :compare (tree-%< tree)))

(defun make-binary-tree (&optional (compare #'<))
  (make-instance 'binary-tree
                 :root nil
                 :compare compare))
