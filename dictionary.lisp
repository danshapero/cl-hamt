
(in-package :cl-hamt)

(defgeneric dict-lookup (dict key))
(defgeneric dict-insert (dict key value))
(defgeneric dict-remove (dict key))
(defgeneric dict-size (dict))
(defgeneric dict-reduce (func dict initial-value))
(defgeneric dict-map (func dict))
(defgeneric dict-filter (predicate dict))

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
