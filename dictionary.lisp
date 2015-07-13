
(in-package :cl-dict)

(defgeneric dict-lookup (dict key))
(defgeneric dict-insert (dict key value))
(defgeneric dict-remove (dict key))
(defgeneric dict-size (dict))
(defgeneric dict-reduce (function dict initial-value))
(defgeneric dict-map (function dict))
(defgeneric dict-filter (predicate dict))

(defun dict-reduce-keys (function dict initial-value)
  (flet ((f (k v)
           (declare (ignore v))
           (function k)))
    (dict-reduce f dict initial-value)))

(defun dict-reduce-values (function dict initial-value)
  (flet ((f (k v)
           (declare (ignore k))
           (function v)))
    (dict-reduce f dict initial-value)))
