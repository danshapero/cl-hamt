;;;; package.lisp

(defpackage #:cl-dict
  (:use #:cl)
  (:export
   ;; Generics for operating on dictionaries
   #:dict-lookup
   #:dict-insert
   #:dict-remove
   #:dict-size
   #:dict-reduce
   #:dict-map
   #:dict-filter
   #:dict-reduce-keys
   #:dict-reduce-values

   ;; Binary trees
   #:binary-tree
   #:make-binary-tree))

