;;;; package.lisp

(defpackage #:cl-dict
  (:use #:cl #:yoda)
  (:export
   ;; Generics for operating on dictionaries
   #:dict-lookup
   #:dict-insert
   #:dict-remove
   #:dict-size
   #:dict-reduce
   #:dict-map
   #:dict-filter

   ;; Utilities, conversion routines
   #:dict-reduce-keys
   #:dict-reduce-values
   #:dict->alist

   ;; Binary trees
   #:binary-tree
   #:make-binary-tree))

