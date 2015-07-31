;;;; package.lisp

(defpackage #:cl-hamt
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

   ;; Hash array-mapped tries
   #:hash-dict
   #:make-hash-dict))

