;;;; package.lisp

(defpackage #:cl-hamt
  (:nicknames :hamt)
  (:use #:cl #:yoda)
  (:export
   ;; Functions for operating on dictionaries
   #:dict-lookup
   #:dict-insert
   #:dict-remove
   #:dict-size
   #:dict-reduce
   #:dict-map
   #:dict-filter

   ;; Functions for operating on sets
   #:set-lookup
   #:set-insert
   #:set-remove
   #:set-size
   #:set-reduce
   #:set-map
   #:set-filter

   ;; Utilities, conversion routines
   #:dict-reduce-keys
   #:dict-reduce-values
   #:dict->alist

   ;; Hash array-mapped tries
   #:hash-dict
   #:make-hash-dict
   #:hash-set
   #:make-hash-set))

