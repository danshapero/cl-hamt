
(defpackage #:cl-hamt
  (:nicknames #:hamt)
  (:use #:cl)
  (:export
   ;; Functions for operating on dictionaries
   #:empty-dict
   #:dict-lookup
   #:dict-insert
   #:dict-remove
   #:dict-size
   #:dict-reduce
   #:dict-map-values
   #:dict-map-keys
   #:dict-filter
   #:dict-eq

   ;; Functions for operating on sets
   #:empty-set
   #:set-lookup
   #:set-insert
   #:set-remove
   #:set-size
   #:set-reduce
   #:set-map
   #:set-filter
   #:set-eq

   ;; Utilities, conversion routines
   #:dict-reduce-keys
   #:dict-reduce-values
   #:dict->alist
   #:set->list

   ;; Set theoretic operations
   #:set-union
   #:set-intersection
   #:set-diff
   #:set-symmetric-diff

   ;; Hash array-mapped tries
   #:hash-dict
   #:hash-set

   ;; Conditions
   #:incompatible-tests
   ))
