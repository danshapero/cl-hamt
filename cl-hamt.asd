
(asdf:defsystem #:cl-hamt
  :description "Dictionary & set data structure using hash array-mapped tries"
  :author "Daniel Shapero <shapero.daniel@gmail.com>"
  :license "BSD 3-clause"
  :depends-on (#:cl-murmurhash)
  :serial t
  :components
  ((:module "src"
    :serial t
    :components
    ((:file "package")
     (:file "util")
     (:file "hamt")
     (:file "hash-dict")
     (:file "hash-set")))))
