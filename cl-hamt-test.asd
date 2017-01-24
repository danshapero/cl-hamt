
(asdf:defsystem #:cl-hamt-test
  :description "Unit tests and benchmarks for cl-hamt"
  :author "Daniel Shapero <shapero.daniel@gmail.com>"
  :license "BSD 3-clause"
  :depends-on (#:cl-hamt
               #:fiveam)
  :serial t
  :components
  ((:module "test"
    :serial t
    :components
    ((:file "package")
     (:file "hash-dict-test")
     (:file "hash-set-test")
     (:file "benchmarks")))))

