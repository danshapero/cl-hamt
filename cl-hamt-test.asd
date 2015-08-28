
(asdf:defsystem #:cl-hamt-test
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

