
(asdf:defsystem #:cl-dict-test
  :depends-on (#:cl-dict
               #:fiveam
               #:yoda)
  :serial t
  :components
  ((:module "test"
            :serial t
            :components
            ((:file "package")
             (:file "cl-dict-tests")))))

