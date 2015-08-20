
(defpackage #:cl-hamt-test
  (:use #:cl #:cl-hamt #:fiveam)
  (:export #:run!
           #:hash-set-tests
           #:hash-dict-tests
           #:asymptotic-runtime))
