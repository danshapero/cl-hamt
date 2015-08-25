
(asdf:defsystem #:cl-hamt-examples
  :depends-on (#:cl-hamt
               #:split-sequence)
  :serial t
  :components
  ((:file "package")))
