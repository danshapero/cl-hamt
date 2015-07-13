
(asdf:defsystem #:cl-dict
  :description "Dictionary data structures"
  :author "Daniel Shapero <shapero.daniel@gmail.com>"
  :license "BSD"
  ;;:depends-on (#:fiveam)
  :serial t
  :components ((:file "package")
               (:file "dictionary")
               (:file "binary-tree")))
