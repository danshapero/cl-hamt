
(asdf:defsystem #:cl-dict
  :description "Dictionary data structures"
  :author "Daniel Shapero <shapero.daniel@gmail.com>"
  :license "BSD"
  :depends-on (#:yoda #:cl-murmurhash)
  :serial t
  :components ((:file "package")
               (:file "dictionary")
               (:file "hash-array-mapped-trie")))
