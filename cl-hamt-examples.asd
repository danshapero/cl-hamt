
(asdf:defsystem #:cl-hamt-examples
  :description "Example code for using cl-hamt"
  :author "Daniel Shapero <shapero.daniel@gmail.com>"
  :license "BSD 3-clause"
  :depends-on (#:cl-hamt
               #:cl-ppcre
               #:drakma)
  :serial t
  :components
  ((:module "examples"
    :serial t
    :components
    ((:file "package")
     (:file "lexicon")))))
