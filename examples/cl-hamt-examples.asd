
(asdf:defsystem #:cl-hamt-examples
  :depends-on (#:cl-hamt
               #:cl-ppcre
               #:drakma)
  :serial t
  :components
  ((:file "package")
   (:file "lexicon")))
