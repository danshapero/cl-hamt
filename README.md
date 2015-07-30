cl-hamt
=======

This library provides persistent dictionaries and sets in Common Lisp, based on the hash array-mapped trie (HAMT) data structure as found in Clojure.

As the name suggests, HAMTs use hashing of the underlying data, for which one must provide an appropriate hash function.
We default to using `murmurhash`, as implemented in the Common Lisp package `cl-murmurhash`; however, any 32-bit hash will work.
Note that the built-in Common Lisp hash function `sxhash` is not a 32-bit hash; for example, on my 64-bit system with SBCL, it returns a 62-bit hash.
