cl-hamt
=======

[![Build Status](https://travis-ci.org/danshapero/cl-hamt.svg?branch=master)](https://travis-ci.org/danshapero/cl-hamt)

This library provides persistent dictionaries and sets in Common Lisp based on the hash array-mapped trie data structure.
The operations provided are:
```
size
lookup
insert
remove
reduce
filter
map
```
The versions for sets and dictionaries are obtained by prepending `set-` or `dict-` to the above symbols, so for example to lookup a key in a set and dictionary you would call `set-lookup` and `dict-lookup` respectively.
An empty collection is created with the functions `empty-set` and `empty-dict`.


Implementation
==============

The data types in cl-hamt are implemented using the [hash array-mapped trie](https://idea.popcount.org/2012-07-25-introduction-to-hamt/) (HAMT) data structure, as found in the Clojure language.
HAMTs provide near-constant time search, insertion and removal and can be used as persistent data structures, i.e. all updates are non-destructive.

As the name suggests, hash array-mapped tries use hashing of the underlying data to store and retrieve it efficiently.
Consequently, any natural ordering on the data, e.g. lexicographic ordering of strings, natural ordering of integers, is not preserved in a HAMT.
When using `reduce` on a collection, the operation in question must not depend on the order in which the elements are accessed.
If the data are ordered and this ordering is important, a self-balancing binary tree may be a more appropriate data structure.
Additionally, one must provide an appropriate 32-bit hash function.
We default to using `murmurhash`, as implemented in the Common Lisp package `cl-murmurhash`.
Note that the built-in Common Lisp hash function `sxhash` is not a 32-bit hash; for example, on my 64-bit system with SBCL, it returns a 62-bit hash.
