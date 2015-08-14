
## A word on the implementation

The `hash-dict` and `hash-set` data structures that the user will see are wrappers around classes for the trie's nodes.
There are three types of trie nodes: leaves, which store the actual data; table nodes, which store a bitmap and array to index the key hashes; and conflict nodes, which are like leaves in the event of a hash collision.
These are defined in `hamt.lisp`.
The call signature for operations on these classes is different from that of the wrapper types because the hash and tree depth must be passed explicitly.
Operations on these classes are prepended with a `%`.

Some of the operations, such as getting the size of a collection or removing a key from it, can be implemented the same way whether the HAMT is storing a set or a dictionary.
Other operations differ between the two; for example, accessing elements in conflict nodes is different for sets and dictionaries because a set stores a list while a dictionary stores an association list when there is a hash collision.
Shared code is found in `hamt.lisp`, while operations specific to dictionaries are sets are found in `hash-dict.lisp` and `hash-set.lisp` respectively.

Operations on table nodes require lots of repetitive bit manipulation.
This has been factored out into the macro `with-table` in `hamt.lisp`.

Many of the higher-level operations on collections are implemented in terms of `reduce`.