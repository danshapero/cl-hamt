(in-package :cl-hamt-test)

(def-suite hash-set-tests)
(in-suite hash-set-tests)

(test empty-set
      (is (= 0 (set-size (make-hash-set)))))

(defvar swinging-hepcats
  (-> (make-hash-set)
      (set-insert "Louis Armstrong")
      (set-insert "Earl Hines")
      (set-insert "Artie Shaw")
      (set-insert "Count Basie")
      (set-insert "Duke Ellington")))

(test inserting
  (is (= 5 (set-size swinging-hepcats)))
  (is-true (set-lookup swinging-hepcats "Earl Hines"))
  (is-false (set-lookup swinging-hepcats "Kenny G")))
