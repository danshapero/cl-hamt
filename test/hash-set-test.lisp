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
      (set-insert "Duke Ellington")
      (set-insert "Coleman Hawkins")))

(defvar beboppers
  (-> (make-hash-set)
      (set-insert "Coleman Hawkins")
      (set-insert "Charlie Parker")
      (set-insert "Dizzy Gillespie")
      (set-insert "Bud Powell")
      (set-insert "Miles Davis")))

(test inserting
  (is (= 6 (set-size swinging-hepcats)))
  (is-true (set-lookup swinging-hepcats "Earl Hines"))
  (is-false (set-lookup swinging-hepcats "Kenny G")))

(test removing
  (is-false (set-lookup (set-remove swinging-hepcats
                                    "Coleman Hawkins")
                        "Coleman Hawkins"))
  (is (= 5 (set-size (set-remove swinging-hepcats "Coleman Hawkins")))))


(defun integer-set (n)
  (labels ((f (s k)
             (if (= k n)
                 s
                 (f (set-insert s k) (1+ k)))))
    (f (make-hash-set) 0)))

(test reducing
  (is (= 45 (set-reduce #'+ (integer-set 10) 0))))

(defvar hepcats-and-beboppers
  (set-filter (lambda (person)
                (set-lookup beboppers person))
              swinging-hepcats))

(test filtering
  (is-true (set-lookup hepcats-and-beboppers "Coleman Hawkins"))
  (is (= 1 (set-size hepcats-and-beboppers))))

(test mapping
  (is-true (set-lookup (set-map (lambda (k) (* k k))
                                (integer-set 10))
                       81)))
