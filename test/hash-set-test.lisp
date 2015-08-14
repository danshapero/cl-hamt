(in-package :cl-hamt-test)

(def-suite hash-set-tests)
(in-suite hash-set-tests)

(test empty
      (is (= 0 (set-size (empty-set)))))

(defvar swinging-hepcats
  (set-insert (empty-set)
              "Louis Armstrong"
              "Earl Hines"
              "Artie Shaw"
              "Count Basie"
              "Duke Ellington"
              "Coleman Hawkins"))

(defvar beboppers
  (set-insert (empty-set)
              "Coleman Hawkins"
              "Charlie Parker"
              "Dizzy Gillespie"
              "Bud Powell"
              "Miles Davis"))

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
    (f (empty-set) 0)))

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


;; These pairs of strings hash to the same number under murmurhash.
(defvar some-word-collisions
  '(("PSYCHOANALYZE" . "BEDUCKS")
    ("PANSPERMIES" . "NONSELF")
    ("UNSIGHING" . "TURBITS")))

(defvar set-with-collisions
  (reduce (lambda (s p)
            (set-insert s (car p) (cdr p)))
          some-word-collisions
          :initial-value (empty-set :test #'equal
                                    :hash #'cl-murmurhash:murmurhash)))

(test collisions
      (is (equal 6 (set-size set-with-collisions)))
      (is-true (reduce (lambda (correct word)
                         (and correct
                              (set-lookup set-with-collisions
                                          word)))
                       '("PSYCHOANALYZE"
                         "BEDUCKS"
                         "PANSPERMIES"
                         "NONSELF"
                         "UNSIGHING"
                         "TURBITS")
                       :initial-value t)))
