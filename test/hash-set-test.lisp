(in-package #:cl-hamt-test)

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
                       :initial-value t))
  (is-true (set-lookup (set-remove set-with-collisions "PSYCHOANALYZE")
                       "BEDUCKS"))
  (is (= 5 (set-size (set-remove set-with-collisions "BEDUCKS")))))

(defvar max-number-value 1000)
(defvar some-numbers
  (loop for i from 0 to 100 collecting (random max-number-value)))

(defvar set-without-collisions
  (reduce (lambda (s p)
            (set-insert s (car p)))
          some-word-collisions
          :initial-value (empty-set :test #'equal
                                    :hash #'cl-murmurhash:murmurhash)))

(test set-equality
  (is-false (set-eq swinging-hepcats beboppers))
  (is-true (set-eq (set-union swinging-hepcats beboppers)
                   (set-union beboppers swinging-hepcats)))
  (is-true (set-eq set-with-collisions set-with-collisions))
  (is-true (let ((set1 (apply #'set-insert (cons (empty-set) some-numbers)))
                 (set2 (apply #'set-insert (cons (empty-set) some-numbers))))
             (and (not (eq set1 set2))
                  (set-eq set1 set2))))
  (is-false (set-eq (apply 'set-insert (cons (empty-set) some-numbers))
                    (apply 'set-insert
                           (cons (empty-set)
                                 (cons (+ 1 max-number-value) some-numbers)))))
  (is-false (set-eq set-with-collisions set-without-collisions))
  (is-true (set-eq (empty-set :test 'equal) (empty-set :test #'equal))))
