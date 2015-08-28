
(in-package #:cl-hamt-test)

(def-suite hash-dict-tests)
(in-suite hash-dict-tests)

(test empty-dict
  (is (not (dict-lookup (empty-dict) "hello"))))


(defvar pacers (dict-insert (empty-dict)
                            "Reggie Miller" 2.01
                            "Larry Bird" 2.06
                            "Detlef Schrempf" 2.08
                            "Paul George" 2.01
                            "Metta World Peace" 2.01))


(test inserting
  (multiple-value-bind (val foundp)
      (dict-lookup pacers "Detlef Schrempf")
    (is (not (null foundp)))
    (is (eql val 2.08)))
  (is (= 5 (dict-size pacers)))
  (is (eql 2.01 (dict-lookup pacers "Reggie Miller"))))


(defvar squares
  (labels ((f (d i)
             (if (= i 16)
                 d
                 (f (dict-insert d i (* i i)) (1+ i)))))
    (f (empty-dict) 0)))

(test removing
  (is (= 16 (dict-size squares)))
  (is (= 16 (dict-lookup squares 4)))
  (is-false (dict-lookup (dict-remove squares 4) 4))
  (is (= 15 (dict-size (dict-remove squares 4))))
  (is-true (dict-lookup (dict-remove squares 4) 6))
  (is (= 14 (dict-size (dict-remove squares 4 12))))
  (is (= 9 (dict-lookup (dict-remove squares 4 12) 3)))

  ;; Check that the data structure is persistent, i.e. a functional update
  ;; leaves the original HAMT intact
  (is-true (let ((fewer-squares (dict-remove squares 4)))
             (and (= 16 (dict-lookup squares 4))
                  (not (dict-lookup fewer-squares 4))))))


;; These pairs of strings hash to the same number under murmurhash.
(defvar some-word-collisions
  '(("PSYCHOANALYZE" . "BEDUCKS")
    ("PANSPERMIES" . "NONSELF")
    ("UNSIGHING" . "TURBITS")))

;; Make a HAMT with them so we can see if we've handled collisions right.
(defvar dict-with-collisions
  (labels ((f (dict word-pairs)
             (if word-pairs
                 (let ((word1 (caar word-pairs))
                       (word2 (cdar word-pairs)))
                   (f (dict-insert dict
                                   word1 word1
                                   word2 word2)
                      (cdr word-pairs)))
                 dict)))
    (f (empty-dict :test #'equal
                   :hash #'cl-murmurhash:murmurhash)
       some-word-collisions)))

(test collisions
   (is (equal 6 (dict-size dict-with-collisions)))

   ;; Check that all the keys were inserted properly
   (is (equal "PSYCHOANALYZE"
              (dict-lookup dict-with-collisions "PSYCHOANALYZE")))
   (is (equal "BEDUCKS"
              (dict-lookup dict-with-collisions "BEDUCKS")))
   (is (equal "PANSPERMIES"
              (dict-lookup dict-with-collisions "PANSPERMIES")))
   (is (equal "NONSELF"
              (dict-lookup dict-with-collisions "NONSELF")))
   (is (equal "UNSIGHING"
              (dict-lookup dict-with-collisions "UNSIGHING")))
   (is (not (dict-lookup dict-with-collisions "IRIDOCYCLITIS")))

   ;; Check that updating into a key with a hash conflict works
   (is (equal "KATY PERRY"
              (dict-lookup (dict-insert dict-with-collisions
                                        "NONSELF"
                                        "KATY PERRY")
                           "NONSELF")))

   ;; Check that removing a key with a hash collision still leaves the key
   ;; it originally collided with in the dictionary
   (is (equal "PSYCHOANALYZE"
              (dict-lookup (dict-remove dict-with-collisions "BEDUCKS")
                           "PSYCHOANALYZE"))))


(defun alist-same-contents-p (alist1 alist2)
  (labels ((f (alist)
             (if alist
                 (let ((p (car alist)))
                   (if (equal p (assoc (car p) alist2))
                       (f (cdr alist))
                       nil))
                 t)))
    (and (= (length alist1) (length alist2))
         (f alist1))))

(test reduce
  (is (= 120 (dict-reduce-keys #'+ squares 0)))
  (is (alist-same-contents-p
       (dict-reduce (lambda (alist k v)
                      (acons k v alist))
                    squares
                    '())
       '((0 . 0) (1 . 1) (2 . 4) (3 . 9) (4 . 16) (5 . 25)
         (6 . 36) (7 . 49) (8 . 64) (9 . 81) (10 . 100) (11 . 121)
         (12 . 144) (13 . 169) (14 . 196) (15 . 225)))))

(defvar tall-pacers (dict-filter (lambda (k v)
                                   (declare (ignore k))
                                   (> v 2.01))
                                 pacers))

(test filter
  (is (= 2 (dict-size tall-pacers)))
  (is-true (dict-lookup tall-pacers "Larry Bird"))
  (is-true (dict-lookup tall-pacers "Detlef Schrempf"))
  (is-false (dict-lookup tall-pacers "Reggie Miller")))
