
(in-package :cl-dict-test)

(def-suite dictionary)
(in-suite dictionary)

(test empty-dict
  (is (not (dict-lookup (make-binary-tree #'string>) "hello"))))


(defvar pacers (-> (make-binary-tree #'string>)
                   (dict-insert "Reggie Miller" 2.01)
                   (dict-insert "Larry Bird" 2.06)
                   (dict-insert "Detlef Schrempf" 2.08)
                   (dict-insert "Paul George" 2.01)
                   (dict-insert "Metta World Peace" 2.01)))


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
    (f (make-binary-tree) 0)))

(test removing
  (is (= 16 (dict-size squares)))
  (is (= 16 (dict-lookup squares 4)))
  (is (null (-> squares
                (dict-remove 4)
                (dict-lookup 4))))
  (is (= 15 (dict-size (dict-remove squares 4))))
  (is (dict-lookup (dict-remove squares 4) 6))
  (is (= 14 (-> squares
                (dict-remove 4)
                (dict-remove 12)
                dict-size)))
  (is (= 9 (-> squares
               (dict-remove 4)
               (dict-remove 12)
               (dict-lookup 3)))))

(test reduce
  (is (= 120 (dict-reduce-keys #'+ squares 0)))
  (is (equal (dict-reduce (lambda (alist k v)
                            (acons k v alist))
                          squares
                          '())
           '((0 . 0) (1 . 1) (2 . 4) (3 . 9) (4 . 16) (5 . 25)
             (6 . 36) (7 . 49) (8 . 64) (9 . 81) (10 . 100) (11 . 121)
             (12 . 144) (13 . 169) (14 . 196) (15 . 225)))))
