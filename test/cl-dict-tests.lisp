
(in-package :cl-dict-test)

(def-suite dictionary)
(in-suite dictionary)

(test empty-dict
  (is (not (dict-lookup (make-binary-tree #'string>) "hello"))))

(test inserting
  (let ((dict (dict-insert (make-binary-tree #'string>) "hello" "Mr. Bond")))
    (multiple-value-bind (val foundp) (dict-lookup dict "hello")
      (is (not (null foundp)))
      (is (eql val "Mr. Bond"))
      (is (= 1 (dict-size dict))))))


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
  (is (dict-lookup (dict-remove squares 4) 6)))

(test reduce
  (is (= 120 (dict-reduce-keys #'+ squares 0)))
  (is (equal (dict-reduce (lambda (alist k v)
                            (cons (cons k v) alist))
                          squares
                          '())
           '((0 . 0) (1 . 1) (2 . 4) (3 . 9) (4 . 16) (5 . 25)
             (6 . 36) (7 . 49) (8 . 64) (9 . 81) (10 . 100) (11 . 121)
             (12 . 144) (13 . 169) (14 . 196) (15 . 225)))))
