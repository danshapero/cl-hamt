
(in-package :cl-dict-test)

(def-suite dictionary)
(in-suite dictionary)

(test empty-dict
  (is (not (dict-lookup (make-binary-tree #'string>) "hello"))))

(test inserting
  (let ((dict (dict-insert (make-binary-tree #'string>) "hello" "Mr. Bond")))
    (multiple-value-bind (val foundp) (dict-lookup dict "hello")
      (is (not (null foundp)))
      (is (eql val "Mr. Bond")))))


(defvar dict
  (labels ((f (d i)
             (if (= i 16)
                 d
                 (f (dict-insert d i (* i i)) (1+ i)))))
    (f (make-binary-tree) 0)))

(test removing
  (is (= 16 (dict-lookup dict 4)))
  (is (null (-> dict
                (dict-remove 4)
                (dict-lookup 4)))))
