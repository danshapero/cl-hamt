
(in-package #:cl-hamt-examples)

(defvar ml-databases-uri
  "https://archive.ics.uci.edu/ml/machine-learning-databases/")

(defvar bag-of-words-uri
  (concatenate 'string ml-databases-uri "bag-of-words/"))

(defun fetch-corpus (corpus)
  "Given the name of a corpus, fetch the corresponding bag of words
from the online database."
  (let* ((uri (concatenate 'string
                           bag-of-words-uri "vocab." corpus ".txt"))
         (text (drakma:http-request uri)))
    (reduce #'set-insert
            (cl-ppcre:all-matches-as-strings "\\w+" text)
            :initial-value (empty-set))))

(defun fetch-corpora ()
  "Fetch all the word bags from UCI's database."
  (reduce (lambda (dict corpus)
            (dict-insert dict corpus (fetch-corpus corpus)))
          '("enron" "kos" "nips" "nytimes" "pubmed")
          :initial-value (empty-dict)))

(defun common-words ()
  "Return a set of words that are found in every corpus"
  (let ((corpora (fetch-corpora)))
    (dict-reduce-values #'set-intersection
                        corpora
                        (dict-lookup corpora "kos"))))
