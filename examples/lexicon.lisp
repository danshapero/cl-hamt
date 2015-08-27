
(in-package :cl-hamt-examples)

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

;; Dictionary of each corpus mapping its name to the set of words
(defvar corpora
  (reduce (lambda (dict corpus)
            (dict-insert dict corpus (fetch-corpus corpus)))
          '("enron" "kos" "nips" "nytimes" "pubmed")
          :initial-value (empty-dict)))

;; How many words are common to every corpus?
(defvar common-words
  (dict-reduce-values #'set-intersection corpora (empty-set)))
