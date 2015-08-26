
(in-package :cl-hamt-examples)

(defvar prince-uri
  "http://www.gutenberg.org/cache/epub/1232/pg1232.txt")

(defvar discourses-uri
  "http://www.gutenberg.org/cache/epub/10827/pg10827.txt")

(defun add-word (dict word)
  (multiple-value-bind (count foundp)
      (dict-lookup dict word)
    (dict-insert dict word (if foundp (1+ count) 0))))

(defun word-frequency (text)
  (dict-filter (lambda (word count)
                 (and (> count 1) (> (length word 1))))
               (reduce #'add-word
                       (cl-ppcre:all-matches-as-strings "\\w+" text)
                       :initial-value (empty-dict))))
