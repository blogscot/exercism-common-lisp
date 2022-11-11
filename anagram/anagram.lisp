(defpackage :anagram
  (:use :cl)
  (:export :anagrams-for))

(in-package :anagram)

(defun anagrams-for (subject candidates)
  "Returns a sublist of candidates which are anagrams of the subject."
  (labels ((string-sort (str)
             (sort (string-downcase (copy-seq str)) #'char<)))
   (remove-if-not (lambda (word)
                    (equal (string-sort subject) (string-sort word)))
                  (remove-if (lambda (word) (equalp word subject)) candidates))))
