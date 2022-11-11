(defpackage :sublist
  (:use :cl)
  (:export :sublist))

(in-package :sublist)

(defun sublist (list1 list2)
  "what is list1 of list2 (sublist, superlist, equal or unequal)"
  (cond
    ((equal list1 list2)   :equal)
    ((search* list2 list1) :superlist)
    ((search* list1 list2) :sublist)
    (t                     :unequal)))

(defun search* (list1 list2)
  "Tests if list1 is a sublist of list2"
  (let ((len1 (length list1))
        (len2 (length list2)))
    (if (<= len1 len2)
        (some #'identity (loop for l on list2
                               when (>= (length l) len1)
                                 collect (equal list1 (subseq l 0 len1)))))))
