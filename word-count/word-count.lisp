(defpackage :word-count
  (:use :cl)
  (:export :count-words))
(in-package :word-count)

(defun match (char)
  (find char " ,.:!@#$%^&
"))

(defun string-split (str)
  (loop for i = 0 then end
        as start = (position-if #'alphanumericp str :start i)
        while start
        as end = (position-if #'match str :start start)
        collect (string-right-trim "'" (subseq str start end))
        while end))

(defun count-words (sentence)
  (let* ((words (string-split (string-downcase sentence)))
         (keys (remove-duplicates words :test 'equal)))
    (loop for key in keys
          collect (cons key (count key words :test 'equal)))))
