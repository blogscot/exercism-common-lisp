(defpackage :food-chain
  (:use :cl)
  (:export :recite))

(in-package :food-chain)

(defparameter line1 "I know an old lady who swallowed a ~a.")
(defparameter swallows "She swallowed the ~a to catch the ~a.")
(defparameter ending1 "I don't know why she swallowed the fly. Perhaps she'll die.")
(defparameter ending2 "She's dead, of course!")

(defparameter food-items
  '(("fly" nil "fly" ending1)
    ("spider" "It wriggled and jiggled and tickled inside her." "spider that wriggled and jiggled and tickled inside her" ending1)
    ("bird" "How absurd to swallow a bird!" "bird" ending1)
    ("cat" "Imagine that, to swallow a cat!" "cat" ending1)
    ("dog" "What a hog, to swallow a dog!" "dog" ending1)
    ("goat" "Just opened her throat and swallowed a goat!" "goat" ending1)
    ("cow" "I don't know how she swallowed a cow!" "cow" ending1)
    ("horse" nil nil ending2)))

(defun get-description (food-item)
  (third (find-if (lambda (x) (equal x food-item)) food-items :key #'first)))

(defun swallow (number)
  (reverse
   (loop for (a b) on (subseq (mapcar #'first food-items) 0 number)
         when b
           collect (format nil swallows b (get-description a)))))

(defun recite-verse (number)
  (destructuring-bind (name statement description ending) (nth (1- number) food-items)
    (declare (ignore description))
    (append (list (format nil line1 name))
            (when statement
              (list statement))
            (when statement
              (swallow number))
            (if (eql ending 'ending1) (list ending1) (list ending2)))))

(defun join (verses &optional (acc nil))
  (if (= 1 (length verses))
      (concatenate 'list acc verses)
      (join (rest verses) (concatenate 'list acc (list (first verses) (list ""))))))

(defun recite (start-verse end-verse)
  (format nil "~{~A~^~%~}" (apply #'concatenate 'list
          (join (loop for verse from start-verse to end-verse
                      collect (recite-verse verse))))))
