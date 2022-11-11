(defpackage :knapsack
  (:use :cl)
  (:export :maximum-value))

(in-package :knapsack)

(defun maximum-value (maximum-weight items)
  (let ((weights (map 'list (lambda (item) (cdr (assoc :weight item))) items))
        (values (map 'list (lambda (item) (cdr (assoc :value item))) items)))
    (knapsack maximum-weight weights values (length items))))

(defun knapsack (maximum-weight weights values n)
  (cond
    ((or (zerop maximum-weight) (zerop n)) 0)
    ((> (nth (1- n) weights) maximum-weight) (knapsack maximum-weight weights values (1- n)))
    (t (max (+ (nth (1- n) values) (knapsack (- maximum-weight (nth (1- n) weights)) weights values (1- n)))
            (knapsack maximum-weight weights values (1- n))))))
