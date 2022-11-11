(defpackage :spiral-matrix
  (:use :cl)
  (:export :spiral-matrix))

(in-package :spiral-matrix)

(defclass matrix ()
  ((spiral
    :initarg :spiral
    :accessor spiral)
   (pos
    :initform '(0 0)
    :accessor pos)
   (direction
    :initform 0
    :accessor direction)
   (directions
    :initform '((0 1) (1 0) (0 -1) (-1 0))
    :reader directions)))

(defmethod boundryp ((m matrix) (position list))
  "Is the position outside the array dimensions or already occupied?"
  (let ((arr (spiral m)))
   (destructuring-bind (row col) position
     (destructuring-bind (num-rows num-cols) (array-dimensions arr)
       (or (minusp row) (minusp col) (>= row num-rows) (>= col num-cols)
           (aref arr row col))))))

(defmethod turn ((m matrix))
  (setf (direction m) (mod (incf (direction m)) (length (directions m)))))

(defmethod set-value ((m matrix) (value integer))
  (with-accessors ((spiral spiral) (pos pos)) m
    (destructuring-bind (row col) pos
     (setf (aref spiral row col) value))))

(defmethod next-position ((m matrix))
  (mapcar #'+ (pos m) (nth (direction m) (directions m))))

(defmethod move ((m matrix))
  "Move to next position. If a boundary has been reached then
   turn first then move."
  (let ((new-pos (next-position m)))
    (when (boundryp m new-pos)
      (turn m)
      (setf new-pos (next-position m)))
    (unless (boundryp m new-pos)
      (setf (pos m) new-pos))))

(defun spiral-matrix (size)
  (when (plusp size)
   (let* ((spiral (make-array (list size size) :initial-element nil))
          (matrix (make-instance 'matrix :spiral spiral)))
     (dotimes (n (* size size))
       (set-value matrix (1+ n))
       (move matrix))
     (spiral matrix))))
