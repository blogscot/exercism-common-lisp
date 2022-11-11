(defpackage :two-bucket
  (:use :cl)
  (:export :measure))

(in-package :two-bucket)

(defun compare (a b) (equal (rest a) (rest b)))

(defmacro add-to (item set) `(pushnew ,item ,set :test #'compare))

(defun next (buckets one-max two-max)
  (let ((values nil))
    (destructuring-bind (moves one two) buckets
      ;; empty
      (add-to (list (1+ moves) 0 two) values)
      (add-to (list (1+ moves) one 0) values)
      ;; fill
      (add-to (list (1+ moves) one-max two) values)
      (add-to (list (1+ moves) one two-max) values)
      ;; pour
      (let ((delta (min one (- two-max two))))
        (add-to (list (1+ moves) (- one delta) (+ two delta)) values))
      (let ((delta (min two (- one-max one))))
        (add-to (list (1+ moves) (+ one delta) (- two delta)) values)))
    values))

(defun goal-reached (bucket-list goal)
  (find-if (lambda (buckets) (find goal (rest buckets))) bucket-list))

(defun announce (other-bucket goal-bucket moves)
  (pairlis '(:other-bucket :goal-bucket :moves) (list other-bucket goal-bucket moves)))

(defun measure* (bucket-one bucket-two goal start-bucket)
  (let ((history (if (eql start-bucket :one)
                     `((1 0 ,bucket-two)) `((1 ,bucket-one 0))))
        (next-step nil)
        (buckets (if (eql start-bucket :one)
                     `((1 ,bucket-one 0)) `((1 0 ,bucket-two)))))
    (loop for goal-found = (goal-reached buckets goal)
          until goal-found
          do (progn
               (push (first buckets) history)
               (setf next-step (union (next (first buckets) bucket-one bucket-two) next-step :test #'compare))
               (setf buckets (set-difference next-step history :test #'compare)))
          finally (return (destructuring-bind (moves one two) goal-found
                            (if (= goal one)
                                (announce two :one moves)
                                (announce one :two moves)))))))

(defun measure (bucket-one bucket-two goal start-bucket)
  "Function to solve the two-bucket puzzle, if possible, when given the capacities
of both buckets, a goal, and which bucket to start with.  Returns an a list of moves
required to reach the goal, the name of the bucket that reach the goal, and the
amount of water left over in the other bucket."
  (cond
    ((plusp (mod goal (gcd bucket-one bucket-two))) nil)
    ((and (> goal bucket-one) (> goal bucket-two)) nil)
    (t (measure* bucket-one bucket-two goal start-bucket))))
