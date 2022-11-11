(defpackage :socks-and-sexprs
  (:use :cl)
  (:export :lennys-favorite-food :lennys-secret-keyword
           :is-an-atom-p :is-a-cons-p :first-thing :rest-of-it))

(in-package :socks-and-sexprs)

;; Evaluates to some symbol (not a keyword)
(defun lennys-favorite-food ()
  'spaghetti)

;; Evaluates to some keyword
(defun lennys-secret-keyword ()
  :spaghetti)

;; Evaluates to T if THING is an atom, NIL otherwise
(defun is-an-atom-p (item)
  (not (consp item)))

;; Evaluates to T if THING is a cons, NIL otherwise
(defun is-a-cons-p (elem)
  (consp elem))

;; Evaluates to the first part of CONS
(defun first-thing (elem)
  (car elem))

;; Evaluates to the 'rest' of the CONS
(defun rest-of-it (elem)
  (cdr elem))
