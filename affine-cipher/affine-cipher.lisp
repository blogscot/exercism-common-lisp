(defpackage :affine-cipher
  (:use :cl :uiop)
  (:export :encode
   :decode))

(in-package :affine-cipher)

(defconstant +alphabet-length+ 26)

(defun group (text &optional (acc ""))
  "Splits text into space separated groups of length five"
  (if (<= (length text) 5)
      (concatenate 'string acc text)
      (group (subseq text 5) (concatenate 'string acc (subseq text 0 5) " "))))

(defun find-modular-multiplicative-inverse (a &optional (m +alphabet-length+))
      (loop for x from 1 below m
            when (= 1 (mod (* a x) m))
              return x))

(defun encrypt-char (ch a b)
  (if (alpha-char-p ch)
   (let ((index (- (char-code ch) (char-code #\a))))
     (code-char (+ (mod (+ (* a index) b) +alphabet-length+) (char-code #\a))))
   ch))

(defun encode (plaintext a b)
  (if (find-modular-multiplicative-inverse a)
   (let ((cleantext (remove-if-not #'alphanumericp (string-downcase plaintext))))
     (group (map 'string (lambda (ch) (encrypt-char ch a b)) cleantext)))))

(defun decrypt-char (ch mmi b)
  (if (alpha-char-p ch)
   (let ((y (- (char-code ch) (char-code #\a))))
     (code-char (+ (mod (* mmi (- y b)) +alphabet-length+) (char-code #\a))))
   ch))

(defun decode (ciphertext a b)
  (let ((cleantext (remove-if-not #'alphanumericp ciphertext))
        (mmi (find-modular-multiplicative-inverse a)))
    (when mmi
      (map 'string (lambda (ch) (decrypt-char ch mmi b)) cleantext))))
