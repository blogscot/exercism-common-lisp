(defpackage :atbash-cipher
  (:use :cl)
  (:export :encode))

(in-package :atbash-cipher)

(defun group (text &optional (acc ""))
  "Splits text into space separated groups of length five"
  (if (<= (length text) 5)
      (concatenate 'string acc text)
      (group (subseq text 5) (concatenate 'string acc (subseq text 0 5) " "))))

(defun encode (plaintext)
  (flet ((transform (letter) (code-char (- 219 (char-code (char-downcase letter))))))
    (loop for char across plaintext
          when (alpha-char-p char) collect (transform char) into output
          when (digit-char-p char) collect char into output
          finally (return (group output)))))
