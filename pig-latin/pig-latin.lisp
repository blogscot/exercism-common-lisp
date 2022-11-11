(defpackage :pig-latin
  (:use :cl :uiop)
  (:export :translate))

(in-package :pig-latin)

(defparameter vowel-sounds '("a" "e" "i" "o" "u" "xr" "yt"))

(defun starts-with-vowel-soundp (word)
  (some (lambda (vowel) (string-prefix-p vowel word)) vowel-sounds))

(defun get-consonant-cluster (word)
  (loop for str = word then (subseq str 1)
        until (or (emptyp str) (starts-with-vowel-soundp str))
        collect (char str 0) into prefix
        finally (return (values (coerce prefix 'string)
                                (subseq word (length prefix))))))

(defun consonant-cluster-withp (phrase str)
  (let ((position (search str phrase)))
    (when position
      (let ((prefix (subseq phrase 0 position)))
        (when (plusp (length prefix))
            (notany (lambda (vowel) (search vowel prefix)) vowel-sounds))))))

(defun translate-helper (word str &optional &key (move nil))
  (let* ((position (search str word))
         (prefix-length (+ position (if move (length str) 0))))
    (strcat (subseq word prefix-length) (subseq word 0 prefix-length))))

(defun translate-word (word)
  (multiple-value-bind (prefix suffix) (get-consonant-cluster word)
   (strcat (cond
             ((starts-with-vowel-soundp word) word)
             ((string-prefix-p "qu" word) (strcat (subseq word 2) "qu"))
             ((consonant-cluster-withp word "qu") (translate-helper word "qu" :move t))
             ((consonant-cluster-withp word "y") (translate-helper word "y"))
             (t (strcat suffix prefix)))
           "ay")))

(defun translate (phrase)
  (format nil "~{~a~^ ~}" (mapcar #'translate-word (split-string phrase))))
