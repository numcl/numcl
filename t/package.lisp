#|

This file is a part of NUMCL project.
Copyright (c) 2019 IBM Corporation
SPDX-License-Identifier: LGPL-3.0-or-later

NUMCL is free software: you can redistribute it and/or modify it under the terms
of the GNU General Public License as published by the Free Software
Foundation, either version 3 of the License, or (at your option) any
later version.

NUMCL is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A
PARTICULAR PURPOSE.  See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License along with
NUMCL.  If not, see <http://www.gnu.org/licenses/>.

|#

(in-package :cl-user)
(defpackage :numcl.test
  (:shadowing-import-from :numcl :flatten :shuffle)
  (:use :numcl
        :fiveam
        :trivia :alexandria :iterate))
(in-package :numcl.test)



(def-suite :numcl)
(in-suite :numcl)

;; run test with (run! test-name) 

(defun same-base-array-p (a b)
  (match* (a b)
    (((array :displaced-to ab)
      (array :displaced-to (eq ab)))
     t)))

(test (numcl :compile-at :run-time)
  (is-true (typep (zeros 5)
                  '(array bit (5))))
  (is-true (typep (zeros '(5 5))
                  '(array bit (5 5))))
  (is-true (typep (zeros 5 :type 'fixnum)
                  '(array fixnum (5))))
  (is-true (typep (ones 5)
                  '(array bit (5))))
  (is-true (typep (arange 10)
                  '(array (integer 0 10) (10))))

  (let ((a (arange 100)))
    (is-false (eq a (copy a)))
    (is-false (eq (copy a) (copy a)))
    (is-true  (equalp a (copy a)))))

(test (aref :compile-at :run-time)
  (let* ((a (zeros '(3 4 5 6))))
    
    (is-true (same-base-array-p a a))

    (is-true (same-base-array-p a (aref a '(0 2))))
    (is (equal '(2 4 5 6) (shape (aref a '(0 2)))))
    (is (equal '(2 4 5 6) (shape (aref a '(0 2) t))))
    (is (equal '(2 4 5 6) (shape (aref a '(0 2) t t))))
    (is (equal '(2 4 5 6) (shape (aref a '(0 2) t t t))))
    (is (equal '(2 4 5 6) (shape (aref a '(0 2) '(t t)))))
    (is (equal '(2 4 5 6) (shape (aref a '(0 2) '(t t) '(t t)))))
    (is (equal '(2 4 5 6) (shape (aref a '(0 2) '(t t) '(t t) '(t t)))))
    (is (equal '(2 4 5 6) (shape (aref a '(0 2) '(t t) t '(t t)))))
    (is (equal '(3 4) (shape (aref a '- 3 3))))
    (is (equal '(4 5) (shape (aref a 2 '- 3))))
    (is (equal '(5 6) (shape (aref a 2 3 '-))))

    
    (is-true (same-base-array-p a (aref a '(0 2) t t)))
    (is (equal '(2 4 5 6) (shape (aref a '(0 2)))))

    (is-true (same-base-array-p a (aref a 0 0)))
    (is (equal '(5 6) (shape (aref a 0 0))))
    
    (is-true (same-base-array-p a (aref a '(0 1) '(0 1))))
    (is (equal '(1 1 5 6) (shape (aref a '(0 1) '(0 1)))))

    (is (numberp (aref a 0 0 0 0)))

    ;; range indices outside the shape are ignored
    (is (equal (shape (aref a '(0 6)))
               (shape (aref a '(0 4))))))

  
  
  (let ((a (reshape (arange 100) '(4 5 5))))
    (is-true (typep a
                    '(array (integer 0 100) (4 5 5))))
    (is-true (typep (aref a 0)
                    '(array (integer 0 100) (5 5))))
    (is-true (typep (aref a 0 0 0)
                    '(integer 0 100)))
    (is-true (typep (aref a '(0 1))
                    '(array (integer 0 100) (1 5 5))))
    (is-true (typep (aref a 0 '(0 2) 0)
                    '(array (integer 0 100) (2))))
    (is-true (typep (aref a '(0 2) '(0 2) 0)
                    '(array (integer 0 100) (2 2))))
    (is-true (typep (aref a '- '(0 2))
                    '(array (integer 0 100) (4 5 2)))))

  (let ((a (zeros 4 :type 'bit)))
    (setf (aref a '(0 4 2)) 1)
    (is (equalp a #(1 0 1 0))))
  )

(test (aset-fill :compile-at :run-time)
  (let ((a (zeros '(2 2) :type 'fixnum)))
    (setf (aref a 1 0) 1)
    
    (is (equalp a #2A((0 0)
                      (1 0)))))

  (let ((a (zeros '(2 2) :type 'fixnum)))
    (setf (aref a 1) 1)
    
    (is (equalp a #2A((0 0)
                      (1 1)))))

  (let ((a (zeros '(2 2) :type 'fixnum)))
    (setf (aref a t 1) 1)
    ;; non contiguous
    (is (equalp a #2A((0 1)
                      (0 1))))))



(test (aset-replace-shape-error :compile-at :run-time)
  (let ((a (zeros '(10 10 10) :type 'fixnum)))
    (finishes
      (setf (aref a '(0 2) '(0 2) 1) (zeros '(2 2))))
    (finishes
      (setf (aref a '(0 2) 1 '(0 2)) (zeros '(2 2))))
    (finishes
      (setf (aref a 1 '(0 2) '(0 2)) (zeros '(2 2))))
    ;;
    (signals error
      (setf (aref a '(0 2) '(0 2) '(0 1)) (zeros '(2 2))))
    (signals error
      (setf (aref a '(0 2) '(0 1) '(0 2)) (zeros '(2 2))))
    ;; broadcast
    (finishes
      (setf (aref a '(0 1) '(0 2) '(0 2)) (zeros '(2 2))))
    (finishes
      (setf (aref a '(0 2) '(0 2) '(0 2)) (zeros '(2 2))))
    (finishes
      (setf (aref a '(0 3) '(0 2) '(0 2)) (zeros '(2 2))))
    (finishes
      (setf (aref a '(0 4) '(0 2) '(0 2)) (zeros '(2 2))))
    
    (signals error
      (setf (aref a '(0 3) '(0 2) 1) (zeros '(2 2))))
    (signals error
      (setf (aref a '(0 3) 1 '(0 2)) (zeros '(2 2))))
    (signals error
      (setf (aref a 1 '(0 3) '(0 2)) (zeros '(2 2))))
    
    (signals error
      (setf (aref a '(0 2) '(0 4) 1) (zeros '(2 2))))
    (signals error
      (setf (aref a '(0 2) 1 '(0 4)) (zeros '(2 2))))
    (signals error
      (setf (aref a 1 '(0 2) '(0 4)) (zeros '(2 2))))))

(test (aset-replace :compile-at :run-time)
  (let ((a (zeros '(4 4) :type 'fixnum)))
    (setf (aref a '(1 3) '(1 3))
          #2A((0 1)
              (1 0)))
    
    (is (equalp #2A((0 0 0 0)
                    (0 0 1 0)
                    (0 1 0 0)
                    (0 0 0 0))
                a)))

  (let ((a (zeros '(3 4 4) :type 'fixnum)))
    (setf (aref a t '(1 3) '(1 3))
          #2A((0 1)
              (1 0)))
    
    (is (equalp #3A(((0 0 0 0)
                     (0 0 1 0)
                     (0 1 0 0)
                     (0 0 0 0))
                    
                    ((0 0 0 0)
                     (0 0 1 0)
                     (0 1 0 0)
                     (0 0 0 0))
                    
                    ((0 0 0 0)
                     (0 0 1 0)
                     (0 1 0 0)
                     (0 0 0 0)))
                a)))

  (let ((a (zeros '(3 4 4) :type 'fixnum)))
    (setf (aref a 1 '(1 3) '(1 3))
          #2A((0 1)
              (1 0)))
    
    (is (equalp #3A(((0 0 0 0)
                     (0 0 0 0)
                     (0 0 0 0)
                     (0 0 0 0))
                    
                    ((0 0 0 0)
                     (0 0 1 0)
                     (0 1 0 0)
                     (0 0 0 0))
                    
                    ((0 0 0 0)
                     (0 0 0 0)
                     (0 0 0 0)
                     (0 0 0 0)))
                a))

    (is (equalp #2A((0 1)
                    (1 0))
                (aref a 1 '(1 3) '(1 3)))))

  (let ((a (zeros '(3 4 4) :type 'fixnum)))
    (setf (aref a '(0 2) '(1 3) 1)
          #2A((0 1) (1 0)))
    
    (is (equalp #2A((0 1) (1 0))
                (aref a '(0 2) '(1 3) 1)))

    (is (equalp #3A(((0 0 0 0)
                     (0 0 0 0)
                     (0 1 0 0)
                     (0 0 0 0))
                    
                    ((0 0 0 0)
                     (0 1 0 0)
                     (0 0 0 0)
                     (0 0 0 0))
                    
                    ((0 0 0 0)
                     (0 0 0 0)
                     (0 0 0 0)
                     (0 0 0 0)))
                a))))

(test (reshape :compile-at :run-time)
  (is (equal '(4 5 5)
             (shape (reshape (arange 100) '(4 5 5)))))
  
  
  (is (equal '(4 5 5)
             (shape (reshape (arange 100) '(-1 5 5)))))

  
  (is (equal '(5 5 4)
             (shape (reshape (arange 100) '(-1 5 4)))))

  
  (signals error
    ;; t=100, t=100
    (reshape (arange 100) '(t 5 t)))
  
  (signals error
    ;; t=100, t=100
    (reshape (arange 100) '(t -1 t)))

  
  (is (equal '(3 2 2 10)
             (shape (reshape (reshape (arange 120) '(3 4 10))
                             '(t -1 2 t))))))

(test (1+/1- :compile-at :run-time)
  (is (equal (upgraded-array-element-type '(unsigned-byte 2))
             (array-element-type
              (1+ (zeros 10))))))


(test (arithmetic :compile-at :run-time)

  (is (typep (/ 1 2) 'ratio))
  (is (typep (/ (ones 5) 2) '(array single-float)))
  (is (subtypep (array-element-type (* (ones 5) 2)) 'fixnum))
  (is (subtypep (array-element-type (+ (ones 5) 2)) 'fixnum))
  (is (subtypep (array-element-type (- (ones 5) 2)) 'fixnum))
  (is (subtypep (array-element-type (max (ones 5) 2)) 'fixnum))
  (is (subtypep (array-element-type (min (ones 5) 2)) 'fixnum)))

(test (arange :compile-at :run-time)
  (is (= 5 (length (arange 5))))
  (is (= 5 (length (arange 5 10)))))

(test (asarray :compile-at :run-time)
  (is (= 1 (array-rank (asarray '((1) (1 2))))))
  (is (= 2 (array-rank (asarray '(((1) (1 2)) ((3) (3 4)))))))
  (is (= 2 (array-rank (asarray '((1 2) (3 4))))))
  (is (= 2 (array-rank (asarray #(#(1 2) #(3 4))))))
  (is (= 2 (array-rank (asarray #((1 2) (3 4))))))
  
  (is (= 2 (array-rank (asarray #(#(1 2) #(3 4))))))
  (is (= 1 (array-rank (asarray #(#(1 2) #(3 4 5))))))
  (is (= 2 (array-rank (asarray #("aa" "aa")))))
  (is (= 1 (array-rank (asarray #("aa" "aaa")))))
  
  (is (= 1 (array-rank (asarray #(#(1 2) #(3 4))   :type 'vector))))
  (is (= 1 (array-rank (asarray #(#(1 2) #(3 4 5)) :type 'vector))))
  (is (= 1 (array-rank (asarray #("aa" "aa")  :type 'string))))
  (is (= 1 (array-rank (asarray #("aa" "aaa") :type 'string))))
  (signals error
    (asarray '((1 2) (3 4))   :type '(array fixnum (* *)))))
