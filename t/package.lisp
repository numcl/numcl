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

#+sbcl
(declaim (sb-ext:muffle-conditions sb-ext:compiler-note))
(in-package :cl-user)
(uiop:define-package :numcl.test
  (:use)
  (:mix :numcl
        :fiveam
        :trivia :alexandria :iterate))
(in-package :numcl.test)

(def-fixture muffle ()
  #+sbcl
  (declare (sb-ext:muffle-conditions sb-ext:compiler-note style-warning))
  (&body))


(def-suite :numcl)
(in-suite :numcl)

;; run test with (run! test-name) 

(defun same-base-array-p (a b)
  (match* (a b)
    (((array :displaced-to ab)
      (array :displaced-to (eq ab)))
     t)))

(defmacro is-type (form type)
  (once-only (form)
    `(is-true (typep ,form ,type)
              "Type mismatch. Expected type ~a, actual: ~a"
              ,type (type-of ,form))))

(test (numcl :compile-at :run-time :fixture muffle)
  (is-type (zeros 5)
           '(array bit (5)))
  (is-type (zeros '(5 5))
           '(array bit (5 5)))
  (is-type (zeros 5 :type 'fixnum)
           '(array fixnum (5)))
  (is-type (ones 5)
           '(array bit (5)))
  (is-type (arange 10)
           '(array (integer 0 10) (10)))

  (let ((a (arange 100)))
    (is-false (eq a (copy a)))
    (is-false (eq (copy a) (copy a)))
    (is-true  (equalp a (copy a)))))

(test (aref-shape1 :compile-at :run-time :fixture muffle)
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
               (shape (aref a '(0 4)))))))

  
  
(test (aref-shape2 :compile-at :run-time :fixture muffle)
  (let ((a (reshape (arange 100) '(4 5 5))))
    (is-type a
             '(array (integer 0 100) (4 5 5)))
    (is-type (aref a 0)
             '(array (integer 0 100) (5 5)))
    (is-type (aref a 0 0 0)
             '(integer 0 100))
    (is-type (aref a '(0 1))
             '(array (integer 0 100) (1 5 5)))
    (is-type (aref a 0 '(0 2) 0)
             '(array (integer 0 100) (2)))
    (is-type (aref a '(0 2) '(0 2) 0)
             '(array (integer 0 100) (2 2)))
    (is-type (aref a '- '(0 2))
             '(array (integer 0 100) (4 5 2)))))

(test (aref1 :compile-at :run-time :fixture muffle)
  (let ((a (zeros 4 :type 'bit)))
    (setf (aref a '(0 4 2)) 1)
    (is (equalp a #(1 0 1 0)))))

(test (aref2 :compile-at :run-time :fixture muffle)
  (let ((a (arange 4)))

    (signals invalid-array-index-error (aref a 5))
    (signals invalid-array-index-error (aref a 4))
    (iter (for i from 0 to 3)
          (is (= i (aref a i))))
    (iter (for i from -4 to -1)
          (for j from 0 to 3)
          (is (= j (aref a i))))
    (signals invalid-array-index-error (aref a -5))))

(test (aref3 :compile-at :run-time :fixture muffle)
  ;; note: end index = -1 in the single index acess: last element
  (is (equalp (aref (arange 5) -1) 4))
  ;; note: end index = -1 in the slice means NOT the last element, but the second-last element 
  (is (equalp (aref (arange 5) '(-3 -1)) #(2 3)))
  ;; to denote the last element use T
  (is (equalp (aref (arange 5) '(-3 t)) #(2 3 4)))
  
  ;; range access : when part of the range is out-of-range, it is ignored
  (is (equalp (aref (arange 5) '(-5 t))
              (aref (arange 5) '(-6 t))))
  (is (equalp (aref (arange 5) '(-5 t))
              (aref (arange 5) '(-7 t))))
  (is (equalp (aref (arange 5) '(-5 t))
              (aref (arange 5) '(-8 t))))
  (is (equalp (aref (arange 5) '(-5 -1))
              (aref (arange 5) '(-6 -1))))
  (is (equalp (aref (arange 5) '(-5 -1))
              (aref (arange 5) '(-7 -1))))
  (is (equalp (aref (arange 5) '(-5 -1))
              (aref (arange 5) '(-8 -1))))
  ;; range access : when the range is out-of-range for the given array,
  ;; an array with size-0 axis is returned
  (is (equalp (empty 0)
              (aref (arange 5) '(-8 -5))))
  (is (equalp (empty 0)
              (aref (arange 5) '(6 10))))

  (is (equalp (zeros '(0 100))
              (aref (zeros '(0 100)) t)))
  (is (equalp (zeros '(0 100))
              (aref (zeros '(5 100)) '(6 10)))))

(test (aset-fill :compile-at :run-time :fixture muffle)
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



(test (aset-replace-shape-error :compile-at :run-time :fixture muffle)
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

(test (aset-replace :compile-at :run-time :fixture muffle)
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

(test (reshape :compile-at :run-time :fixture muffle)
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

(test (1+/1- :compile-at :run-time :fixture muffle)
  (is-type (1+ (zeros 10))
           '(array (unsigned-byte 2))))

(test (map-array :compile-at :run-time :fixture muffle)
  (let ((a (zeros '(3 3) :type 'single-float)))
    (map-array-into a
                    'cl:+
                    (reshape (arange 9) '(3 3))
                    (ones '(3 3)))
    (is (equalp a
                #2A((1.0 2.0 3.0) (4.0 5.0 6.0) (7.0 8.0 9.0)))))

  (let ((a (map-array 'cl:+
                      (reshape (arange 9) '(3 3))
                      (ones '(3 3)))))
    (is (subtypep (array-element-type a) 'fixnum))
    (is (equalp a #2A((1 2 3) (4 5 6) (7 8 9))))))



(test (arithmetic :compile-at :run-time :fixture muffle)

  (finishes (print (sin pi)))
  (is-type (/ 1 2) 'ratio)
  (is-type (/ (ones 5) 2) '(array single-float))
  (is (subtypep (array-element-type (* (ones 5) 2)) 'fixnum))
  (is (subtypep (array-element-type (+ (ones 5) 2)) 'fixnum))
  (is (subtypep (array-element-type (- (ones 5) 2)) 'fixnum))
  (is (subtypep (array-element-type (max (ones 5) 2)) 'fixnum))
  (is (subtypep (array-element-type (min (ones 5) 2)) 'fixnum)))

(test (arange :compile-at :run-time :fixture muffle)
  (is (cl:= 5 (length (arange 5))))
  (is (cl:= 5 (length (arange 5 10)))))

(test (asarray :compile-at :run-time :fixture muffle)
  (is (cl:= 1 (array-rank (asarray '((1) (1 2))))))
  (is (cl:= 2 (array-rank (asarray '(((1) (1 2)) ((3) (3 4)))))))
  (is (cl:= 2 (array-rank (asarray '((1 2) (3 4))))))
  (is (cl:= 2 (array-rank (asarray #(#(1 2) #(3 4))))))
  (is (cl:= 2 (array-rank (asarray #((1 2) (3 4))))))
  
  (is (cl:= 2 (array-rank (asarray #(#(1 2) #(3 4))))))
  (is (cl:= 1 (array-rank (asarray #(#(1 2) #(3 4 5))))))
  (is (cl:= 2 (array-rank (asarray #("aa" "aa")))))
  (is (cl:= 1 (array-rank (asarray #("aa" "aaa")))))
  
  (is (cl:= 1 (array-rank (asarray #(#(1 2) #(3 4))   :type 'vector))))
  (is (cl:= 1 (array-rank (asarray #(#(1 2) #(3 4 5)) :type 'vector))))
  (is (cl:= 1 (array-rank (asarray #("aa" "aa")  :type 'string))))
  (is (cl:= 1 (array-rank (asarray #("aa" "aaa") :type 'string))))
  (signals error
    (asarray '((1 2) (3 4))   :type '(array fixnum (* *))))

  (finishes
    (print (asarray '(1)))))

(test (mixed :compile-at :run-time :fixture muffle)
  (finishes
   ;; should return #2A((1 2) (1 2))
   (aref (asarray '((1 2 "text") (1 2 "text"))) t '(0 2))))

(test (reduce :compile-at :run-time :fixture muffle)
  (is (equalp 10 (reduce-array 'cl:+ (arange 5))))
  (is (cl:= 10 (sum (arange 5))))
  (finishes
    (sum (reshape (arange 125) '(5 5 5)))                ; sum all elements
    (sum (reshape (arange 125) '(5 5 5)) :axes '())      ; does nothing
    (sum (reshape (arange 125) '(5 5 5)) :axes '(0))
    (sum (reshape (arange 125) '(5 5 5)) :axes '(0 1))
    (sum (reshape (arange 125) '(5 5 5)) :axes '(0 1 2)) ; sum all elements, again

    (prod (reshape (1+ (arange 16)) '(4 4)))
    (prod (reshape (1+ (arange 16)) '(4 4)) :axes '(0))

    (amax (reshape (arange 16) '(4 4)))
    (amax (reshape (arange 16) '(4 4)) :axes '(0))
    (amax (reshape (arange 16) '(4 4)) :axes '(1))

    (amin (reshape (arange 16) '(4 4)))
    (amin (reshape (arange 16) '(4 4)) :axes '(0))
    (amin (reshape (arange 16) '(4 4)) :axes '(1)))

  ;; giving a local function
  (is (equalp 10
              (flet ((a (x y) (cl:+ x y)))
                (reduce-array #'a (arange 5))))))

(test (equality :compile-at :run-time :fixture muffle)
  ;; it should return NIL (from cl:=), not 0
  (is (eq t (= 10 10)))
  (is (eq nil (= 10 11))))

(test (histogram :compile-at :run-time :fixture muffle)
  (finishes (histogram (arange 5)))
  (finishes (histogram (uniform 5.0 10.0 10000))))

(test (random :compile-at :run-time :fixture muffle)
  (is (integerp (bernoulli 0.3)))
  (is-type (bernoulli 0.3 5) 'bit-vector)
  (is-type (bernoulli 0.3 '(5 5)) '(array bit))
  (is-type (bernoulli-like (zeros '(5 5))) '(array bit (5 5)))
  
  (is (integerp (uniform 0 5)))
  (is (floatp (uniform 0 5.0)))
  (is-type (uniform 0 5 5) `(VECTOR ,(upgraded-array-element-type '(integer 0 5)) 5)))

(test (einsum :compile-at :run-time :fixture muffle)

  (signals type-error
    (numcl.impl::einsum-normalize-subscripts '(i00)))
      
  (is (equalp (ones 5)
              (einsum '(i) (ones 5))))
  (is (equalp (ones 5)
              (einsum '(i -> i) (ones 5))))
  (is (equalp 5
              (einsum '(i ->) (ones 5 :type 'fixnum))))
  
  (is (equalp 25 (einsum '(ij ->) (ones '(5 5) :type 'fixnum))))
  (is (equalp 5  (einsum '(ii ->) (ones '(5 5) :type 'fixnum ))))
  (is (equalp (ones 5)
              (einsum '(ii -> i) (ones '(5 5) :type 'fixnum ))))
  
  (is (equalp (ones '(5 5) :type 'fixnum)
              (einsum '(ij) (ones '(5 5) :type 'fixnum))))
  (is (equalp (full 5 5)
              (einsum '(ij -> i) (ones '(5 5) :type 'fixnum))))

  (is (equalp #2A((7 8) (31 36))
              (einsum '(ij jk -> ik)
                      (asarray #2A((0 1)
                                   (2 3)) :type 'fixnum)
                      (asarray #2A((5 6)
                                   (7 8)) :type 'fixnum))))

  (is (equalp #2A((7 8) (31 36))
              (einsum '((i k) (k j) -> (i j)) ; alternative notation
                      (asarray #2A((0 1)
                                   (2 3)) :type 'fixnum)
                      (asarray #2A((5 6)
                                   (7 8)) :type 'fixnum))))
  
  (is (equalp #2A((0 2) (1 3))
              (einsum '(ij -> ji) (asarray #2A((0 1)
                                               (2 3)) :type 'fixnum))))
  (is (equalp #2A((0 2) (1 3))
              (einsum '(ji) (asarray #2A((0 1) (2 3)) :type 'fixnum))))
  (is (equalp #(0 3)
              (einsum '(ii) (asarray #2A((0 1) (2 3)) :type 'fixnum))))
  (is (equalp 6
              (einsum '(ij ->) (asarray #2A((0 1) (2 3)) :type 'fixnum))))

  (let ((result (zeros '(2 2) :type 'single-float)))
    (einsum '(ij -> ji) (asarray #2A((0 1) (2 3)) :type 'fixnum) result)
    (is (equalp #2A((0 2) (1 3)) result))))

(test (einsum-stride :compile-at :run-time :fixture muffle)

  (is (= 10 (einsum '(i -> ) (ones 10))))
  (is (= 10 (einsum '(i -> (cl:+ @1 $1) -> ) (ones 10))))
  (is (= 10 (einsum '(i -> (cl:+ @1 $1) -> -> ((i :start 0 :end 10 :step 1))) (ones 10))))
  (is (= 5  (einsum '(i -> (cl:+ @1 $1) -> -> ((i :start 0 :end 10 :step 2))) (ones 10))))
  (is (= 4  (einsum '(i -> (cl:+ @1 $1) -> -> ((i :start 0 :end 10 :step 3))) (ones 10))))

  (is (= 9  (einsum '(i -> (cl:+ @1 $1) -> -> ((i :start 0 :end -1))) (ones 10))))
  
  (is (= 3  (einsum '(i -> (cl:+ @1 $1) -> -> ((i :start 0 :end 3  :step 1))) (ones 10))))
  (is (= 5  (einsum '(i -> (cl:+ @1 $1) -> -> ((i :start 0 :end 5  :step 1))) (ones 10))))
  (is (= 7  (einsum '(i -> (cl:+ @1 $1) -> -> ((i :start 0 :end 7  :step 1))) (ones 10))))

  (is (= 7  (einsum '(i -> (cl:+ @1 $1) -> -> ((i :start 3 :end 10 :step 1))) (ones 10))))
  (is (= 5  (einsum '(i -> (cl:+ @1 $1) -> -> ((i :start 5 :end 10 :step 1))) (ones 10))))
  (is (= 3  (einsum '(i -> (cl:+ @1 $1) -> -> ((i :start 7 :end 10 :step 1))) (ones 10))))

  (is (= 3  (einsum '(i -> (cl:+ @1 $1) -> -> ((i :start 0 :end 5  :step 2))) (ones 10))))
  (is (= 2  (einsum '(i -> (cl:+ @1 $1) -> -> ((i :start 0 :end 5  :step 3))) (ones 10))))
   
  (is (= 3  (einsum '(i -> (cl:+ @1 $1) -> -> ((i :start 5 :end 10 :step 2))) (ones 10))))
  (is (= 2  (einsum '(i -> (cl:+ @1 $1) -> -> ((i :start 5 :end 10 :step 3))) (ones 10))))
  
  (is (= 2  (einsum '(i -> (cl:+ @1 $1) -> -> ((i :start 5 :end 9  :step 2))) (ones 10)))))

(test (einsum-broadcast :compile-at :run-time :fixture muffle)

  (is (equalp #3A(((0 1) (0 3)) ((0 3) (4 9)))
              (einsum '(-i -i -> -i)
                      ;; #2A((0 1) (2 3))
                      (reshape (arange 4)   '(2 2))
                      ;; #3A(((0 1)) ((2 3)))
                      (reshape (arange 4) '(2 1 2)))))

  (is (equalp #2A((0 1 2 3 4)
                  (1 2 3 4 5)
                  (2 3 4 5 6)
                  (3 4 5 6 7)
                  (4 5 6 7 8))
              (einsum '(- - -> (cl:+ $1 $2) -> -)
                      ;; #2A((0 1) (2 3))
                      (reshape (arange 5)   '(5))
                      ;; #3A(((0 1)) ((2 3)))
                      (reshape (arange 5) '(5 1)))))
  
  (is (equalp #2A((0  0  0  0  0 )
                  (0  1  2  3  4 )
                  (0  2  4  6  8 )
                  (0  3  6  9  12)
                  (0  4  8  12 16))
              (einsum '(- - -> (cl:* $1 $2) -> -)
                      ;; #2A((0 1) (2 3))
                      (reshape (arange 5)   '(5))
                      ;; #3A(((0 1)) ((2 3)))
                      (reshape (arange 5) '(5 1))))))
 
(test (linarg :compile-at :run-time :fixture muffle)

  (is (equalp 
       #2A((1 1 0 0)
           (1 1 0 0)
           (0 0 1 1)
           (0 0 1 1))
       (kron (eye 2) (ones '(2 2))))))

(test (copy :compile-at :run-time :fixture muffle)
  (let ((a (reshape (arange 25) '(5 5))))
    (is (equalp (copy a) a))))


(test (concatenate :compile-at :run-time :fixture muffle)
  (is (equalp (concatenate nil) nil))
  (is (equalp (stack nil) nil))
  (let ((a (arange 5))
        (b (arange 5)))
    (is (equalp (concatenate (list a b))
                #(0 1 2 3 4 0 1 2 3 4))))
  (let ((a (reshape (arange 4) '(2 2)))
        (b (reshape (arange 4) '(2 2))))
    (is (equalp (concatenate (list a b))
                #2A((0 1) (2 3) (0 1) (2 3))))
    (is (equalp (concatenate (list a b) :axis 0)
                #2A((0 1) (2 3) (0 1) (2 3))))
    (is (equalp (concatenate (list a b) :axis 1)
                #2A((0 1 0 1) (2 3 2 3))))
    (is (equalp (concatenate (list a b) :axis -2)
                #2A((0 1) (2 3) (0 1) (2 3))))
    (is (equalp (concatenate (list a b) :axis -1)
                #2A((0 1 0 1) (2 3 2 3)))))

  ;; type conversion
  (let ((a (arange 5 :type 'single-float))
        (b (arange 5)))
    (is (equalp (concatenate (list a b))
                #(0 1 2 3 4 0 1 2 3 4))))

  (let ((a (arange 5))
        (b (arange 5)))
    (is (equalp (stack (list a b))
                #2A((0 1 2 3 4) (0 1 2 3 4)))))
  (let ((a (reshape (arange 4) '(2 2)))
        (b (reshape (arange 4) '(2 2))))
    (is (equalp (stack (list a b))
                #3A(((0 1) (2 3)) ((0 1) (2 3)))))
    (is (equalp (stack (list a b) :axis 0)
                #3A(((0 1) (2 3)) ((0 1) (2 3)))))
    (is (equalp (stack (list a b) :axis 1)
                #3A(((0 1) (0 1)) ((2 3) (2 3)))))
    (is (equalp (stack (list a b) :axis 2)
                #3A(((0 0) (1 1)) ((2 2) (3 3)))))

    (is (equalp (stack (list a b) :axis -3)
                #3A(((0 1) (2 3)) ((0 1) (2 3)))))
    (is (equalp (stack (list a b) :axis -2)
                #3A(((0 1) (0 1)) ((2 3) (2 3)))))
    (is (equalp (stack (list a b) :axis -1)
                #3A(((0 0) (1 1)) ((2 2) (3 3))))))

  (let ((a (zeros '(3 5))))
    (is (equalp (stack (unstack a))
                a))))

(test (issue-18 :compile-at :run-time :fixture muffle)
  (let ((a (- (uniform 0 10 10))))
    (is (subtypep '(integer -10 0)
                  (array-element-type a))))
  (let ((a (/ (uniform 1 10 10))))
    (is (subtypep numcl.impl::*numcl-default-float-format*
                  (array-element-type a)))))

(test (issue-19 :compile-at :run-time :fixture muffle)
  (let ((a (bernoulli 0.2 10)))
    (is (typep a 'bit-vector))
    (is (typep (- 1 a) 'bit-vector))))

(test (cos-inferer :fixture muffle)
  (is (equal '(SINGLE-FLOAT 1.0 1.0)
             (numcl.impl::interpret-type `(cl:cos (single-float 0 0)))))
  (is (equal '(SINGLE-FLOAT -1.0 1.0)
             (numcl.impl::interpret-type `(cl:cos (single-float 0 ,pi)))))
  (is (equal '(SINGLE-FLOAT -1.0 1.0)
             (numcl.impl::interpret-type `(cl:cos (single-float 0 4)))))
  (is (equal '(SINGLE-FLOAT -1.0 1.0)
             (numcl.impl::interpret-type `(cl:cos (single-float 0 6)))))
  
  (is (subtypep (numcl.impl::interpret-type `(cl:cos (single-float 1.58 ,pi))) ; slightly past pi/2
                '(SINGLE-FLOAT -1.0 0.0)))

  (is (subtypep (numcl.impl::interpret-type `(cl:cos (single-float 4.72 6.27))) ; slightly past 3/2 * pi, before 2 * pi
                '(SINGLE-FLOAT 0.0 1.0)))

  (is (subtypep (numcl.impl::interpret-type `(cl:cos (single-float 4.72 7)))
                '(SINGLE-FLOAT 0.0 1.0)))

  (signals error
    (numcl.impl::interpret-type `(cl:cos (single-float 1.0 0.0))))
  
  (iter (for i from -20.0 to 20.0)
        (iter (for j from i)
              (for pj previous j)
              (repeat 40)
              (for res = (numcl.impl::interpret-type `(cl:cos (single-float ,i ,j))))
              (for pres previous res)
              (unless (first-iteration-p)
                (is (subtypep pres res)
                    "~%type inference of ~a : ~a~%type inference of ~a : ~a --- should be a supertype"
                    `(cl:cos (single-float ,i ,pj)) pres
                    `(cl:cos (single-float ,i ,j)) res)))))
