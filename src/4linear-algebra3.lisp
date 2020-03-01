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

(in-package :numcl.impl)

;;; multi-arg GEMM

(define-compiler-macro matmul* (&whole form &rest args)
  (if (= 2 (length args))
      `(matmul ,@args)                   ;avoid runtime overhead
      form))

;;; naive version

(defun matmul*-naive (&rest args)
  "Takes an arbitrary number of matrices and performs multiplication."
  (reduce (lambda (acc m)
            (matmul acc m))
          (cdr args)
          :initial-value (car args)))

;;; implementation 1 : by thrashdin

(defun matmul*-thrashdin (&rest args)
  "Takes an arbitrary number of matrices and performs multiplication,
optimizing the order using the shape information and the associativity
to minimize the size of the intermediate matrix."
  (let* ((dimension-vector (map 'vector #'array-dimensions args))
         (tree (car (%build-tree dimension-vector))))
    (labels ((process-tree (tree)
               (cond ((atom tree) (nth tree args))
                     ((consp tree)
                      (assert (= 2 (length tree)) nil "Tree must be binary")
                      (matmul (process-tree (first tree))
                              (process-tree (second tree)))))))
      (process-tree tree))))

(defun %build-tree (dimensions)
  "Builds an order tree of multiplications, dynamically solving it for all ranges."
  (let* ((n (length dimensions))
         (table (make-array (list n n))))
    (loop :for i :from 0 :below n
          :do (setf (aref table i i) (list i (aref dimensions i) 0)))
    (loop :for i :from 0 :below (1- n)
          :do (setf (aref table i (1+ i))
                    (cons (list i (1+ i)) (%check-two-matrices (second (aref table i i))
                                                               (second (aref table (1+ i) (1+ i)))))))
    (loop :for len :from 2 :below n
          :do (loop :for ind :from 0 :below (- n len)
                    :do (setf (aref table ind (+ ind len))
                              (loop :for step :from 1 :upto len
                                    :with best := (cons (list (first (aref table ind ind))
                                                              (first (aref table (1+ ind) (+ ind len))))
                                                        (%add-two-cases
                                                            (cdr (aref table ind ind))
                                                            (cdr (aref table (1+ ind) (+ ind len)))))
                                    :for next := (cons (list (first (aref table ind (+ ind (1- step))))
                                                             (first (aref table (+ ind step) (+ ind len))))
                                                       (%add-two-cases
                                                        (cdr (aref table ind (+ ind (1- step))))
                                                        (cdr (aref table (+ ind step) (+ ind len)))))
                                    :when (< (third next) (third best))
                                      :do (setf best next)
                                    :finally (return best)))))
    (aref table 0 (1- n))))

(defun %check-two-matrices (first second)
  #.(format
     nil "~@{~A~%~}"
     "first and second are cons of a form (height width) in this case."
     "The function returns the dimensions of resulting thing and the number of"
     "multiplications requred. I work under assertion, that those matrices are"
     "actually multipliable => (= (cdr first) (car second)).")
  (destructuring-bind (first-height first-width) first
    (destructuring-bind (sec-height sec-width) second
      (list (list first-height sec-width) (* first-width sec-height sec-width)))))

(defun %add-two-cases (first second)
  "Compares two cases of what previous thing returns and returns the best one."
  (destructuring-bind (first-matrix first-mult) first
    (destructuring-bind (sec-matrix sec-mult) second
      (let ((res (%check-two-matrices first-matrix sec-matrix)))
        (incf (second res) first-mult)
        (incf (second res) sec-mult)
        res))))

(defun %index->matrix (operation indices matrices)
  "Tree of indexes -> tree of multiplications"
  (if (atom indices)
      (elt matrices indices)
      (list operation (%index->matrix operation (car indices) matrices)
                      (%index->matrix operation (cadr indices) matrices))))

(defun %straight-forward-check (sizes)
  "Naive atomic multiplications check for comparison"
  (loop :for i :from 1 :below (length sizes)
        :with fh := (car (aref sizes 0))
        :sum (* fh (car (aref sizes i)) (cadr (aref sizes i)))))

;;; implementation 2 : O(n^2) naive DP https://en.wikipedia.org/wiki/Matrix_chain_multiplication#A_dynamic_programming_algorithm

#|

This is a copy from wikipedia. This code uses 1-based index, and I also believe
there is an indexing bug.

// Matrix A[i] has dimension dims[i-1] x dims[i] for i = 1..n
MatrixChainOrder(int dims[])
{
    // length[dims] = n + 1
    n = dims.length - 1;
    // m[i,j] = Minimum number of scalar multiplications (i.e., cost)
    // needed to compute the matrix A[i]A[i+1]...A[j] = A[i..j]
    // The cost is zero when multiplying one matrix
    for (i = 1; i <= n; i++)
        m[i, i] = 0;

    for (len = 2; len <= n; len++) { // Subsequence lengths
        for (i = 1; i <= n - len + 1; i++) {
            j = i + len - 1;
            m[i, j] = MAXINT;
            for (k = i; k <= j - 1; k++) {
                cost = m[i, k] + m[k+1, j] + dims[i-1]*dims[k]*dims[j];
                if (cost < m[i, j]) {
                    m[i, j] = cost;
                    s[i, j] = k; // Index of the subsequence split that achieved minimal cost
                }
            }
        }
    }
}

|#

(defun matmul* (&rest args)
  "Takes an arbitrary number of matrices and performs multiplication,
optimizing the order using the shape information and the associativity
to minimize the size of the intermediate matrix."

  (let* ((n (length args))
         (dims (make-array (1+ n) :element-type 'single-float)))
    (setf (aref dims 0)
          (float (array-dimension (first args) 0) 0.0))
    (iter (for arg in args)
          (for i from 1)
          (setf (aref dims i)
                (float (array-dimension arg 1) 0.0)))

    (let ((order (matrix-chain-order dims n))
          (args (coerce args 'vector))
          (*print-array* t))
      (declare (type (simple-array fixnum 2) order))
      ;; (print order)
      (labels ((rec (i j)
                 (if (= i j)
                     (aref args i)
                     (let ((k (aref order i j)))
                       (matmul (rec i k)
                               (rec (1+ k) j))))))
        (rec 0 (1- n))))))

(defun matrix-chain-order (dims n)
  (declare (type (simple-array single-float 1) dims)
           (type (unsigned-byte 31) n)
           (optimize (speed 3) (safety 0))
           )
  (let ((cost  (make-array (list n n) :element-type 'single-float :initial-element 0.0))
        (index (make-array (list n n) :element-type 'fixnum :initial-element 0)))
    (iter (declare (iterate:declare-variables))
          (declare (fixnum len))
          (for len from 2 to n)            ; e.g. n=10, len=4 : number of matrices involved
          (iter (declare (iterate:declare-variables))
                (declare (fixnum i j))
                (for i to (- n len))       ; 0 <= i <= 6 --- within range
                (for j = (+ i len -1))     ; 3 <= j <= 9 --- within range (j=10 is invalid)
                ;; computing the cost of multiplying [i..k] and [k..j]
                (setf (aref cost i j) most-positive-single-float)
                (iter (declare (iterate:declare-variables))
                      (declare (fixnum k))
                      (declare (single-float c))
                      (for k from i below j)
                      (for c = (+ (aref cost i k)       ; cost of [i..k]
                                  (aref cost (1+ k) j)  ; cost of [k+1..j]
                                  ;; e.g. i=2, k=3, j=5
                                  ;; 
                                  ;; dims = [_, _, 2, 5, 3, 7, 4, _, _, _, _]
                                  ;; dimensions: [2x5, 5x3], [3x7, 7x4]
                                  ;; [i  ..k+1] = [2..3] = [2,5,3] --- result : 2x3
                                  ;; [k+1..j+1] = [4..5] = [3,7,4] --- result : 3x4
                                  ;; 
                                  ;; computation -- (3 mul + 2 add) x 4 x 2
                                  (* (aref dims i)          ; first  dim of i
                                     (aref dims (1+ k))     ; second dim of k
                                     (aref dims (1+ j))))) ; second dim of j
                      (when (< c (aref cost i j))
                        (setf (aref cost  i j) c)
                        (setf (aref index i j) k)))))
    index))


;;; performance testing

;; #+(or)
(defun test ()
  (let ((*print-array* nil))
    (time (print
           ;; naive approach will generate a 1000x1000 intermediate matrix
           (matmul* (zeros '(1000 1) :type 'fixnum)
                    (zeros '(1 1000) :type 'fixnum)
                    (zeros '(1000 1000) :type 'fixnum)))))
  nil)

;; #+(or)
(defun test-naive ()
  (let ((*print-array* nil))
    (time (print
           ;; naive approach will generate a 1000x1000 intermediate matrix
           (matmul*-naive (zeros '(1000 1) :type 'fixnum)
                          (zeros '(1 1000) :type 'fixnum)
                          (zeros '(1000 1000) :type 'fixnum)))))
  nil)

;; Results

#|

Environment: sbcl 1.5.9, RYZEN 1700

* Naive

IMPL> (test-naive)

#<(ARRAY FIXNUM (1000 1000)) {1004500D1F}> 
Evaluation took:
  4.646 seconds of real time
  4.643399 seconds of total run time (4.635514 user, 0.007885 system)
  99.94% CPU
  6 forms interpreted
  410 lambdas converted
  13,910,318,070 processor cycles
  30,318,096 bytes consed
  
NIL



* proposed

IMPL> (test)

#<(ARRAY FIXNUM (1000 1000)) {1003C3046F}> 
Evaluation took:
  0.032 seconds of real time
  0.031556 seconds of total run time (0.031556 user, 0.000000 system)
  100.00% CPU
  6 forms interpreted
  410 lambdas converted
  95,227,980 processor cycles
  22,308,112 bytes consed
  
NIL

|#
