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


(declaim (inline shape size rank dtype numcl:length to-simple-array reshape numcl:flatten squeeze expand-dims transpose))

(defun shape (array)
  (declare (type (or number array sequence) array))
  (etypecase array
    (number nil)
    (array (array-dimensions array))
    (sequence (list (length array)))))

(defun size (array)
  (declare (type (or number array sequence) array))
  (etypecase array
    (number 1)
    (array (array-total-size array))
    (sequence (length array))))

(defun rank (array)
  (declare (type (or number array sequence) array))
  (etypecase array
    (number 0)
    (array (array-rank array))
    (sequence 1)))

(defun dtype (array)
  (declare (type (or number array sequence) array))
  (etypecase array
    (number (type-of array))
    (array  (array-element-type array))
    (sequence t)))

(defun numcl:length (array)
  (declare (type (or array sequence) array))
  (etypecase array
    ;; (number (type-of array))
    (array (array-dimension array 0))
    (sequence (length array))))

(defun to-simple-array (array)
  "Returns a simple array of the equivalent contents."
  (declare (type array array))
  (alexandria:copy-array array :fill-pointer nil :adjustable nil))

(defun reshape (a shape)
  "Reshape the array while sharing the backing 1D array.
-1 implies that the axis size is deduced from the other axes. At most one axis is allowed to be -1.
T  implies that the axis size is preserved. It can be used as many times, but only at the right/leftmost axes.

Example of reshaping (3 8 5):

valid:

    (6 -1 10)     = (6 2 10)
    (t 2 2 2 t)   = (3 2 2 2 5)
    (3 t t)       = (3 8 5)
    (2 -1 2 2 t)  = (2 3 2 2 5)

invalid:

    (2 t 2 2 t)
"
  (declare (numcl-array a))
  (assert (<= (count -1 shape) 1) (shape) "-1 should not be used more than once. Target shape ~a" shape)
  (let ((tshape (coerce shape 'vector)) ; target shape
        (t-usage 0))
    (iter (for i from 0 below (min (length tshape) (rank a)))
          (while (eq t (aref tshape i)))
          (incf t-usage)
          (assert (< t-usage (rank a)) (shape) "Axis specifier T was used too many times.~% Target shape ~a, Input dimension: ~a" shape (shape a))
          (setf (aref tshape i) (array-dimension a i)))
    (iter (for i from (1- (length tshape)) downto 0)
          (for j from (1- (rank a))        downto 0)
          (while (eq t (aref tshape i)))
          (incf t-usage)
          (assert (< t-usage (rank a)) (shape) "Axis specifier T was used too many times.~% Target shape ~a, Input dimension: ~a" shape (shape a))
          (setf (aref tshape i) (array-dimension a j)))
    (iter (for i from 0 below (length tshape))
          (when (= -1 (aref tshape i))
            (multiple-value-bind (missing remainder) (floor (size a) (abs (reduce #'* tshape)))
              (assert (plusp missing)   (shape) "The number of elements in the target shape is too large.~% Target shape ~a, Input dimension: ~a" shape (shape a))
              (assert (zerop remainder) (shape) "The target shape is not compatible for reshaping the input.~% The product of the positive-valued axes in the target shape does not divide the total size.~% Target shape ~a, Input dimension: ~a" shape (shape a))
              (setf (aref tshape i) missing)
              (finish))))
    (%make-array (coerce tshape 'list) :displaced-to a)))

(defun numcl:flatten (a)
  (etypecase a
    (null
     a)
    (array
     (%make-array (array-total-size a) 
                  :element-type (array-element-type a)
                  :displaced-to a))
    (cons
     (the list (alexandria:flatten a)))))

(defun squeeze (a)
  (%make-array (remove 1 (shape a)) 
               :element-type (array-element-type a)
               :displaced-to a))

(defun expand-dims (a axes)
  "axes: an int or a list of ints"
  (let ((dims (shape a))
        (axes (ensure-list axes)))
    (%make-array (iter
                   outer
                   (for j from 0 to (rank a))
                   (iter (while (and (first axes) (<= (first axes) j)))
                     (in outer
                         (collect 1))
                     (pop axes))
                   (when (first dims)
                     (collect (first dims))
                     (pop dims)))
                   
                 :element-type (array-element-type a)
                 :displaced-to a)))

;; transpose

