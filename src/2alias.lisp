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
  (etypecase array
    (number nil)
    (array (array-dimensions array))))

(defun size (array)
  (array-total-size array))

(defun rank (array)
  (array-rank array))

(defun dtype (array)
  (array-element-type array))

(defun numcl:length (array)
  (if (arrayp array)
      (first (shape array))
      (length array)))

(defun to-simple-array (array)
  "Returns a simple array of the equivalent contents."
  (alexandria:copy-array array :fill-pointer nil :adjustable nil))

(defun reshape (a shape)
  "Reshape the array while sharing the backing 1D array.
-1 implies that the axis size is deduced from the other axes.
T  implies that the axis size is preserved.
   Since the rank may change, T is allowed only when it is the leftmost/rightmost axis,
   or is adjacent to another valid T.

Example of reshaping (3 8 5):

 valid:   (6 -1 10)   = (6 2 10)
 valid:   (t 2 2 2 t) = (3 2 2 2 5)
 valid:   (2 -1 2 2 t) --- (2 3 2 2 5)
 invalid: (2 t 2 2 t)
"
  (assert (<= (count -1 shape) 1))
  (let ((shape (coerce shape 'vector)))
    (iter (for axis in (shape a))
          (for i from 0)
          (while (eq t (aref shape i)))
          (setf (aref shape i) axis))
    (iter (for axis in (reverse (shape a)))
          (for i downfrom (1- (length shape)))
          (while (eq t (aref shape i)))
          (setf (aref shape i) axis))
    (if-let ((pos (position -1 shape)))
      (let ((missing (/ (size a)
                        (reduce #'* (remove -1 shape)))))
        (assert (integerp missing))
        (let ((shape (substitute missing -1 shape)))
          
          (%make-array (coerce shape 'list)
                       :element-type (array-element-type a)
                       :displaced-to a)))
      (progn
        (assert (= (reduce #'* shape) (size a)))
        (%make-array (coerce shape 'list)
                     :element-type (array-element-type a)
                     :displaced-to a)))))

(defun numcl:flatten (a)
  (%make-array (array-total-size a) 
               :element-type (array-element-type a)
               :displaced-to a))

(defun squeeze (a)
  (%make-array (remove 1 (shape a)) 
               :element-type (array-element-type a)
               :displaced-to a))

(defun expand-dims (a axis)
  (let ((dims (shape a)))
    (%make-array (append (subseq dims 0 axis) '(1) (subseq dims axis))
                 :element-type (array-element-type a)
                 :displaced-to a)))

;; transpose

