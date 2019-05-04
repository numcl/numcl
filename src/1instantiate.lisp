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

(declaim (inline %make-array))

(defun %make-array (shape
                    &key
                      (element-type t)
                      displaced-to
                      (displaced-index-offset 0)
                      initial-element)
  "The base function for the arrays managed by NUMCL.

This function instantiate a new flattened array and returns another array
displaced to it with the specified shape. The flattened array is returned as the
secondary value.

The justification for this scheme is that some implementations (esp. SBCL)
require an indirection for accessing the array element (e.g. through
array-header in SBCL) even for a simple multi-dimentional array and thus using a
displacing array has essentially no performance penalty over using a simple
multi-dimentional array.

We also ensure that the length of the base arrays are the multiples of 8.
This ensures that the program can safely iterate over that extended region
with a future support for SIMD operations in mind.
"
  (check-type shape (or list (integer 0 #.array-total-size-limit)))
  (if displaced-to
      (multiple-value-bind (d o) (array-displacement displaced-to)
        (let ((base-array (or d displaced-to)))
          (check-type base-array (simple-array * 1))
          (values (make-array shape
                              :element-type element-type
                              :displaced-to
                              (if d d displaced-to)
                              :displaced-index-offset
                              (+ o displaced-index-offset))
                  base-array)))
      (let ((total-size (* 8 (ceiling (if (listp shape)
                                          (reduce #'* shape)
                                          shape)
                                      8))))
        (check-type total-size (integer 0 #.array-total-size-limit))
        (let ((base-array
               (cond
                 (initial-element
                  (make-array total-size
                              :element-type element-type
                              :initial-element initial-element))
                 (t
                  (make-array total-size
                              :element-type element-type)))))
          (values (make-array shape
                              :element-type element-type
                              :displaced-to base-array)
                  base-array)))))


(declaim (inline %displace-at %displace-range))

(defun %displace-at (array i)
  "Displace to the i-th element subarray in the first axis"
  (let ((newshape (cdr (shape array))))
    (%make-array newshape
                 :element-type (array-element-type array)
                 :displaced-to array
                 :displaced-index-offset (* i (reduce #'* newshape)))))

(defun %displace-range (array from to)
  "Displace to the subarray between FROM-th and TO-th element subarray in the first axis"
  (let ((newshape (cdr (shape array))))
    (%make-array (list* (- to from) newshape)
                 :element-type (array-element-type array)
                 :displaced-to array
                 :displaced-index-offset (* from (reduce #'* newshape)))))

