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
                      (element-type t element-type-given)
                      displaced-to
                      (displaced-index-offset 0)
                      (initial-element nil initial-element-given))
  "The base function for creating a new array satisfying NUMCL-ARRAY-P. (See also: Type NUMCL-ARRAY.)

This function instantiates a fresh 1D array and returns another array
displaced to it with the specified shape. The 1D array is returned as the
secondary value.

The justification for this scheme is that some implementations (esp. SBCL)
require an indirection for accessing the array element (e.g. through
array-header in SBCL) even for a simple multi-dimentional array and thus using a
displacing array has essentially no performance penalty over using a simple
multi-dimentional array.

We also ensure that the length of the base arrays are the multiples of 8.
This ensures that the program can safely iterate over that extended region
with a future support for SIMD operations in mind.

When DISPLACED-TO is given, it should be a numcl array.

When DISPLACED-INDEX-OFFSET is also given, the actual offset is
the specified offset plus the original offset in DISPLACED-TO.

When DISPLACED-TO and ELEMENT-TYPE are given, the upgraded-array-element-type of ELEMENT-TYPE
should be TYPE= to the actual array element type of DISPLACED-TO.

When DISPLACED-TO is given and ELEMENT-TYPE is not given,
ELEMENT-TYPE is simply the actual array element type of DISPLACED-TO.
"
  (declare (type (or list (integer 0 #.array-total-size-limit)) shape))
  (declare (type (or null numcl-array) displaced-to))
  (declare (type (integer 0 #.array-total-size-limit) displaced-index-offset))
  (if displaced-to
      (locally
          (declare (type numcl-array displaced-to))
        (when element-type-given
          (assert (and (csubtypep (cupgraded-array-element-type element-type)
                                  (array-element-type displaced-to))
                       (csubtypep (array-element-type displaced-to)
                                  (cupgraded-array-element-type element-type)))))
        (multiple-value-bind (base-array offset) (array-displacement displaced-to)
          (declare (type base-array base-array))
          ;; constant foldable by inlining
          (values (make-array shape
                              :element-type (array-element-type displaced-to)
                              :displaced-to base-array
                              :displaced-index-offset (+ offset displaced-index-offset))
                  base-array)))
      (let ((total-size (* 8 (ceiling (if (listp shape)
                                          (reduce #'* shape)
                                          shape)
                                      8))))
        (let ((base-array
               (if initial-element-given
                   (make-array total-size
                               :element-type element-type
                               :initial-element initial-element)
                   (make-array total-size
                               :element-type element-type))))
          (declare (type base-array base-array))
          (values (make-array shape
                              :element-type element-type
                              :displaced-to base-array)
                  base-array)))))

