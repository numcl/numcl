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

(declaim (inline empty empty-like
                 full  full-like
                 zeros zeros-like
                 ones  ones-like))

(defun empty (shape &key (type 'bit))
  (%make-array shape
               :element-type type))

(defun full (shape value &key (type (type-of value)))
  (%make-array shape
               :element-type type
               :initial-element (%coerce value type)))

(defun zeros (shape &key (type 'bit))
  (full shape (%coerce 0 type) :type type))

(defun ones (shape &key (type 'bit))
  (full shape (%coerce 1 type) :type type))



(defun empty-like (array &key (type (array-element-type array)))
  (empty (shape array) :type type))

(defun full-like (array value &key (type (array-element-type array)))
  (full (shape array) (%coerce value type) :type type))

(defun zeros-like (array &key (type (array-element-type array)))
  (full-like array (%coerce 0 type) :type type))

(defun ones-like (array &key (type (array-element-type array)))
  (full-like array (%coerce 1 type) :type type))

#+(or)
(defun fn ()
  (ones '(1 2 3)))


;; (defun identity (shape &key (type 'bit))
;;   )

(declaim (inline eye))
(defun eye (n &key (m n) (k 0) (type 'bit))
  (declare (fixnum n m k))
  (let ((a (zeros (list n m) :type type))
        (one (%coerce 1 type))
        (min (min m n)))
    (cond
      ((zerop k)
       ;; (print :a)
       (iter (for i below min)
             (setf (aref a i i) one)))
      ((plusp k)
       (assert (< k min))
       (iter (for i below (min m n k))
             (setf (aref a i i) one)))
      ((minusp k)
       (assert (< (- k) min))
       (iter (for i from (+ min k) below min)
             (setf (aref a i i) one))))
    a))
