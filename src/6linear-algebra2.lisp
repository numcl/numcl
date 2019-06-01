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

;; dot(a, b[, out]) 	Dot product of two arrays.
;; linalg.multi_dot(arrays) 	Compute the dot product of two or more arrays in a single function call, while automatically selecting the fastest evaluation order.
;; vdot(a, b) 	Return the dot product of two vectors.
;; inner(a, b) 	Inner product of two arrays.
;; outer(a, b[, out]) 	Compute the outer product of two vectors.
;; matmul(a, b[, out]) 	Matrix product of two arrays.
;; tensordot(a, b[, axes]) 	Compute tensor dot product along specified axes for arrays >= 1-D.
;; einsum(subscripts, *operands[, out, dtype, …]) 	Evaluates the Einstein summation convention on the operands.
;; einsum_path(subscripts, *operands[, optimize]) 	Evaluates the lowest cost contraction order for an einsum expression by considering the creation of intermediate arrays.
;; linalg.matrix_power(a, n) 	Raise a square matrix to the (integer) power n.
;; kron(a, b) 	Kronecker product of two arrays.

;; misc functions. These are tentative, base implementations.
;; they will eventually be replaced by BLAS/LAPACK routines.

(declaim (inline transpose))
(defun transpose (matrix)
  "Reverses the axes of an array."
  (let ((indices (make-gensym-list (rank matrix))))
    ;; needs some caching for functions here
    (einsum `(,indices -> ,(reverse indices)) matrix)))

(declaim (inline matmul))
(defun matmul (a b)
  "Matrix product of two arrays."
  (einsum '(ij jk -> ik) a b))

(declaim (inline vdot))
(defun vdot (a b)
  "Dot product of two vectors."
  (einsum '(i i -> ) a b))

(declaim (inline outer))
(defun outer (a b)
  "Compute the outer product of two vectors."
  (einsum '(i j -> ij) a b))

(declaim (inline kron))
(defun kron (a b)
  "Compute the kronecker product of two vectors."
  (reshape (einsum '(ij kl -> ikjl) a b)
           (mapcar #'* (shape a) (shape b))))

;; (declaim (inline dot))
;; (defun dot (a b)
;;   "Dot product of two arrays."
;;   (etypecase 
;;   
;;   (einsum '(ij jk -> ik) a b))

